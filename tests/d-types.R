library(bcputility)
# test map data types
testTypeMapping <- function() {
  mapDataTypes(data.frame(
    int = 1:5L,
    numeric = seq(0, 1, length.out = 5),
    character = LETTERS[1:5],
    factor = paste(LETTERS[1:5], LETTERS[1:5], sep = ''),
    logical = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    date = seq(Sys.Date() - 4, Sys.Date(), 1L),
    datetime = seq(Sys.time() - 5, Sys.time(), length.out = 5)
  )
  ) |>
    identical(
      c(int = "TINYINT", numeric = "FLOAT", character = "VARCHAR(1)",
        factor = "VARCHAR(2)", logical = "BIT", date = "DATE",
        datetime = "DATETIME")) |>
    stopifnot()
  # test varChar
  (varChar(paste(rep('A', 1), collapse = '')) == 'VARCHAR(1)') |>
    stopifnot()
  (varChar(paste(rep('A', 8001), collapse = '')) == 'VARCHAR(MAX)') |>
    stopifnot()
  (varChar(NA_character_) == 'VARCHAR(1)') |>
    stopifnot()
  # test varBinary
  (varBinary(blob::as_blob(raw(length = 1))) == 'VARBINARY(1)') |>
    stopifnot()
  (varBinary(blob::as_blob(raw(length = 8001))) == 'VARBINARY(MAX)') |>
    stopifnot()
  (varBinary(blob::as_blob(NA)) == 'VARBINARY(0)') |>
    stopifnot()
  (int(2) == 'TINYINT') |>
    stopifnot()
  (int(2^8) == 'SMALLINT') |>
    stopifnot()
  (int(2^15) == 'INTEGER') |>
    stopifnot()
  (int(2^31) == 'BIGINT') |>
    stopifnot()
}
testTypeMapping()
print('All data type tests passed')
