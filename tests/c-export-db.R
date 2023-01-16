library(bcputility)
testLocal <- FALSE
if (identical(Sys.getenv('NOT_CRAN'), 'true') && isTRUE(testLocal)) {
  testExport <- function(connectargs) {
    on.exit(
      expr = file.remove(tmpFiles) |>
        invisible() |>
        suppressWarnings(),
      add = TRUE)
    tmpFiles <- tempfile(c('exportTableChar', 'exportTableNChar',
      'queryTableChar', 'queryTableNChar'), fileext = '.csv')
    # bcpExportChar
    bcpExport(
      tmpFiles[1],
      connectargs = connectArgs,
      table = 'exportTableInit',
      fieldterminator = ',',
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    # bcpExportNchar
    bcpExport(
      tmpFiles[2],
      connectargs = connectArgs,
      table = 'exportTableInit',
      fieldterminator = ',',
      stdout = FALSE,
      bcpOptions = list(
        '-w',
        '-b', 1000,
        '-a', 4096,
        '-m', 0
      )
    ) |>
      identical(0L) |>
      stopifnot()
    data.table::fread(tmpFiles[1]) |>
      identical(data.table::data.table(bcputility:::.exportTableInit)) |>
      stopifnot()
    # test query export
    query <- 'SELECT * FROM [dbo].[exportTableInit] WHERE int < 1000'
    # bcpExportChar
    bcpExport(file = tmpFiles[3],
      connectargs = connectArgs, query = query, fieldterminator = ',',
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    # bcpExportQueryNchar
    bcpExport(
      tmpFiles[4],
      connectargs = connectArgs,
      query = query,
      fieldterminator = ',',
      stdout = FALSE,
      bcpOptions = list(
        '-w',
        '-b', 1000,
        '-a', 4096,
        '-m', 0
      )
    ) |>
      identical(0L) |>
      stopifnot()
    data.table::fread(tmpFiles[3]) |>
      identical(data.table::data.table(bcputility:::.queryTableInit)) |>
      stopifnot()
  }
  # set up connnect args
  server <- Sys.getenv('MSSQL_SERVER')
  database <- Sys.getenv('MSSQL_DB')
  connectArgs <- makeConnectArgs(server = server, database = database)
  testExport(connectargs = connectArgs)
}
