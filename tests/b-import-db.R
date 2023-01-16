library(bcputility)
testLocal <- FALSE
if (identical(Sys.getenv('NOT_CRAN'), 'true') && isTRUE(testLocal)) {
  tryDropTable <- function(connectargs, table) {
    if (checkTableExists(connectargs, table)) {
      dropTable(connectargs, table)
    }
  }
  testImport <- function(connectargs) {
    # clean up on exit
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'importTable1000'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'importTable10000'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'sfTableGeom'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'sfTableGeog'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'importTableLogin'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'test.importTable'),
      add = TRUE
    )
    on.exit(
      expr = tryDropTable(connectargs = connectArgs, 'test.sfTable'),
      add = TRUE
    )
    # create sample datasets
    set.seed(11)
    n <- 100000
    importTable <- data.frame(
      int = sample(x = seq(1L, 10000L, 1L), size = n, replace = TRUE),
      numeric = sample(x = seq(0, 1, length.out = n / 100), size = n,
        replace = TRUE),
      character = sample(x = state.abb, size = n, replace = TRUE),
      factor = sample(x = factor(x = month.abb, levels = month.abb),
        size = n, replace = TRUE),
      logical = sample(x = c(TRUE, FALSE), size = n, replace = TRUE),
      date = sample(x = seq(as.Date('2022-01-01'), as.Date('2022-12-31'),
        by = 'days'), size = n, replace = TRUE),
      datetime = sample(x = seq(as.POSIXct('2022-01-01 00:00:00'),
        as.POSIXct('2022-12-31 23:59:59'), by = 'min'), size = n,
        replace = TRUE)
    )
    sfTable <- sf::st_read(system.file(package = 'sf', 'gpkg/nc.gpkg'),
      quiet = TRUE)
    sfTable <- sfTable[sample.int(nrow(sfTable), 10000, replace = TRUE), ]
    # bcpImport 1000
    bcpImport(
      importTable,
      connectargs = connectArgs,
      table = '[importTable1000]',
      overwrite = TRUE,
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs,
      table = 'importTable1000') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'importTableInit')) |>
      stopifnot()
    # bcpImport 10000
    bcpImport(
      importTable,
      connectargs = connectArgs,
      table = '[importTable10000]',
      overwrite = TRUE,
      stdout = FALSE,
      bcpOptions = list(
        '-b', 10000,
        '-a', 4096
      )
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs,
      table = 'importTable10000') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'importTableInit')) |>
      stopifnot()
    # bcpImportGeometry
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = 'sfTableGeom',
      overwrite = TRUE,
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs, table = 'sfTableGeom') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'sfTableGeomInit')) |>
      stopifnot()
    # bcpImportGeography
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = 'sfTableGeog',
      overwrite = TRUE,
      stdout = FALSE,
      spatialtype = 'geography'
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs, table = 'sfTableGeog') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'sfTableGeogInit')) |>
      stopifnot()
    # test sql login
    loginConnectArgs <- makeConnectArgs(server = server, database = database,
      username = Sys.getenv('MSSQL_UID'),  password = Sys.getenv('MSSQL_PWD'))
    bcpImport(
      importTable,
      connectargs = connectArgs,
      table = '[importTableLogin]',
      overwrite = TRUE,
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs,
      table = 'importTableLogin') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'importTableInit')) |>
      stopifnot()
    # bcpImport schema
    bcpImport(
      importTable,
      connectargs = connectArgs,
      table = 'test.[importTable]',
      overwrite = TRUE,
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    bcputility:::readTable(connectargs = connectArgs,
      table = 'test.importTable') |>
      identical(bcputility:::readTable(connectargs = connectArgs,
        table = 'importTableInit')) |>
      stopifnot()
    # test return output
    # fail no capture
    bcpImport(
      importTable[1:10000, rev(seq_len(ncol(importTable)))],
      connectargs = connectArgs,
      table = '[test].importTable',
      overwrite = FALSE,
      stdout = FALSE
    ) |>
      identical(1L) |>
      stopifnot()
    # pass with capture
    bcpImport(
      importTable[1:10000, ],
      connectargs = connectArgs,
      table = '[test].importTable',
      overwrite = TRUE,
      stdout = TRUE,
      stderr = TRUE
    ) |>
      attr('status') |>
      is.null() |>
      stopifnot()
    # fail with capture
    bcpImport(
      importTable[1:10000, rev(seq_len(ncol(importTable)))],
      connectargs = connectArgs,
      table = '[test].importTable',
      overwrite = FALSE,
      stdout = TRUE,
      stderr = TRUE
    ) |>
      attr('status') |>
      identical(1L) |>
      stopifnot()
    # test return output sf
    # pass no capture
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = '[test].sfTable',
      overwrite = TRUE,
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    # fail no capture
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = '[test].sfTable',
      overwrite = FALSE,
      stdout = FALSE
    ) |>
      identical(1L) |>
      stopifnot()
    # pass with capture
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = '[test].sfTable',
      overwrite = TRUE,
      stdout = TRUE,
      stderr = TRUE
    ) |>
      attr('status') |>
      is.null() |>
      stopifnot()
    # fail with capture
    bcpImport(
      sfTable,
      connectargs = connectArgs,
      table = '[test].sfTable',
      overwrite = FALSE,
      stdout = TRUE,
      stderr = TRUE
    ) |>
      attr('status') |>
      identical(1L) |>
      stopifnot()
  }
  # set up connnect args
  server <- Sys.getenv('MSSQL_SERVER')
  database <- Sys.getenv('MSSQL_DB')
  connectArgs <- makeConnectArgs(server = server, database = database)
  testImport(connectargs = connectArgs)
}
