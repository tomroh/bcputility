library(bcputility)
testLocal <- TRUE
if (!identical(Sys.getenv('NOT_CRAN'), 'true') && isTRUE(testLocal)) {
  testExport <- function(connectargs) {
    on.exit(
      expr = file.remove(
        c(
          'inst/benchmarks/queryTable.csv',
          'inst/benchmarks/queryTableNchar.csv',
          'inst/benchmarks/exportTableChar.csv',
          'inst/benchmarks/exportTableNchar.csv')) |>
      invisible() |>
      suppressWarnings(),
      add = TRUE)
    # bcpExportChar
    bcpExport(
      'inst/benchmarks/exportTableChar.csv',
      connectargs = connectArgs,
      table = 'exportTableInit',
      fieldterminator = ',',
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    # bcpExportNchar
    bcpExport(
      'inst/benchmarks/exportTableNchar.csv',
      connectargs = connectArgs,
      table = 'exportTableInit',
      fieldterminator = ',',
      stdout = FALSE,
      bcpOptions = list(
        '-w',
        '-b', 1000,
        '-a', 4096,
        '-e', 10
      )
    ) |>
      identical(0L) |>
      stopifnot()
    data.table::fread('inst/benchmarks/exportTableChar.csv') |>
      identical(data.table::fread('inst/benchmarks/exportTableInit.csv')) |>
      stopifnot()
    # test query export
    query <- 'SELECT * FROM [dbo].[exportTableInit] WHERE int < 1000'
    # bcpExportChar
    bcpExport(file = 'inst/benchmarks/queryTable.csv',
      connectargs = connectArgs, query = query, fieldterminator = ',',
      stdout = FALSE
    ) |>
      identical(0L) |>
      stopifnot()
    # bcpExportQueryNchar
    bcpExport(
      'inst/benchmarks/queryTableNchar.csv',
      connectargs = connectArgs,
      query = query,
      fieldterminator = ',',
      stdout = FALSE,
      bcpOptions = list(
        '-w',
        '-b', 1000,
        '-a', 4096,
        '-e', 10
      )
    ) |>
      identical(0L) |>
      stopifnot()
    data.table::fread('inst/benchmarks/queryTable.csv') |>
      identical(data.table::fread('inst/benchmarks/queryTableInit.csv')) |>
      stopifnot()
  }
  # set up connnect args
  server <- Sys.getenv('MSSQL_SERVER')
  database <- Sys.getenv('MSSQL_DB')
  connectArgs <- makeConnectArgs(server = server, database = database)
  testExport(connectargs = connectArgs)
}
