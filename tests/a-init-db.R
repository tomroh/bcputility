library(bcputility)
init <- FALSE
if (!identical(Sys.getenv("NOT_CRAN"), "true") && isTRUE(init)) {
  # set up connnect args
  server <- Sys.getenv("MSSQL_SERVER")
  database <- Sys.getenv('MSSQL_DB')
  connectArgs <- makeConnectArgs(server = server, database = database)
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
    #logical = sample(x = c(TRUE, FALSE), size = n, replace = TRUE),
    date = sample(x = seq(as.Date('2022-01-01'), as.Date('2022-12-31'),
      by = 'days'), size = n, replace = TRUE),
    datetime = sample(x = seq(as.POSIXct('2022-01-01 00:00:00'),
      as.POSIXct('2022-12-31 23:59:59'), by = 'min'), size = n, replace = TRUE)
  )
  exportTable <- importTable[seq(1L, 10000L, 1L), ]
  sfTable <- sf::st_read(system.file(package = 'sf', 'gpkg/nc.gpkg'),
    quiet = TRUE)
  sfTable <- sfTable[sample.int(nrow(sfTable), 10000, replace = TRUE),]
  # create init datasets for testing against
  bcpImport(x = importTable, connectargs = connectArgs,
    table = 'importTableInit', overwrite = TRUE, stdout = FALSE)
  bcpImport(x = exportTable, connectargs = connectArgs,
    table = 'exportTableInit', overwrite = TRUE, stdout = FALSE)
  bcpExport(file = 'inst/benchmarks/exportTableInit.csv',
    connectargs = connectArgs, table = 'exportTableInit', fieldterminator = ',',
    stdout = FALSE
  )
  query <- 'SELECT * FROM [dbo].[exportTableInit] WHERE int < 1000'
  bcpExport(file = 'inst/benchmarks/queryTableInit.csv',
    connectargs = connectArgs, query = query, fieldterminator = ',',
    stdout = FALSE
  )
  bcpImport(x = sfTable, connectargs = connectArgs, table = 'sfTableGeomInit',
    overwrite = TRUE, stdout = FALSE)
  bcpImport(x = sfTable, connectargs = connectArgs, table = 'sfTableGeogInit',
    overwrite = TRUE, stdout = FALSE, spatialtype = 'geography')
  sqlcmdArgs <- append(bcputility:::mapConnectArgs(connectargs = connectArgs,
    utility = 'sqlcmd'), values = list('-Q',
      shQuote("IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'test')
      BEGIN
      EXEC('CREATE SCHEMA test;')
      END")))
  system2(command = bcputility:::findUtility('sqlcmd'), args = sqlcmdArgs)
}
