library(bcputility)
# DEVELOPERS ONLY
# This is provided as a convenience to set up a testing environment for those
# who wish to modify. This only needs to be run once for setting up the
# required tables and schema in a SQL Server instance. Set the environment
# variables MSSQL_SERVER and MSSQL_DB or modify the `makeConnectArgs` function
# below. It is best to `source` this file from the package home directory.

# Writes the data to SQL Server if TRUE
initDB <- FALSE
# This should never need to be TRUE as the internal R data is already contained
# in the package.
initRData <- FALSE
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  initTestData <- function(connectargs, initDB = initDB,
    initRData = initRData) {
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
    exportTable <- importTable[seq(1L, 10000L, 1L), ]
    sfTable <- sf::st_read(system.file(package = 'sf', 'gpkg/nc.gpkg'),
      quiet = TRUE)
    sfTable <- sfTable[sample.int(nrow(sfTable), 10000, replace = TRUE), ]
    if (isTRUE(initDB)) {
      # create init datasets for testing against
      bcpImport(x = importTable, connectargs = connectArgs,
        table = 'importTableInit', overwrite = TRUE, stdout = FALSE)
      bcpImport(x = exportTable, connectargs = connectArgs,
        table = 'exportTableInit', overwrite = TRUE, stdout = FALSE)
      # write spatial data
      bcpImport(x = sfTable, connectargs = connectArgs,
        table = 'sfTableGeomInit', overwrite = TRUE, stdout = FALSE)
      bcpImport(x = sfTable, connectargs = connectArgs,
        table = 'sfTableGeogInit', overwrite = TRUE, stdout = FALSE,
        spatialtype = 'geography')
      # create "test" schema
      sqlcmdArgs <- append(
        bcputility:::mapConnectArgs(connectargs = connectArgs,
          utility = 'sqlcmd'), values = list('-Q',
          shQuote("IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'test')
      BEGIN
      EXEC('CREATE SCHEMA test;')
      END")))
      system2(command = bcputility:::findUtility('sqlcmd'), args = sqlcmdArgs)
    }
    if (isTRUE(initDB)) {
      # create internal data
      exportTableFile <- tempfile('exportTableInit', fileext = '.csv')
      bcpExport(file = exportTableFile, connectargs = connectArgs,
        table = 'exportTableInit', fieldterminator = ',',
        stdout = FALSE
      )
      .exportTableInit <- data.table::fread(exportTableFile)
      queryTableFile <- tempfile('queryTableInit', fileext = '.csv')
      query <- 'SELECT * FROM [dbo].[exportTableInit] WHERE int < 1000'
      bcpExport(file = queryTableFile, connectargs = connectArgs, query = query,
        fieldterminator = ',', stdout = FALSE
      )
      .queryTableInit <- data.table::fread(queryTableFile)
      save(.exportTableInit, .queryTableInit,
        file = '../bcputility/R/sysdata.rda',
        compress = 'bzip2')
    }
  }
  # set up connnect args
  server <- Sys.getenv("MSSQL_SERVER")
  database <- Sys.getenv('MSSQL_DB')
  connectArgs <- makeConnectArgs(server = server, database = database)
  initTestData(connectargs = connectArgs, initDB = initDB,
    initRData = initRData)
}
