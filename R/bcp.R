#' Import data to SQL Server
#'
#' A wrapper for a system call to the bcp utility which bulk inserts to SQL
#' Server.
#'
#' @param x
#'
#' dataframe object or path to file
#'
#' @param connectargs
#'
#' named list of connection arguments. See \link[bcputility]{makeConnectArgs}.
#'
#' @param table
#'
#' Name of the source table when importing from  SQL Server. For specifying the
#' schema in the table name see \code{<schema>.<table>} and if not specified the
#' default is "dbo".
#'
#' @param fieldterminator
#'
#' character separator for columns
#'
#' @param rowterminator
#'
#' character separator for rows--new lines
#'
#' @param overwrite
#'
#' Whether to overwrite the table if it exists
#'
#' @param spatialtype
#'
#' spatial data type for schema
#' \url{https://docs.microsoft.com/en-us/sql/relational-databases/spatial/spatial-data-types-overview},
#' ignored if \code{x} is not an 'sf' object
#'
#' @param bcpOptions
#'
#' list of additional options to pass to the 'bcp' utility. See details.
#'
#' @param ...
#'
#' arguments to pass to \link[base]{system2}
#'
#' @details
#'
#' If x is a dataframe object, \code{data.table::fwrite} is used to write the
#' in memory object to disk in a temporary file that is deleted when the
#' function exits. The \code{fieldterminator} and \code{rowterminator} are
#' ignored in this case.
#'
#' If \code{overwrite} is \code{TRUE}, any existing table of the same name
#' will be deleted and the schema is inferred from \code{DBI::dbCreateTable}. To
#' use a customized schema, create the schema before calling the function and
#' use \code{overwrite=FALSE}.
#'
#' If x is a sf object, the geometry column is converted to binary and
#' written to the database before conversion to geometry/geometry data type.
#' The EPSG code is automatically read from the sf object and used as the
#' SRID.
#'
#' To override the default path to the bcp command line utility, set the
#' \code{bcputility.bcp.path} option.
#' To override the default path to the sqlcmd command line utility, set the
#' \code{bcputility.sqlcmd.path} option.
#'
#' The \code{bcpOptions} allows the user to include additional arguments for the
#' call to \code{system2}. Please refer to
#' \url{https://learn.microsoft.com/en-us/sql/tools/bcp-utility}.
#' The default options are set to the defaults for \code{bcp} CLI. \code{-b}
#' refers to number of rows to write at a time; 10,000 to 50,000 is a
#' starting recommendation. \code{-a} refers to size of packets to be sent in
#' bytes. \code{-e} refers to the maximum number of errors before failure.
#'
#' @return
#'
#' Output from \code{system2}. See \code{...} to redirect output.
#'
#' @export
#'
bcpImport <- function(
  x,
  connectargs,
  table,
  fieldterminator = '\t',
  rowterminator = ifelse(.Platform$OS.type == 'windows',
    '\r\n', '\n'),
  overwrite = FALSE,
  spatialtype = c('geometry', 'geography'),
  bcpOptions = list(
    '-b', 1000,
    '-a', 4096,
    '-m', 10
  ),
  ...) {
  on.exit(
    if (exists('tmp', inherits = FALSE)) {
      unlink(tmp)
    },
    add = TRUE)
  if (missing(connectargs)) {
    stop('connectargs is missing. See ?bcputility::makeConnectArgs.')
  }
  bcp <- findUtility('bcp')
  # syntax differs for the two utilities
  bcpArgs <- mapConnectArgs(connectargs = connectargs, utility = 'bcp')
  quotedIdentifiers <- connectargs[['quotedidentifiers']]
  isSpatial <- methods::is(x, 'sf')
  if (methods::is(x, 'data.frame')) {
    tmp <- tempfile(fileext = '.dat')
    fileName <- tmp
    if (isSpatial) {
      spatialtype <- match.arg(spatialtype)
      srid <- sf::st_crs(x)$epsg
      if (is.null(srid) || !is.numeric(srid)) {
        stop('Only EPSGs are supported for SQL Server SRIDs.
          Check with sf::st_crs and change projection with sf::st_transform.')
      }
      geometryCol <- attr(x, 'sf_column')
      binaryCol <- sprintf('%sWkb', geometryCol)
      x <- data.table::data.table(x)
      data.table::set(x = x, j = binaryCol,
        value = sf::st_as_binary(x[[geometryCol]], hex = TRUE))
      data.table::set(x = x, j = geometryCol, value = NULL)
    }
    data.table::fwrite(x,
                       fileName,
                       sep = fieldterminator,
                       eol = rowterminator,
                       logical01 = TRUE,
                       col.names = FALSE,
                       dateTimeAs = 'write.csv')
  } else {
    stopifnot(file.exists(x))
    fileName <- x
    # check data types
    # this isn't currently working
    x <- data.table::fread(fileName, nrows = 100000)
  }
  # must use -c character storage for file type
  bcpArgs <- append(bcpArgs, list('-t', shQuote(fieldterminator),
                                  '-r', shQuote(rowterminator),
                                  '-c'))
  bcpArgs <- append(bcpArgs, list(
    ifelse(quotedIdentifiers, shQuote(table), quoteTable(table = table)),
    'in', shQuote(fileName)), after = 0)
  tableExists <-  checkTableExists(connectargs = connectargs, table = table)
  append <- tableExists && isFALSE(overwrite)
  if (isFALSE(append)) {
    # guess sql server data types
    if (isSpatial) {
      dbTypes <- mapDataTypes(x = x[, !names(x) %in% binaryCol])
      dbTypes[[binaryCol]] <- 'varbinary(max)'
    } else {
      dbTypes <- mapDataTypes(x = x)
    }
    # delete table if overwrite is true
    if (isTRUE(overwrite) && isTRUE(tableExists)) {
      dropOutput <- dropTable(connectargs = connectargs, table = table,
        stderr = TRUE)
      if (length(dropOutput) != 0) {
        stop(paste(dropOutput, collapse = ' '))
      }
    }
    # create empty table
    createOutput <- createTable(connectargs = connectargs, table = table,
      coltypes = dbTypes, stderr = TRUE)
    if (length(createOutput) != 0) {
      stop(paste(createOutput, collapse = ' '))
    }
  }
  # add optional args
  bcpArgs <- append(bcpArgs, bcpOptions)
  output <- suppressWarnings(system2(bcp, args = bcpArgs, ...))
  if (isSpatial && !append) {
    # update empty geometry column with binary data and clean up
    geoOutput <- convertGeoCol(connectargs = connectargs,
      table = table, geometrycol = geometryCol, binarycol = binaryCol,
      spatialtype = spatialtype, srid = srid, stdout = FALSE, stderr = TRUE)
    if (length(geoOutput) != 0) {
      stop(paste(geoOutput, collapse = ' '))
    }
  }
  output
}

#' Export data from SQL Server
#'
#' A wrapper for a system call to the bcp utility which writes a SQL Server
#' table or query (T-SQL) to a file.
#'
#' @param file
#'
#' output file name
#'
#' @param connectargs
#'
#' named list of connection arguments. See \link[bcputility]{makeConnectArgs}.
#'
#' @param table
#'
#' name of the source table when exporting from  SQL Server
#'
#' @param query
#'
#' Transact-SQL query that returns a result set. Ignored if
#' table is specified.
#'
#' @param fieldterminator
#'
#' character separator for columns
#'
#' @param rowterminator
#'
#' character separator for rows--new lines
#'
#' @param bcpOptions
#'
#' list of additional options to pass to the \code{bcp} utility. See details.
#'
#' @param ...
#'
#' arguments to pass \link[base]{system2}
#'
#' @return
#'
#' No return value. Operations from bcp are printed to console; see
#' \code{...} to redirect output

#' @details
#'
#' The \code{bcpOptions} allows the user to include additional arguments for the
#' call to \code{system2}. Please refer to
#' \url{https://learn.microsoft.com/en-us/sql/tools/bcp-utility}.
#' The default options are set
#' to the defaults for \code{bcp} CLI. \code{-b} refers to
#' number of rows to write at a time; 10,000 to 50,000 is a
#' starting recommendation. \code{-a} refers to size of packets to be sent in
#' bytes. \code{-e} refers to the maximum number of errors before failure.
#'
#' @export
#'
bcpExport <- function(
  file,
  connectargs,
  table,
  query,
  fieldterminator = '\t',
  rowterminator = ifelse(.Platform$OS.type == 'windows', '\r\n', '\n'),
  bcpOptions = list(
    '-c',
    '-b', 1000,
    '-a', 4096,
    '-m', 10
  ),
  ...) {
  if (missing(connectargs)) {
    stop('connectargs is missing. See ?bcputility::makeConnectArgs.')
  }
  bcp <- findUtility('bcp')
  bcpArgs <- mapConnectArgs(connectargs = connectargs, utility = 'bcp')
  bcpArgs <- append(bcpArgs,
                    list(
                      '-t', shQuote(fieldterminator),
                      '-r', shQuote(rowterminator)
                    )
  )
  outArg <- 'out'
  if (missing(table)) {
    table <- shQuote(query)
    outArg <- 'queryout'
  }
  bcpArgs <- append(bcpArgs, list(table,
                                  outArg, shQuote(file)), after = 0)
  # add optional args
  bcpArgs <- append(bcpArgs, bcpOptions)
  system2(bcp, args = bcpArgs, ...)
}
#' Check bcp and sqlcmd versions
#'
#' @param ...
#'
#' arguments to pass \link[base]{system2}
#'
#' @export
#'
#' @name SQLServerCLIVersions
#'
bcpVersion <- function(...) {
  bcp <- findUtility('bcp')
  system2(command = bcp, args = list('-v'), ...)
}
#' @export
#'
#' @rdname SQLServerCLIVersions
sqlcmdVersion <- function(...) {
  sqlcmd <- findUtility('sqlcmd')
  system2(command = sqlcmd, args = list('-?'), ...)
}

#' Determine SQL Server data types from data frame. Follows SQL Server
#' data type size constraints and chooses the smallest data type size.
#'
#' @param x
#'
#' data.frame object
#'
#' @param coltypes
#'
#' vector with names of columns to override the default data type mapping
#'
#' @return
#'
#' character vector with names of columns
#'
#' @export
#'
#' @name mapDataTypes
#'
#' @examples
#'
#' mapDataTypes(data.frame(
#'   int = 1:5L,
#'   numeric = seq(0, 1, length.out = 5),
#'   character = LETTERS[1:5],
#'   factor = paste(LETTERS[1:5], LETTERS[1:5], sep = ''),
#'   logical = c(TRUE, FALSE, TRUE, FALSE, TRUE),
#'   date = seq(Sys.Date() - 4, Sys.Date(), 1L),
#'   datetime = seq(Sys.time() - 5, Sys.time(), length.out = 5)
#'   )
#' )
mapDataTypes <- function(x, coltypes) {
  sqlTypes <- vapply(x, FUN = function(.x) {
    # order is important, returns left-most match
    types <- c('character', 'factor', 'integer', 'numeric', 'logical', 'Date',
               'POSIXct', 'sfc', 'blob')
    dataType <- Find(f = function(.type) {
      methods::is(object = .x, class2 = .type)
    }, types, nomatch = 'nomatch')
    switch(dataType,
      character = varChar(.x),
      factor = varChar(.x),
      numeric = 'FLOAT',
      integer = int(.x),
      logical = 'BIT',
      Date = 'DATE',
      POSIXct = 'DATETIME',
      sfc = 'VARBINARY(MAX)',
      blob = varBinary(.x),
      stop('Data type not supported.')
    )
  }, FUN.VALUE = character(1))
  if (!missing(coltypes)) {
    sqlTypes[names(coltypes)] <- coltypes
  }
  sqlTypes
}
#' @export
#'
#' @rdname mapDataTypes
varChar <- function(x) {
  n <- suppressWarnings(max(nchar(as.character(x)), na.rm = TRUE))
  if (is.infinite(n)) {
    n <- 1L
  }
  if (n > 8000) {
    n <- 'MAX'
  }
  sprintf('VARCHAR(%s)', n)
}
#' @export
#'
#' @rdname mapDataTypes
varBinary <- function(x) {
  n <- max(lengths(x), na.rm = TRUE)
  if (n > 8000) {
    n <- 'MAX'
  }
  sprintf('VARBINARY(%s)', n)
}
#' @export
#'
#' @rdname mapDataTypes
int <- function(x) {
  # if all missing assume TINYINT
  # range returns c(Inf, -Inf)
  xRange <- suppressWarnings(range(x, na.rm = TRUE))
  if (xRange[1] >= 0 && xRange[2] <= 255) {
    'TINYINT'
  } else if (xRange[1] >= (-2^15) && xRange[2] <= (2^15 - 1)) {
    'SMALLINT'
  } else if (xRange[1] >= (-2^31) && xRange[2] <= (2^31 - 1)) {
    'INTEGER'
  } else if (xRange[1] >= (-2^63) && xRange[2] <= (2^63 - 1)) {
    'BIGINT'
  } else {
    'Invalid integer range.'
  }
}

#' Create or drop table
#'
#' @param connectargs
#'
#' named list of connection arguments. See \link[bcputility]{makeConnectArgs}.
#'
#' @param table
#'
#' Name of the source table when importing from  SQL Server. For specifying the
#' schema in the table name see \code{<schema>.<table>} and if not specified the
#' default is "dbo".
#'
#' @param coltypes
#'
#' character vector of data types with the column names as list/vector names.
#' Use \link[bcputility]{mapDataTypes} or refer to for proper format.
#'
#' @param ...
#'
#' arguments to pass to \link[base]{system2}
#'
#' @return
#'
#' No return value. Operations from bcp are printed to console; see
#' \code{...} to redirect output
#'
#' @export
#'
createTable <- function(connectargs, table, coltypes, ...) {
  sqlcmd <- findUtility('sqlcmd')
  quotedTable <- quoteTable(table)
  query <- sprintf(
    'CREATE TABLE %s (%s);',
    quotedTable,
    paste(names(coltypes), coltypes, sep = ' ', collapse = ', ')
  )
  sqlcmdArgs <- mapConnectArgs(connectargs = connectargs, utility = 'sqlcmd')
  sqlcmdArgs <- append(sqlcmdArgs, values = list('-Q', shQuote(query)))
  system2(command = sqlcmd, args = sqlcmdArgs, ...)
}
#' @rdname createTable
#'
#' @export
#'
dropTable <- function(connectargs, table, ...) {
  sqlcmd <- findUtility('sqlcmd')
  quotedTable <- quoteTable(table)
  query <- sprintf('DROP TABLE %s;', quotedTable)
  sqlcmdArgs <- mapConnectArgs(connectargs = connectargs, utility = 'sqlcmd')
  sqlcmdArgs <- append(sqlcmdArgs, values = list('-Q', shQuote(query)))
  system2(command = sqlcmd, args = sqlcmdArgs, ...)
}
#' @rdname createTable
#'
#' @export
#'
checkTableExists <- function(connectargs, table) {
  sqlcmd <- findUtility('sqlcmd')
  query <- sprintf("
  IF OBJECT_ID('%s') IS NOT NULL
    BEGIN PRINT 1 END
  ELSE
   BEGIN PRINT 0 END", table)
  sqlcmdArgs <- mapConnectArgs(connectargs = connectargs, utility = 'sqlcmd')
  sqlcmdArgs <- append(sqlcmdArgs, values = list('-Q', shQuote(query)))
  identical(system2(command = sqlcmd, args = sqlcmdArgs, stdout = TRUE)[[1]], '1')
}
readTable <- function(connectargs, table, ...) {
  sqlcmd <- findUtility('sqlcmd')
  quotedTable <- quoteTable(table)
  query <- sprintf('SET NOCOUNT ON; SELECT * FROM %s;', quotedTable)
  queryHeaders <- sprintf('SET NOCOUNT ON; SELECT TOP 0 * FROM %s;',
    quotedTable)
  sqlcmdArgs <- mapConnectArgs(connectargs = connectargs, utility = 'sqlcmd')
  sqlcmdArgs <- append(sqlcmdArgs,
    values = list(
      '-s', shQuote(','),
      '-W',
      '-r', 1,
      '-V', 1
      ))
  sqlCmdArgsHeader <- append(sqlcmdArgs, list('-Q', shQuote(queryHeaders)))
  sqlcmdArgsData <- append(sqlcmdArgs, list('-Q', shQuote(query), '-h', '-1'))
  data.table::fread(
    cmd = paste(append(shQuote(sqlcmd), sqlcmdArgsData), collapse = ' '),
    header = FALSE,
    col.names = strsplit(x = system2(command = sqlcmd, args = sqlCmdArgsHeader,
      stdout = TRUE), split = ',')[[1]])
}
#' Create a named list of connection arguments to translate to bcp and
#' sqlcmd options
#'
#' @param server
#'
#' the instance of SQL Server to which to connect
#'
#' @param database
#'
#' specifies the database to connect to
#'
#' @param username
#'
#' login ID
#'
#' @param password
#'
#' password for login ID
#'
#' @param trustedconnection
#'
#' use integrated security, username and password are not required
#'
#' @param trustservercert
#'
#' trust the server certificate
#'
#' @param azure
#'
#' use Azure Active Directory authentication, does not work with integrated
#' authentication.
#'
#' @param quotedidentifiers
#'
#' set QUOTED_IDENTIFIERS option to 'ON' for the connection between bcp/sqlcmd
#' and SQL Server.
#'
#' @return
#'
#' a list with connection arguments
#'
#' @export
makeConnectArgs <- function(server, database, username, password,
  trustedconnection = TRUE, trustservercert = FALSE, azure = FALSE,
  quotedidentifiers = FALSE) {
  if (isTRUE(trustedconnection) && isTRUE(azure)) {
    stop('trustedconnection and azure cannot both be TRUE')
  }
  connectArgs <- list(server = server, database = database)
  if (isTRUE(trustedconnection)) {
    connectArgs <- append(x = connectArgs,
      values = list(trustedconnection = trustedconnection))
  } else {
    connectArgs <- append(x = connectArgs,
      values = list(username = username, password = password))
  }
  if (isTRUE(azure)) {
    connectArgs <- append(x = connectArgs,
      values = list(azure = azure))
  }
  if (isTRUE(trustservercert)) {
    connectArgs <- append(x = connectArgs,
      values = list(trustservercert = trustservercert))
  }
   if (isTRUE(quotedidentifiers)) {
    connectArgs <- append(x = connectArgs,
      values = list(quotedidentifiers = quotedidentifiers))
  }
  connectArgs
}

# internal functions
quoteTable <- function(table) {
  paste(lapply(strsplit(table, split = '\\.')[[1]], function(x) {
    if (substring(text = x, first = 1, 1) == '[' &&
        substring(text = x,
                  first = nchar(x),
                  last = nchar(x)) == ']') {
      return(x)
    }
    sprintf('[%s]', x)
  }), collapse = '.')
}
convertGeoCol <- function(connectargs, table, geometrycol, binarycol,
  spatialtype, srid, ...) {
  sqlcmd <- findUtility(utility = 'sqlcmd')
  quotedTable <- quoteTable(table)
  query <- sprintf('
  SET NOCOUNT ON;
  ALTER TABLE %1$s ADD [%2$s] %3$s;
  GO
  UPDATE %1$s SET [%2$s] = %3$s::STGeomFromWKB([%4$s], %5$s);
  ALTER TABLE %1$s DROP COLUMN [%4$s];',
    quotedTable, geometrycol, spatialtype, binarycol, srid)
  if (spatialtype == 'geography') {
    correctionQuery <- sprintf(
      '
     UPDATE %1$s SET [%2$s] = [%2$s].MakeValid().ReorientObject().MakeValid()
     WHERE [%2$s].MakeValid().EnvelopeAngle() > 90;',
      quotedTable, geometrycol)
    query <- sprintf('%s %s', query, correctionQuery)
  }
  sqlcmdArgs <- append(mapConnectArgs(connectargs = connectargs,
    utility = 'sqlcmd'), values = list('-Q', shQuote(query)))
  system2(command = sqlcmd, args = sqlcmdArgs, ...)
}
mapConnectArgs <- function(connectargs, utility = c('sqlcmd', 'bcp')
  ) {
  utility <- match.arg(utility)
  argSyntax <- switch(utility,
    sqlcmd = list(server = '-S',
      database = '-d',
      trustedconnection = '-E',
      username = '-U',
      password = '-P',
      azure = '-G',
      trustservercert = '-C',
      quotedidentifiers = '-I'),
    bcp = list(server = '-S',
      database = '-d',
      trustedconnection = '-T',
      username = '-U',
      password = '-P',
      azure = '-G',
      trustservercert = '-u',
      quotedidentifiers = '-q'),
    stop('Unsupported utility')
  )
  argSyntax <- argSyntax[names(connectargs)]
  unlist(
    Map(function(argname, argsyntax, argvalue) {
      if (is.logical(argvalue)) {
        argsyntax
      } else if (is.character(argvalue)) {
        list(argsyntax, shQuote(argvalue))
      } else {
        stop(sprintf('Invalid argument for %s option.', argname))
      }
    }, argname = names(connectargs), argsyntax = argSyntax,
      argvalue = connectargs),
    recursive = FALSE, use.names = FALSE
  )
}
findUtility <- function(utility) {
  optionName <- sprintf('bcputility.%s.path', utility)
  if (!is.null(getOption(optionName))) {
    utility <- getOption(optionName)
  }
  if (Sys.which(utility) == '') {
    stop(sprintf('%1$s was not found or invalid path. Add %1$s to path or to
    "%2$s" option.', utility, optionName))
  }
  utility
}
