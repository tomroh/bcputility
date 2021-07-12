#' Import data to SQL Server
#'
#' A wrapper for a system call to the bcp utility which bulk inserts to SQL Server.
#'
#' @param x
#'
#' dataframe object or path to file
#'
#' @param server
#'
#' the instance of SQL Server to which to connect
#'
#' @param database
#'
#' Specifies the database to connect to
#'
#' @param table
#'
#' name of the source table when exporting from  SQL Server
#'
#' @param driver
#'
#' name of driver for ODBC connection
#'
#' @param maxerrors
#'
#' maximum number of errors allowed
#'
#' @param batchsize
#'
#' number of rows to write at a time; 10,000 to 50,000 is a
#' starting recommendation
#'
#' @param packetsize
#'
#' size of packets to be sent
#'
#' @param regional
#'
#' Specifies that currency, date, and time data is bulk copied into SQL Server
#' using the regional format defined for the locale setting of the client
#' computer
#'
#' @param trustedconnection
#'
#' use integrated security, username and password are not required
#'
#' @param username
#'
#' login ID
#'
#' @param password
#'
#' password for login ID
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
#' @return
#'
#' No return value. Operations from bcp are printed to console; see
#' \code{...} to redirect output
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(33)
#' x <- data.frame(
#'   w = rpois(10, 10),
#'   x = rnorm(10),
#'   y = sample(LETTERS, 10),
#'   z = Sys.time()
#' )
#' bcpImport(x,
#'           server = server,
#'           database = database,
#'           table = 'mytable')
#' }
bcpImport <- function(x,
                      server,
                      database,
                      table,
                      driver = 'SQL Server',
                      maxerrors = 10,
                      batchsize = 1000,
                      packetsize = 4096,
                      regional = FALSE,
                      trustedconnection = TRUE,
                      username,
                      password,
                      fieldterminator = '\t',
                      rowterminator = ifelse(.Platform$OS.type == 'windows', '\r\n', '\n'),
                      overwrite = FALSE,
                      ...) {
  on.exit(DBI::dbDisconnect(con))
  on.exit(file.remove(tmp), add = TRUE)
  if ( trustedconnection ) {
    bcpArgs <- list('-T')
    con <- DBI::dbConnect(odbc::odbc(),
                          driver = driver,
                          server = server,
                          database = database)
  } else {
    bcpArgs <- list('-U', username, '-P', password)
    con <- DBI::dbConnect(odbc::odbc(),
                          driver = driver,
                          server = server,
                          database = database,
                          UID = username,
                          PWD = password)
  }
  bcpArgs <- append(bcpArgs,
                    list(
                      '-c',
                      '-b', batchsize,
                      '-a', packetsize))
  if ( inherits(x, 'data.frame') ) {
    tmp <- tempfile(fileext = '.dat')
    data.table::fwrite(x,
                       tmp,
                       sep = '\t',
                       col.names = FALSE,
                       dateTimeAs = 'write.csv')
  } else {
    stopifnot(file.exists(x))
    tmp <- x
    x <- data.table::fread(tmp, nrows = 0)
    bcpArgs <- append(bcpArgs, list('-t', shQuote(fieldterminator),
                                    '-r', shQuote(rowterminator)))
  }
  bcpArgs <- append(bcpArgs, list(table,
                                  'in', shQuote(tmp),
                                  '-S', server,
                                  '-d', database), after = 0)
  if ( regional ) {
    bcpArgs <- append(bcpArgs, list('-R'))
  }
  if ( overwrite ) {
    if ( DBI::dbExistsTable(con, table) ) {
      DBI::dbRemoveTable(con, name = table)
    }
    DBI::dbCreateTable(con, name = table, fields = x)
  }
  if ( !DBI::dbExistsTable(con, table) ) {
    DBI::dbCreateTable(con, name = table, fields = x)
  }
  #cat(paste(append(bcpArgs, 'bcp', after = 0), collapse = ' '), sep = '\n')
  system2('bcp', args = bcpArgs, ...)
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
#' @param server
#'
#' the instance of SQL Server to which to connect
#'
#' @param database
#'
#' Specifies the database to connect to
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
#' @param trustedconnection
#'
#' use integrated security, username and password are not required
#'
#' @param username
#'
#' login ID
#'
#' @param password
#'
#' password for login ID
#'
#' @param fieldterminator
#'
#' character separator for columns
#'
#' @param rowterminator
#'
#' character separator for rows--new lines
#'
#' @param datatypes
#'
#' the format of datatypes,
#' char performs the operation using a character data type,
#' nchar performs the bulk copy operation using Unicode characters
#'
#' @param ...
#'
#' arguments to pass \link[base]{system2}
#'
#' @return
#'
#' No return value. Operations from bcp are printed to console; see
#' \code{...} to redirect output
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' bcpExport('myfile.tsv',
#'           server = server,
#'           database = database,
#'           table = 'mytable',
#'           fieldterminator = '|',
#'           stdout = FALSE,
#'           datatypes = 'char')
#' }
bcpExport <- function(file,
                      server,
                      database,
                      table,
                      query,
                      trustedconnection = TRUE,
                      username,
                      password,
                      fieldterminator = '\t',
                      rowterminator = ifelse(.Platform$OS.type == 'windows', '\r\n', '\n'),
                      datatypes = c('char', 'nchar'),
                      ...) {
  if ( trustedconnection ) {
    bcpArgs <- list('-T')
  } else {
    bcpArgs <- list('-U', username, '-P', password)
  }
  datatypes <- match.arg(datatypes)
  if ( datatypes == 'char' ) {
    bcpArgs <- append(bcpArgs, list('-c'))
  } else {
    bcpArgs <- append(bcpArgs, list('-w'))
  }
  bcpArgs <- append(bcpArgs,
                    list(
                      '-t', shQuote(fieldterminator),
                      '-r', shQuote(rowterminator)
                    )
  )
  outArg <- 'out'
  if ( missing(table) ) {
    table <- shQuote(query)
    outArg <- 'queryout'
  }
  bcpArgs <- append(bcpArgs, list(table,
                                  outArg, shQuote(file),
                                  '-S', server,
                                  '-d', database), after = 0)
  #cat(paste(append(bcpArgs, 'bcp', after = 0), collapse = ' '))
  system2('bcp', args = bcpArgs, ...)
}

