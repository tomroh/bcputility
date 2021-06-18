% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcp.R
\name{bcpExport}
\alias{bcpExport}
\title{Export data from SQL Server}
\usage{
bcpExport(
  file,
  server,
  database,
  table,
  query,
  trustedconnection = TRUE,
  username,
  password,
  fieldterminator = "\\t",
  rowterminator = ifelse(.Platform$OS.type == "windows", "\\r\\n", "\\n"),
  ...
)
}
\arguments{
\item{file}{output file name}

\item{server}{the instance of SQL Server to which to connect}

\item{database}{Specifies the database to connect to}

\item{table}{name of the source table when exporting from  SQL Server}

\item{query}{Transact-SQL query that returns a result set. Ignored if
table is specified.}

\item{trustedconnection}{use integrated security, username and password are not required}

\item{username}{login ID}

\item{password}{password for login ID}

\item{fieldterminator}{character separator for columns}

\item{rowterminator}{character separator for rows--new lines}

\item{...}{arguments to pass \link[base]{system2}}
}
\description{
A wrapper for a system call to the bcp utility which writes a SQL Server
table or query (T-SQL) to a file.
}