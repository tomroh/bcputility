% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcp.R
\name{mapDataTypes}
\alias{mapDataTypes}
\alias{varChar}
\alias{varBinary}
\alias{int}
\title{Determine SQL Server data types from data frame. Follows SQL Server
data type size constraints and chooses the smallest data type size.}
\usage{
mapDataTypes(x, coltypes)

varChar(x)

varBinary(x)

int(x)
}
\arguments{
\item{x}{data.frame object}

\item{coltypes}{vector with names of columns to override the default data type mapping}
}
\value{
character vector with names of columns
}
\description{
Determine SQL Server data types from data frame. Follows SQL Server
data type size constraints and chooses the smallest data type size.
}
\examples{

mapDataTypes(data.frame(
  int = 1:5L,
  numeric = seq(0, 1, length.out = 5),
  character = LETTERS[1:5],
  factor = paste(LETTERS[1:5], LETTERS[1:5], sep = ''),
  logical = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  date = seq(Sys.Date() - 4, Sys.Date(), 1L),
  datetime = seq(Sys.time() - 5, Sys.time(), length.out = 5)
  )
)
}
