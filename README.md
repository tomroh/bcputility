
# bcputility

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bcputility)](https://CRAN.R-project.org/package=bcputility)
[![R-CMD-check](https://github.com/tomroh/bcputility/workflows/R-CMD-check/badge.svg)](https://github.com/tomroh/bcputility/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
<!-- badges: end -->

**bcputility** is a wrapper for the command line utility program from SQL Server 
that does bulk imports/exports. The package assumes that [bcp](https://docs.microsoft.com/en-us/sql/tools/bcp-utility?view=sql-server-ver15)
is already installed and is on the system search path. For large inserts to SQL 
Server over an ODBC connection (e.g. with the 
"[DBI](https://db.rstudio.com/dbi/)" package), writes can take a very long time as 
each row generates an individual insert statement. The bcp Utility greatly 
improves performance of large writes by using bulk inserts.

An export function is provided for convenience, but likely will not significantly
improve performance over other methods.


## Installation

You can install the released version of bcputility from 
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bcputility")
```

Install the development version with:

```r
devtools::install_github("tomroh/bcputility")
```

## Benchmarks

Benchmarks were performed with a local installation of SQL Server Express. 
When testing with a remote SQL Server, performance of *bcp* over *odbc* was further 
improved.

### Import

``` r
library(DBI)
library(data.table)
library(nycflights13)
data("flights")
set.seed(82)
flights <- flights[sample.int(nrow(flights), 1000000, replace = TRUE),]
server <- 'localhost\\SQLEXPRESS'
database <- 'Main'
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "SQL Server",
                      Server = server,
                      Database = database)
results <- microbenchmark::microbenchmark(
  bcpImport1000 = {
    bcpImport(flights,
              server = server,
              database = database,
              table = 'flights1',
              overwrite = TRUE,
              stdout = FALSE)
    },
  bcpImport10000 = {
    bcpImport(flights,
              server = server,
              database = database,
              table = 'flights2',
              overwrite = TRUE,
              stdout = FALSE,
              batchsize = 10000)
  },
  bcpImport50000 = {
    bcpImport(flights,
              server = server,
              database = database,
              table = 'flights3',
              overwrite = TRUE,
              stdout = FALSE,
              batchsize = 50000)
  },
  dbWriteTable = {
    DBI::dbWriteTable(con, name = 'flights4', flights, overwrite = TRUE)
    },
  times = 30L
)
```

|expr           |      min|       lq|     mean|   median|       uq|      max| neval|
|:--------------|--------:|--------:|--------:|--------:|--------:|--------:|-----:|
|bcpImport1000  | 13.83131| 14.92755| 15.59979| 15.26361| 16.50634| 17.31753|    30|
|bcpImport10000 | 11.41458| 12.96426| 13.61049| 13.42964| 14.34338| 15.46362|    30|
|bcpImport50000 | 10.70270| 12.46274| 13.29288| 13.10546| 13.59440| 17.85827|    30|
|dbWriteTable   | 25.71646| 28.39663| 29.63298| 29.39512| 30.49987| 35.38698|    30|

*Time in seconds*

### Export Table

**Note:** *bcp* exports of data may not match the format of ```fwrite```. 
```dateTimeAs = 'write.csv'``` was used to make timings comparable, which 
decreased the performance of "[data.table](https://rdatatable.gitlab.io/data.table/)". 
Optimized write formats for date times from ```fwrite``` outperforms *bcp* for 
data that is small enough to be pulled into memory.

``` r
exportResults <- microbenchmark::microbenchmark(
  bcpExportChar = {
    bcpExport('test1.csv',
              server = server,
              database = database,
              table = 'flights1',
              fieldterminator = ',',
              stdout = FALSE,
              datatypes = 'char')
    },
  bcpExportNchar = {
    bcpExport('test2.csv',
              server = server,
              database = database,
              table = 'flights1',
              fieldterminator = ',',
              stdout = FALSE,
              datatypes = 'nchar')
  },
  fwriteQuery = {
    fwrite(DBI::dbReadTable(con, 'flights1'),
           'test3.csv', dateTimeAs = 'write.csv',
           col.names = FALSE)
  },
  times = 30L
)
```

|expr           |       min|        lq|      mean|    median|       uq|       max| neval|
|:--------------|---------:|---------:|---------:|---------:|--------:|---------:|-----:|
|bcpExportChar  |  6.179264|  6.527793|  7.051096|  7.207091|  7.40403|  8.221965|    30|
|bcpExportNchar |  7.127671|  7.685442|  8.240679|  8.413417|  8.54752|  9.313255|    30|
|fwriteQuery    | 11.096326| 11.515995| 11.852508| 11.741095| 12.20761| 13.015459|    30|

*Time in seconds*

### Export Query Benchmarks

``` r
queryResults <- microbenchmark::microbenchmark(
  bcpExportQueryChar = {
    bcpExport('test4.csv',
              server = server,
              database = database,
              query = query,
              fieldterminator = ',',
              stdout = FALSE,
              datatypes = 'char')
  },
  bcpExportQueryNchar = {
    bcpExport('test5.csv',
              server = server,
              database = database,
              query = query,
              fieldterminator = ',',
              stdout = FALSE,
              datatypes = 'nchar')
  },
  fwriteQuery = {
    fwrite(DBI::dbGetQuery(con, query),
           'test6.csv', dateTimeAs = 'write.csv',
           col.names = FALSE)
  },
  times = 30L
)
```

|expr                |      min|       lq|     mean|   median|       uq|      max| neval|
|:-------------------|--------:|--------:|--------:|--------:|--------:|--------:|-----:|
|bcpExportQueryChar  | 2.330189| 2.335963| 2.777578| 2.444756| 3.010092| 4.892223|    30|
|bcpExportQueryNchar | 2.666880| 2.777737| 3.435565| 2.789167| 3.443354| 7.774271|    30|
|fwriteQuery         | 3.384254| 3.572138| 4.285159| 3.904367| 4.361619| 8.017063|    30|

*Time in seconds*
