
# bcputility

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bcputility)](https://CRAN.R-project.org/package=bcputility)
[![R-CMD-check](https://github.com/tomroh/bcputility/workflows/R-CMD-check/badge.svg)](https://github.com/tomroh/bcputility/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
<!-- badges: end -->

bcputility is a wrapper for the commmand line utility program from SQL Server 
that does bulk imports/exports. The package assumes that bcp is already installed 
and is on the system search path. For large inserts to SQL Server over an 
ODBC connection (e.g. with the [DBI](https://db.rstudio.com/dbi/) package), 
writes can take a very long time as each row generates an individual insert 
statement. The bcp Utility greatly improves performance of large writes by 
using bulk inserts.

An export function is provided for convenience, but likely will not significantly
improve performance over other methods.


## Installation

You can install the released version of bcputility from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("bcputility")
```

Install the development version with:

```r
devtools::install_github("tomroh/bcputility")
```

## Import Benchmarks

``` r

```

## Export Table Benchmarks

``` r

```

## Export Query Benchmarks

``` r

```

