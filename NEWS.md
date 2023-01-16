# bcputility 0.4.0

* Override/set path to sqlcmd with 
`options(bcputility.sqlcmd.path = "<path-to-sqlcmd>")`

* Added `bcpVersion` and `sqlcmdVersion` to check versions.

* Specify tables without schema strictly as a character vector of size 1 e.g.
```"<schema>.<table>"```. The schema and table will be quoted although special 
characters are not recommended except for "_".

* Tests have been added that can run against a local instance of SQL Server. 
These do not work without it as SQL Server is a client/server configuration.

* The README has been updated for new syntax and updated benchmarks.

* `bcpImport` and `bcpExport` gain a bcpOptions argument that allows the user 
to include any options from the CLI. See documentation for formatting and 
specification.

* The connections arguments for `bcpImport` and `bcpExport` have been 
deprecated--server, database, username, password, trustedconnection. Use 
`makeConnectArgs` for the connectargs argument which adds Azure AD 
authentication and the option to disregard the trust server certificate.

* The "DBI" dependency has been dropped and all SQL Server queries are run 
using bcp or sqlcmd.

* `mapDataTypes` is a convenience function to determine SQL Server data type 
conversions. The defaults attempt to determine the smallest type size for 
each column.

* `createTable`, `dropTable`, and `checkTableExists` have been added for 
internals and convenience. The primary purpose would be to define an empty 
table with user-defined types that `bcpImport` could append to.

* logicals no longer need to be converted from `FALSE/TRUE` to `0/1` before 
importing in-memory data.

* The argument azure was added to `bcpImport` and `bcpExport` for Azure 
Active Directory authentication. This only works for username and password 
authentication.

# bcputility 0.3.0

* `bcpImport` now returns the output of system2 for all specifications of 
stderr and stdout

* added documentation for using schema and `DBI::Id` is now supported

* trusted connections are now compatible with newer sql server drivers as the 
default for all drivers is now `trusted_connection='yes'`

* user name and passwords are now quoted with `shQuote` so special characters 
are handled across platforms

* added check to see if bcp program can be found

# bcputility 0.2.0

* set the path to the bcp utility with the bcputility.bcp.path option

* added links to source code, bug reports, and documentation site

* rowterminator and fieldterminator are no passed to `data.table::fwrite` if data
is in memory

* Added support for geometry/geography data import for 'sf' objects

# bcputility 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added `bcpImport` to import data to SQL Server
* Added `bcpExport` to export data from SQL Server
