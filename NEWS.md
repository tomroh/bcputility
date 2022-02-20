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
