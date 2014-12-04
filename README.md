## racquel - An Object/Relational Mapper for Racket

Racquel is an object/relational mapper for Racket. Its features include both manual and automatic 
generation of mappings, a basic S-expression query language, and JSON and XML serialization. This 
supports MySQL, Postgres, SQLite, SQL Server, Oracle, and DB/2.

See the [Racquel documentation]
(http://htmlpreview.github.io/?https://github.com/brown131/racquel/blob/master/doc/racquel.html) 
for further details.

To Do/Wish List:
   * Support Views as well as Tables. Would need to determine primary key for generating.
   * Create select-value, select-list, and select-rows for sub-queries.
   * Track changes in get-column for updates.
   * Test object inheritance.
