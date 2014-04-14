racquel - An Object/Relational Mapper for Racket


Racquel is an object/relational mapper for Racket. Its features include both manual and automatic generation of 
mappings, a basic S-expression query language, and JSON and XML serialization. This supports MySQL, Postgres, SQLite, SQL Server, 
Oracle, and DB/2.

I was dissatisfied with the lack of an ORM that uses Racket Objects and supports multiple databases, so I
decided to write my own. This is a preliminary release of this package. I will probably consider it "released" once there have 
been a couple of users to shake any remaining bugs out.

See the [Racquel documentation](http://www.browncross.com/racquel/doc/racquel.html) for further details.

To Do/Wish List:
   * Support Views as well as Tables. Would need to determine primary key for generating.
   * Qualify all generated SQL names by DB type (\`name\`, [name], etc.)
   * Create select-value, select-list, and select-rows for sub-queries.
   * Track changes in get-column for updates.
   * Test object inheritance.
