racquel
=======

An ORM for Racket. 

I was dissatisfied with the lack of an ORM that uses Racket Objects and supports multiple databases, so I
decided to write my own.

THIS IS STILL UNDER DEVELOPMENT.

Requirements:

   * Supports MySQL.
   * Supports SQLite3.
   * Supports Postgres.
   * Supports SQL Server (ODBC).
   * Supports Oracle (ODBC).
   * Supports DB/2 (ODBC).
   * Supports Mixed case, Camel case, or a separator ("_") in table and column names when generating.
   * Can select one or more objects using where-cause s-expression.
   * Generates data classes using schema info.
   * Can create object using primary key.
   * Multiple-part primary keys are lists of key values.
   * Supports auto-increment in DB systems that support it (MySQL, SQL Server, ?)
   * Can select one or more objects using where-clause string.
   * Objects have save method.
   * Objects have insert method.
   * Objects have update method.
   * Objects have delete method.
   * Objects have contained objects (joins).
   * Joined objects are loaded lazily.
   * Can print generated class s-expression.
   * Can externalize/internalize to/from JSON.
   * Can externalize/internalize to/from XML.
   * Can have a different base class.
   * Can a-synch select objects in the background.
   * Has level-1 (primary key) cache.
   * Can generate accessors and mutators.
