#lang scribble/manual
@;;;; Racquel - An ORM for Racket
@;;;;
@;;;; racquel - Documentation module for the project
@;;;;
@;;;; Copyright (c) Scott Brown 2013
@;;;;
@;;;; This file is part of Racquel
@;;;;
@;;;; Racquel is free software: you can redistribute it and/or modify
@;;;; it under the terms of the GNU General Public License as published by
@;;;; the Free Software Foundation, either version 3 of the License, or
@;;;; (at your option) any later version.
@;;;;
@;;;; This program is distributed in the hope that it will be useful,
@;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
@;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@;;;; GNU General Public License for more details.
@;;;;
@;;;; You should have received a copy of the GNU General Public License
@;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

@(require racquel
          scribble/manual scribble/eval scribble/bnf
          (for-label racket)
          (for-syntax racket/base racket/class racket/serialize))

@title{Racquel: An Object/Relational Mapper for Racket}
 
Racquel is an object/relational mapper for Racket. It consists of several components.

@itemlist[@item{An extension of Racket's class system that allows mapping of database tables to classes.}
          @item{A generator that automatically creates mapped classes using database schema, including joins.}
          @item{Persistence functions for selecting, saving, and deleting objects in the database.}
          @item{An S-expression based SQL-like query language.}
          @item{Functions for serializing objects to and from JSON or XML.}]

Racquel supports connectivity to all the database systems provided by Racket's @link["http://docs.racket-lang.org/db/"]{DB}
package, which are: MySQL, PostgreSQL, SQLite3, and through ODBC: SQL Server, Oracle, and DB/2.

Racquel can be used by installing the package from @link["https://github.com/brown131/racquel"]{GitHub}
Or, as of Racket 6.0, it can be downloaded using the Package Manager in DrRacket.

@defmodule[racquel]

@section[#:tag "dataclass"]{Data Class Mapping}
 
Mapping of Racket classes to database tables is performed using @racket[data-class], which extends a Racket @racket[class] with
expressions for mapping a database table and columns to the data class. Below is an example of a @racket[data-class] with mapping
expressions.

@racketblock[
(define vehicle% 
  (data-class object%
    (table-name "Vehicle")                         
    (column (vehicle-id #f "Vehicle_Id") (make #f "Make") 
            (year 0 "Year") (axels 1 "Axels"))
    (primary-key vehicle-id)
    (join owner customer% #:cardinality 'one-to-one  
          (where (= (customer% customer-id) (vehicle% customer-id))))
    (define/public (wheels)
      (* (get-field axels this) 2))
    (super-new)))]

Here a database table named "Vehicle" is mapped to the @racket[vehicle%] data class. Columns are defined mapping the columns
of the table to a field of the data class, as well as default value for the field. There is also an expression which defines the
primary key. This table also has a join field @racket[owner] which defines a one-to-one join to a @racket[customer%] object.

@defform/subs[
#:literals (table-name init-column column field join primary-key)
(data-class* superclass-expr (interface-expr ...)
  data-class-clause
  ...)
([data-class-clause
  (table-name table-name external-name)
  (init-column column-decl ...)
  (column column-decl ...)
  (join join-table-id join-decl ...)
  (primary-key primary-key-decl auto-increment-kw)
  class-clause ...]

[init-column-decl
  (maybe-renamed column-name-decl)
  (maybe-renamed default-value-expr column-name-decl)]

[column-decl
  (maybe-renamed default-value-expr column-name)]

[maybe-renamed
  id
  (internal-id external-id)]

[column-name-decl
  column-name
  (column-name external-name)]

[join-decl
 (join-name joined-table-id cardinality-kw where-clause)]

[cardinality-kw (code:line) (code:line #:cardinality cardinality-expr)]

[primary-key-decl
  column-id
  (column-ids ...)]

[auto-increment-kw (code:line) (code:line #:auto-increment auto-increment-expr)]
)]{

Produces a data class value used for persisting data objects from a database.
   
The @racket[table-name] expression is a string that names the database table that the data class is mapped to. An
optional external class name can also be defined. This external name is used when the class is exported
to JSON or XML using the Racquel data class mixins.

The @racket[column] and @racket[init-column] are analogous to class @racket[field] and @racket[init-field]
expressions. The difference being that columns are mapped to columns of a database and are persistent. A data class
may also contain @racket[field] columns, but the fields, as they are not mapped to database columns are not persisted.

Data classes can map joins to other data classes, using a @racket[join] expression so that objects related to 
the object can be contained as part of the object.  For example a join can be defined so that a field of the object
can contain a list of order for a customer. The path of a the join is defined using an RQL expression (see @secref["rql"])
which allows for considerable flexibility in regard to what the contained objects are, e.g. a join could be defined so that
only customer order in the last six months are contained in the object. The cardinality of the join is also definable using a
keyword. Valid values for the cardinality keyword are @racket['one-to-one] and @racket['one-to-many].

Joined objects are loaded lazily, that is, they are not loaded from the database until they are first referenced.

The @racket[primary-key] expression defines the primary key for the mapped table. If a primary key consists of multiple parts
then the columns must be defined in a list. An optional keyword @racket[#:auto-increment] can be used to indicate that the
primary key is an auto-incrementing a.k.a. identity column. Typically the value for this keyword is simply @racket[#t], 
however if the type of database being mapped to is PostgreSQL or Oracle, then the value of the keyword must be a string 
defining the name of the sequence entity being used for the table.

A @racket[data-class] automatically defines a @racket[inspect] with a value of @racket[#f], as class transparancy
is necessary for persistent mechanism. Therefore defining an @racket[inspect] expression will generate an error that
an @racket[inspect] expression has already been defined.
}

@defform[(data-class superclass-expr class-clause ...)]{
This is analagous to the @racket[class] definition, where the interface expression is omitted.                                           
}
 
@defproc[(data-class? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a dtata class, @racket[#f] otherwise.
}
  
@defproc[(data-class-info [class data-class?])
         (values member-name-key?
                 member-name-key?
                 string?
                 (listof (listof identifier? string? string?))
                 (listof (listof identifier? identifier? identifier? any/c))
                 (or/c identifier? (listof identifier?))
                 (or/c #t string? #f)
                 (or/c string? #f))]{
Returns eight values, analogous to the returnvalues of @racket[class-info]:

@itemize[
  @item{@racket[_class-id-key]: the hidden name for the class's metadata id field;}

  @item{@racket[_state-key]: the hidden name for the class's state field;}

  @item{@racket[_table-name]: the database table name for the class;}

  @item{@racket[_columns]: a list of column definition lists. Each column defintion
  consists of the column field, the database column name, and the external name used for serialization;}

  @item{@racket[_joins]: a list of join definition lists. Each join definition
  consists of the join field, the data class of the object(s) joined to, 
  the cardinality (either @racket['one-to-one] or @racket['one-to-many], and the RQL where-clause for the join;}

  @item{@racket[_primary-key]: either an field or a list of fields that constitute the primary key;}

  @item{@racket[_autoincrement-key]: set to @racket[#t] if the primary key is an auto-increment key
         (unless the database system is PostgreSQL or Oracle, in which case it is the name of the sequence used
         for the primary key);}

  @item{@racket[_external-name]: the external name used for JSON/XML serialization;}
]}
                                                      
@section[#:tag "generation"]{Automated Data Class Generation}
                                
A powerful feature of Racquel is the ability to generate @racket[data-class] mappings automatically using 
database schema metadata. This allows for data classes to be defined for all the tables in 
a database without the tedious effort of manually coding the mappings.

@defform/subs[
#:literals (table-name init-column column field join primary-key)
(gen-data-class db-connection
                table-name
                #:db-system-type db-system-type-kw
                #:generate-joins? generate-joins-kw
                #:generate-reverse-joins? generate-reverse-joins-kw
                #:schema-name schema-name
                #:inherits base-class
                #:table-name-normalizer proc
                #:column-name-normalizer proc
                #:join-name-normalizer proc
                #:table-name-externalizer proc
                #:print? print-kw
                #:prepare? prepare-kw
                data-class-clause ...)
([db-system-type-kw (code:line) (code:line #:db-system-type db-system-type)]

[generate-joins-kw (code:line) (code:line #:generate-joins? (or/c #t #f))]

[generate-reverse-joins-kw (code:line) (code:line #:generate-reverse-joins? (or/c #t #f))]

[schema-name-kw (code:line) (code:line #:schema-name (string?))]

[inherits-kw (code:line) (code:line #:inherits (string?))]

[table-name-normalizer-kw (code:line) (code:line #:table-name-normalizer )]

[column-name-normalizer-kw (code:line) (code:line #:column-name-normalizer )]

[join-name-normalizer-kw (code:line) (code:line #:join-name-normalizer )]

[table-name-externalizer-kw (code:line) (code:line #:table-name-externalizer )]

[print-kw (code:line) (code:line #:print? (or/c #t #f))]
)]{
Generates a data class from the specified @racket[table-name] using the @racket[db-connection]. If the database system type is an ODBC connection, then
the particular system type can be specified using the @racket[#:db-system-type] keyword. (This is not necessary if the database system type has
already been defined by calling @racket[set-odbc-dbsystem-type!]. If the @racket[#:db-system-type] is not
specified, then the database system type is determined from the @racket[dbsystem-name] of the
@racket[db-connection], where the @racket['odbc] database system is aassumed to be SQL Server.

The @racket[generate-joins?] and @racket[generate-reverse-joins] keywords control the 
automatic generation of joins and reverse joins. Joins are determined based on foreign key constraints 
in the database, and reverse joins are based on any foreign key contraints referencing it. By default
joins are generated, but reverse joins are not.

In some cases, the table name may not be unique in the database server's metadata. In those cases, the 
@racket[schema-name] can be used to specify the schema that the table resides in.

The base class of the generated data class can be specified using the @racket[#:inherits] keyword. The
default base-class is @racket[object%].

Behavior of the map generation can be controlled by customizing the normalizer procedures which normalize
database names into Racket symbols. Generation of external names from the symbol can also be controlled 
using the externalizer procedures. A normalizer or externalizer can be customized by specifying the
procedure to use with the appropriate keyword. An example of a situation where one would want to override
the default normalizers is if table names in a database started with a prefix.

Below are the default normalizers and externalizers.
}
  
@defproc[(table-name-normalizer [table-name string?]) (string?)]{
This normalizer converts database table names into Racket class names, using a set of rules. First,
the normalizer will convert mixed-case names, e.g. "MixedCase", and make the all lower-case with hyphens
between the names, e.g. "mixed-case". It will then convert any underscores to hyphens. Finally, it will
append a percent sign to the end of the name, since that is the Racket standard for naming classes.

@racketblock[
> (table-name-normalizer "ExampleTable_Name")
  example-table-name%]

This is default normalizer for table names if the @racket[#:table-name-externalizer] keyword is not specified.
}
  
@defproc[(column-name-normalizer [table-name string?]) (string?)]{
This converts column names of a table into Racket symbols, following a set of rules. The 
rules are similar to those for the @racket[table-name-normalizer]. First mixed-case names are converted to 
lower-case with hyphens, then underscores are converted to hyphens.

@racketblock[
> (column-name-normalizer "ExampleColumn_Name")
  example-column-name]

This is default normalizer for table names if the @racket[#:column-name-externalizer] keyword is not specified.
}
  
@defproc[(join-name-normalizer [table-name string?]) (string?)]{
This converts joined table names into Racket symbols, following a set of rules. The 
rules are similar to those for the @racket[column-name-normalizer]. First mixed-case names are converted to 
lower-case with hyphens, then underscores are converted to hyphens. Also, if the cardinality of the join is 
@racket['one-to-many], an "s" is appended to the end of the name (or "es" if the name ends with an "s".)

@racketblock[
> (join-name-normalizer "JoinExample_Address")
  join-example-addresses]

This is default normalizer for table names if the @racket[#:join-name-externalizer] keyword is not specified.
}
  
@defproc[(table-name-externalizer [table-name string?]) (string?)]{
Converts a database table name to an external name for JSON or XML serialization. The default is no change at all to the table name.
}
  
@defproc[(set-odbc-dbsystem-type! [odbc-sys-type (or/c 'sqlserver 'oracle 'db2)]) (void?)]{
If the database system being used is either Oracle or DB/2, then the database system type needs to be set to distinquish the ODBC
connection from a SQL Server connection, which is assumed for an ODBC connection is it is not sepcified using this procedure.
}

@section[#:tag "persistence"]{Data Object Persistence}
 
@defproc[(make-data-object [db-connection connection?] 
                           [data-class data-class?]
                           [primary-key (or/c identifier? (list of identifier?))]) (data-object?)]{
Loads a data object from the database by primary key.
}
 
@defproc[(save-data-object [db-connection connection?] [data-object data-object?]) (void?)]{
Saves a data object into the database connected to. This will either insert this object into the database, if the object's state is @racket['new] or
update if the object has been previously loaded. The object's state will be changed to @racket['saved].                                                                                                                                                                                         
}
 
@defproc[(insert-data-object [db-connection connection?] [data-object data-object?]) (void?)]{
Inserts a data object into the database connected to. The object's state will be changed to @racket['saved].
}
   
@defproc[(update-data-object [db-connection connection?] [data-object data-object?])(void?)]{
Updates a data object into the database connected to. The object's state will be changed to @racket['saved].
}
 
@defproc[(delete-data-object [db-connection connection?] [data-object data-object?])(void?)]{
Deletes a data object from the connected database. The object's state will be changed to @racket['deleted].
}
 
@defproc[(select-data-object [db-connection connection?] [data-class data-class?] 
                             [print-kw (code:line) (code:line #:print? (or/c #t #f))]
                             [prepare-kw (code:line) (code:line #:prepare? (or/c #t #f))]
                             [join-clause any/c] ... [where-clause any/c] [rest any/c] ...) (data-object?)]{
Loads a data object from the database connected to using the criteria defined by the where and/or
join RQL clauses. The object's initial state will be @racket['loaded].

The optional @racket[#:print?] keyword if true, will return only the SQL generated from the RQL. This is useful for debugging.

The optional @racket[#:prepare?] keyword if true, will force the SQL statement generated to not be cached as a prepared statement. 
This is useful for RQL that may have variable inputs, such a a list in an RQL @racket[in] cause.
}
 
@defproc[(select-data-objects [db-connection connection?] [data-class data-class?] 
                              [print-kw (code:line) (code:line #:print? (or/c #t #f))]
                              [prepare-kw (code:line) (code:line #:prepare? (or/c #t #f))]
                              [join-clause any/c] ... [where-clause any/c] [rest any/c] ...) (listof data-object?)]{
Loads data objects from the database connected to using the criteria defined by the where and/or
join RQL clauses. Each object's initial state will be @racket['loaded].

The optional @racket[#:print?] keyword if true, will return only the SQL generated from the RQL. This is useful for debugging.

The optional @racket[#:prepare?] keyword if true, will force the SQL statement generated to not be cached as a prepared statement. 
This is useful for RQL that may have variable inputs, such a a list in an RQL @racket[in] cause.
}
  
@defproc[(data-object-state [data-object data-object?]) (or/c 'new 'loaded 'saved 'deleted)]{
Returns the current state of the data object.

@itemize[
  @item{@racket['new]: the state of a data object that has been created but never stored in the database;}
  @item{@racket['loaded]: the state of a data object that has been loaded from the database;}
  @item{@racket['saved]: the state of a data object that has been saved to the database after being 
         either newly created or loaded from the database;}
  @item{@racket['deleted]: the state of a data object that has been deleted from the database;}
]
}
  
@defproc[(get-column [id symbol?] [data-object data-object?]) (any/c)]{
Gets the value of a data object's column, analogous to @racket[get-field].
}
  
@defproc[(set-column! [id symbol?] [data-object data-object?] [value any/c]) (void?)]{
Sets the value of a data object's column, analogous to @racket[set-field!].
}
  
@defproc[(get-join [id symbol?] [data-object data-object?]) (or/c any/c (listof any/c))]{
Gets the value of a data object join. The value of thie join field is loaded from the database upon
first call. (This is known as "lazy" loading.)
}               

@section[#:tag "rql"]{RQL: The Racquel Query Language}

The RQL query language defines SQL-like S-expressions. The expressions are used to define
selection criteria when loading data objects (using @racket[select-data-object] or @racket[select-data-objects])
or joining to other data objects (using @racket[join]s). RQL-expressions are translated into database system-specific SQL. 

Rather than naming a table in a query, as in an SQL, a class which maps to a table is named instead. For instance if the
table "INVOICE" was mapped to the class @racket[invoice%], the name @racket[invoice%] would be using the an RQL query. In
addition, columns in RQL as identified using a pair with class name and field name. This the SQL table-column
reference "INVOICE.CREATED" would be @racket[(invoice% created)]. A column name must be defined in RQL with it's corresponding
class, in order to be correctly mapped to the corresponding SQL.

RQL query parameters are represented using a question mark (@racket[?]). Since the representation of parameter values can
be database system specific, it is recommended that parameters be used rather than actual hard-coded values, as to ensure correct
mapping of the value into the specific database system's format.

@subsection[#:tag "syntax"]{Syntax forms}

Below is the BNF for RQL expressions.

@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @litchar{                } @litchar{})
        (list @nonterm{expression}
              @BNF-seq[open @litchar{join} @nonterm{table name} @nonterm{search condition} close]
              @BNF-seq[open @litchar{where} @nonterm{search condition} close])
        (list @nonterm{search condition}
              @BNF-seq[@nonterm{boolean term}]
              @BNF-seq[open @litchar{or} @nonterm{search condition} @nonterm{boolean term} close])
        (list @nonterm{boolean term}
              @BNF-seq[@nonterm{boolean factor}]
              @BNF-seq[open @litchar{and} @nonterm{boolean term} @nonterm{boolean factor} close])
        (list @nonterm{boolean factor}
              @BNF-seq[open @litchar{not} @nonterm{boolean test} close])
        (list @nonterm{boolean test}
              @BNF-seq[@nonterm{boolean primary}])
        (list @nonterm{boolean primary}
              @BNF-seq[@nonterm{predicate}]
              @BNF-seq[@nonterm{search condition}])
        (list @nonterm{predicate}
              @BNF-seq[@nonterm{comparison predicate}]
              @BNF-seq[@nonterm{between predicate}]
              @BNF-seq[@nonterm{in predicate}]
              @BNF-seq[@nonterm{like predicate}]
              @BNF-seq[@nonterm{null predicate}])
        (list @nonterm{between predicate}
              @BNF-seq[open @litchar{between} @nonterm{row value constructor} @nonterm{row value constructor} @nonterm{row value constructor} close])
        (list @nonterm{in predicate}
              @BNF-seq[open @litchar{in} @nonterm{row value constructor} @nonterm{in value list} close])
        (list @nonterm{in value list}
              @BNF-seq[@kleeneplus[@nonterm{value expression}]])
        (list @nonterm{like predicate}
              @BNF-seq[open @litchar{like} @nonterm{pattern} close])
        ])
     
@subsection[#:tag "where"]{The where clause}

A @racket[where] clause is used in @racket[select-data-object] and @racket[select-data-objects]. It follows the
behavior of SQL-expressions, which can include AND, OR, =, IN, LIKE, etc., but are expressed as S-expressions. Thus the
SQL-expression "ID = 1" would be coded as the S-expression "(= ID 1)".

Currently only a subset of SQL is supported. Subqueries and existence functions are not supported.

@subsection[#:tag "join"]{The join clause}

Join clauses can also included in @racket[select-data-object] and @racket[select-data-objects] functions. The @racket[join]
clauses must be defined before the @racket[where] clause. There may be any number of join clauses, each expressing a
join relationship, similar to an SQL join clause.

A @racket[join] clause may also be defined in a @racket[data-class] declaration, but is expressed in a slightly different 
form (see @racket[data-class*] above. For instance, the @racket[join] clause, expresses the equivalant of the SQL-expression 
"JOIN PERSON ON PERSON.ID = ADDRESS.PERSON_ID".

@verbatim|{
(join person% (= (person% id) (address% person-id)))
}|

@subsection[#:tag "examples"]{RQL examples}

Below is an example of a manually defined data class map. This class implements interface @racket[my-interface,%>]. It also has
a column @racket[x] which is required to be specified when a new instance of the class is created. There is also a one-to-one
join to an object of class @racket[object%] where the id column of the object equals 1.
@verbatim|{
(define my-class% (data-class* object% (my-interface<%>)
                                   (table-name "test")
                                   (column [id #f ("id" "Id")] 
                                           [name #f ("name" "Name")] 
                                           [title #f ("title" "Title")])
                                   (init-column [x ("x" "X")])
                                   (join [object object% 
                                          #:cardinality 'one-to-one 
                                          (where (= id ?)) 1])
                                   (primary-key id)
                                   (define/public (test) (x + 1))
                                   (super-new)))
                                   }|

This creates an instance of the class above. Note that @racket[x] must be specified.
@verbatim|{
(define obj (new test-class% [x 2]))
}|

Generate a class @racket[book%] from the table Book in the Library database, with joins and reverse joins.
@verbatim|{
(define book% (gen-data-class con "Book" 
               #:schema-name "Library"
               #:generate-joins? #t #:generate-reverse-joins? #t)
}|

Because the @racket[#:print?] keyword is true, this will return the SQL that would be used to select the objects from
the database.
@verbatim|{
(select-data-objects con address% #:print? #t (where (in id ,address-ids))
}|

@subsection[#:tag "tips"]{Tips and suggestions}

@section[#:tag "serialization"]{Data Object Serialization}

@defproc[(data-object->jsexpr [object data-object?]) (jsexpr?)]{
Serializes a data object into a JS-expression.
}

@defproc[(jsexpr->data-object [jsx jsexpr?]) (data-object?)]{
Creates a data object from a JS-expression string.
}

@defproc[(data-object->xexpr [object data-object?]) (xexpr?)]{
Serializes a data object into an X-expression.
}

@defproc[(xexpr->data-object [xml xexpr?]) (data-object?)]{
Creates a data object from an X-expression.
}
