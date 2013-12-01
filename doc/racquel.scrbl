#lang scribble/doc
@(require racquel
          scribble/manual scribble/eval
          (for-label racket)
          (for-syntax racket/base racket/class racket/serialize))

@title{Racquel: An Object/Relational Mapper for Racket}
 
Racquel is an object/relational mapper for Racket. It consists of several components.

@itemlist[@item{An extension of Racket's class system that allows mapping of database tables to classes.}
          @item{A generator that automatically creates mapped classes using database schema, including joins.}
          @item{Persistence functions for selecting, saving, and deleting objects in the database.}
          @item{An S-expression based SQL-like query language.}
          @item{Mix-in classes for serializing objects to and from JSON or XML.}]

Racquel supports connectivity to all the database systems provided by Racket's DB package, which are: MySQL, PostgreSQL, 
SQLite3, and through ODBC: SQL Server, Oracle, and DB/2.

Racquel can be used by downloading and installing the package from @link["http://planet.racket-lang.org/"]{PLaneT}.

@defmodule[racquel]

@section[#:tag "dataclass"]{Data Class Mapping}
 
Mapping of Racket classes to database tables is performed using @racket[data-class], which extends a Racket @racket[class] with
expressions for mapping a database table and columns to the data class. Below is an example of a @racket[data-class] with mapping
expressions.

@racketblock[
(define vehicle% 
  (data-class object%
    (table-name "VEHICLE")                         
    (column (vehicle-id #f "VEHICLE_ID") (make #f "MAKE") (year 0 "YEAR") (axels 1 "AXELS"))
    (primary-key vehicle-id)
    (join owner customer% #:cardinality 'one-to-one  
          (where (= (customer% customer-id) (vehicle% customer-id))))
    (define/public (wheels)
      (* (get-field axels this) 2))
    (super-new)))]

Here a database table named "VEHICLE" is mapped to the @racket[vehicle%] data class. Columns are defined mapping the columns
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
however if the type of database being mapped to is Postgres or Oracle, then the value of the keyword must be a string 
defining the name of the sequence entity being used for the table.

A @racket[data-class] automatically defines a @racket[inspect] with a value of @racket[#f], as class transparancy
is necessary for persistent mechanism. Therefore defining an @racket[inspect] expression will generate an error that
an @racket[inspect] expression has already been defined.
}

@defform[(data-class superclass-expr class-clause ...)]{
This is analagous to the @racket[class] definition, where the interface expression is omitted.                                           
}
 
@defform[(data-class? sym)]{
Determines if a symbol is a data class.
}
  
@defform[(data-class-info cls)]{
Determines if a symbol is a data class.
}
                              
                                
@section[#:tag "generation"]{Automated Data Class Generation}
                                
A powerful feature of Racquel is the ability to generate @racket[data-class] mappings automatically using 
database schema meta-data. This allows for data classes to be defined for all the tables in 
a database without the tedious effort of manually coding the mappings.

@defform/subs[
#:literals (table-name init-column column field join primary-key)
(gen-data-class db-connection
                table-name 
                #:db-system-type val
                #:generate-joins? val
                #:generate-reverse-joins? val
                #:schema-name val
                #:inherits val
                #:table-name-normalizer proc
                #:column-name-normalizer proc
                #:join-name-normalizer proc
                #:table-name-externalizer proc
                #:print? (prnt? #f)
                data-class-clause ...)
([db-system-type-kw (code:line) (code:line #:db-system-type cardinality-expr)]

[generate-joins?-kw (code:line) (code:line #:generate-joins? )]

[generate-reverse-joins?-kw (code:line) (code:line #:generate-reverse-joins? )]

[schema-name-kw (code:line) (code:line #:schema-name )]

[inherits-kw (code:line) (code:line #:inherits )]

[table-name-normalizer-kw (code:line) (code:line #:table-name-normalizer )]

[column-name-normalizer-kw (code:line) (code:line #:column-name-normalizer )]

[join-name-normalizer-kw (code:line) (code:line #:join-name-normalizer )]

[table-name-externalizer-kw (code:line) (code:line #:table-name-externalizer )]

[print?-kw (code:line) (code:line #:print? )]
)]{

}
  
@defform[(default-table-name-normalizer table-name)]{

}
  
@defform[(default-column-name-normalizer table-name)]{

}
  
@defform[(default-join-name-normalizer table-name)]{

}
  
@defform[(default-table-name-externalizer table-name)]{

}
  
@defform[(set-odbc-dbsystem-type! odbc-sys-type)]{

}

@section[#:tag "persistence"]{Data Object Persistence}
 
@defform[(make-data-object db-connection data-class primary-key)]{
Load a data object from the database by primary key.
}
 
@defform[(save-data-object db-connection data-object)]{
Save a data object.
}
 
@defform[(insert-data-object db-connection data-object)]{
Save a data object.
}
   
@defform[(update-data-object db-connection data-object)]{
Save a data object.
}
 
@defform[(delete-data-object db-connection data-object)]{
Save a data object.
}
 
@defform[(select-data-object db-connection data-class join-clause ... where-clause rest)]{
Save a data object.
}
 
@defform[(select-data-objects db-connection data-class join-clause ... where-clause rest)]{
Save a data object.
}
  
@defform[(data-object-state sym)]{
Determines if a symbol is a data class.
}
  
@defform[(get-column sym)]{
Determines if a symbol is a data class.
}
  
@defform[(set-column! sym)]{
Determines if a symbol is a data class.
}
  
@defform[(get-join sym)]{
Determines if a symbol is a data class.
}
                   

@section[#:tag "rql"]{RQL: The Racquel Query Language}


@section[#:tag "mixins"]{Data Object Serialization}

@defform[(json-data-class-mixin class-definition)]{
Defines a mixin that implements the @racket[externalize] and @racket[internalize] methods.
}

@defform[(xml-data-class-mixin class-definition)]{
Defines a mixin that implements the @racket[externalize] and @racket[internalize] methods.
}
