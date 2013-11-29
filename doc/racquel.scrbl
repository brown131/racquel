#lang scribble/doc
@(require "../main.rkt" 
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
 
Mapping of Racket classes to database table is performed using data-class, which extends the Racket class.  

@examples[
(define car% (data-class object%
    (column (id #f "ID") (make #f "MAKE") (year 0 "YEAR") (axels 1 "AXELS"))
    (primary-key id)
    (define/public (wheels)
      (* (get-field axels this) 2))
    (inspector #f)
    (super-new)))
]

@defform/subs[
#:literals (table-name init-column column field join primary-key)
(data-class* superclass-expr (interface-expr ...)
  data-class-clause
  ...)
([data-class-clause
  (table-name table-name)
  (init-column column-decl ...)
  (column column-decl ...)
  (join join-decl ...)
  (primary-key primary-key-decl #:auto-increment val)
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
  (column-name external-name)])]{

Produces a data class value.
   

;;; TODO: RE-WRITE
The @racket[superclass-expr] expression is evaluated when the
@racket[class*] expression is evaluated. The result must be a class
value (possibly @racket[object%]), otherwise the
@racket[exn:fail:object].  The result of the
@racket[superclass-expr] expression is the new class's superclass.

;;; TODO: RE-WRITE
The @racket[interface-expr] expressions are also evaluated when the
@racket[class*] expression is evaluated, after
@racket[superclass-expr] is evaluated. The result of each
@racket[interface-expr] must be an interface value, otherwise the
@racket[exn:fail:object].  The interfaces returned by the
@racket[interface-expr]s are all implemented by the class. For each
identifier in each interface, the class (or one of its ancestors) must
declare a public method with the same name, otherwise the
@racket[exn:fail:object]. The class's superclass must satisfy the
implementation requirement of each interface, otherwise the
@racket[exn:fail:object].

An @racket[inspect] @racket[class-clause] selects an inspector 
}
                                
                                
@section[#:tag "generation"]{Automated Data Class Generation}
                                

@section[#:tag "persistence"]{Data Object Persistence}
                      

@section[#:tag "rql"]{RQL: Racquel Query Language}


@section[#:tag "mixins"]{Data Object Serialization}

@defform[(json-data-class-mixin class-definition)]
{
Defines a mixin that implements the @racket[externalize] and @racket[internalize] methods.
}
