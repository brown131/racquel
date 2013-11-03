#lang scribble/manual

@title{Racquel: An ORM for Racket}
 
Racqual is an object relational modeller for Racket. It consists of several components.

@itemlist[@item{An extension of Racket's class system that allows mapping of database tables to classes.}
          @item{A generator that automatically creates mapped classes using database schema.}
          @item{Functions for selecting, saving, and deleting persistent objects.}
          @item{RQL: An S-expression based SQL-like query language.}
          @item{Mix-ins for serializing objects to and from JSON or XML.}]

Racquel supports connectivity to all the database systems provided by Racket's DB package, which are: MySQL, PostgreSQL, 
SQLite3, SQL Server, Oracle, and DB/2.

Racquel can be used by downloading ...
 
@section{Data Class Mapping}
 
Mapping of Racket classes to databast table is performed using data-class, which extends the Racket class.

syntax
(data-class* superclass-expr (interface-expr ...)
  (table-name table-name)
  (init-column column-decl ...)
  (column column-decl ...)
  (join join-decl ...)
  (primary-key primary-key-decl)
  class-clause
  ...)

  