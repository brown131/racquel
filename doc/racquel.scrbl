#lang scribble/manual

@title{Racquel: An ORM for Racket}
 
Racqual is an object relational modeller for Racket. It consists of several components.

@itemlist[@item{An extension of Racket's class system that allows mapping of database tables to classes.}
          @item{A generator that automatically creates mapped classes using database schema meta-data.}
          @item{Functions for saving and deleting persistent objects.}
          @item{RQL: An S-expression based SQL-like query language.}
          @item{Serializers for serializing objects to and from JSON or XML.}]

