#lang setup/infotab
(define name "Racquel: An Object/Relational Mapper for Racket")
(define blurb 
  '("Racquel is an object/relational mapper for Racket. Its features include both manual and automatic generation of 
mappings, a basic S-expression query language, and JSON and XML serialization. Supports MySQL, Postgres, SQLite, SQL Server, 
Oracle, and DB/2."))
(define release-notes
  '((p "0.9  Preliminary release.")))
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.3.16")
(define version "0.9")
(define scribblings '(("racquel.scrbl")))
(define primary-file "main.rkt")
