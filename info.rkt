#lang setup/infotab
;;;; Racquel - An ORM for Racket
;;;;
;;;; info - Package information.
;;;;
;;;; Copyright (c) Scott Brown 2013
;;;;
;;;; This file is part of Racquel
;;;;
;;;; Racquel is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
