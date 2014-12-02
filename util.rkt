#lang racket
;;;;
;;;; util - Racquel utilities.
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

(require db "metadata.rkt" "schema.rkt" (for-syntax racket/syntax syntax/parse))

(provide (all-defined-out))

;;; ODBC database system type. Values are: 'sqlserver, 'oracle, or 'db2.
(define *odbc-dbsystem-type* 'sqlserver)

;;; Database system type.
(define (dbsystem-type con) 
  (let ([dbsys-type (dbsystem-name (connection-dbsystem con))])
    (if (equal? dbsys-type 'odbc) *odbc-dbsystem-type* dbsys-type)))

;;; Set the ODBC database system type.
(define (set-odbc-dbsystem-type! odbc-dbsys-type) (set! *odbc-dbsystem-type* odbc-dbsys-type))

;;; Create a multi-dimensional hash table.
(define (make-multi-hash #:weak? (wk? #f)) (if wk? (make-weak-hash) (make-hash)))

;;; Set a value given a sequence of keys.
(define (multi-hash-set! hash-tbl value . keys)
  (if (null? (cdr keys)) (hash-set! hash-tbl (car keys) value)
      (if (hash-has-key? hash-tbl (car keys)) 
          (multi-hash-set! (hash-ref hash-tbl (car keys)) value (cdr keys)) 
          (let ([h (make-multi-hash #:weak? (hash-weak? hash-tbl))]) 
            (hash-set! hash-tbl (car keys) h) (multi-hash-set! h value (cdr keys))))))
          
;;; Retrieve a value given a sequence of keys.
(define (multi-hash-ref hash-tbl . keys) 
  (if (null? (cdr keys)) (if (hash-has-key? hash-tbl (car keys)) (hash-ref hash-tbl (car keys)) #f)
      (if (hash-has-key? hash-tbl (car keys)) 
          (multi-hash-ref (hash-ref hash-tbl (car keys)) (cdr keys)) #f)))
          
;;; Test if the hash contains the given sequence of keys.
(define (multi-hash-has-key? hash-tbl . keys) 
  (if (null? (cdr keys)) (hash-has-key? hash-tbl (car keys))
      (if (hash-has-key? hash-tbl (car keys)) 
          (multi-hash-ref (hash-ref hash-tbl (car keys)) (cdr keys)) #f)))

;;; Define a global hash table holding data class schema.
(define *data-class-schema* (make-multi-hash))

;;; Define type checker for a data class.
(define (data-class? cls) (implementation? cls data-class<%>))

;;; Define type checker for a data object.
(define (data-object? obj) (is-a? obj data-class<%>))

;;; Define an empty interface used to identify a data class.
(define data-class<%> (interface ()))

;;; Class of an object
(define (object-class obj) (let-values ([(cls x) (object-info obj)]) cls))

;;; Get a prepared SQL statement.
(define-syntax (make-select-statement stx)
  (syntax-parse stx
     [(_ con:id cls:id (~optional (~seq #:print? prnt)) 
         (~optional (~seq #:prepare? prep)) where-clause:expr)
      (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                    [prep? (not (or (attribute prep) #'#f))])
        #`(let* ([dbsys-type (dbsystem-type con)]
                 [tbl-nm (sql-escape (get-class-metadata table-name cls) dbsys-type)]
                 [col-nms (sort (map (λ (n) (sql-escape n dbsys-type)) 
                                     (get-column-names cls)) string<?)]
                 [sql (string-append "select " 
                                     (string-join (map (lambda (c) (string-append tbl-nm "." c))
                                                       col-nms) ", ") 
                                     " from " tbl-nm " "
                                     (sql-placeholder where-clause (dbsystem-type con)))]
                 [pst (if (or prnt? (not prep?)) sql (virtual-statement con sql))])
            pst))]))
   
;;; SQL schema by database system type.
(define (load-schema con schema-nm tbl-nm #:reverse-join? (rev-jn? #f) #:db-system-type dbsys-type)
  (unless (multi-hash-has-key? *data-class-schema* con schema-nm tbl-nm)
    (multi-hash-set! *data-class-schema* 
                     (cond [(eq? dbsys-type 'db2) 
                            (load-db2-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'mysql) 
                            (load-mysql-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'oracle) 
                            (load-oracle-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'postgresql) 
                            (load-postgresql-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'sqlite3) 
                            (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'sqlserver) 
                            (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)])  
                     con schema-nm tbl-nm))                                
  (multi-hash-ref *data-class-schema* con schema-nm tbl-nm))
