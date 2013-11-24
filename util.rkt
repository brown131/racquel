#lang racket
;;;;
;;;; util - Racquel utilities.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db "metadata.rkt" "schema.rkt")

(provide (all-defined-out))


;;; ODBC database system type. Values are: 'sqlserver, 'oracle, or 'db2.
(define *odbc-dbsystem-type* 'sqlserver)

;;; Database system type.
(define-syntax-rule (dbsystem-type con) 
  (let ([dbsys-type (dbsystem-name (connection-dbsystem con))])
    (if (equal? dbsys-type 'odbc) *odbc-dbsystem-type* dbsys-type)))

(define (set-odbc-dbsystem-type! odbc-dbsys-type) (set! *odbc-dbsystem-type* odbc-dbsys-type))

; Create a multi-dimensional hash table.
(define (make-multi-hash #:weak? (wk? #f)) (if wk? (make-weak-hash) (make-hash)))

;;; Define a global hash table holding data class schema.
(define *data-class-schema* (make-multi-hash))

;;; Define type checker for a data class.
(define (data-class? cls) (implementation? cls data-class<%>))

;;; Define an empty interface used to identify a data class.
(define data-class<%> (interface ()))

;;; Class of an object
(define-syntax-rule (object-class obj) (let-values ([(cls x) (object-info obj)]) cls))

;;; Select SQL.
(define (select-sql con cls where-clause)
  (let ([col-nms (sort (get-column-names cls) string<?)])
    (string-append "select " (string-join col-nms ", ") 
                   " from " (get-class-metadata table-name cls) " "
                   (sql-placeholder where-clause (dbsystem-type con)))))

; Set a value given a sequence of keys.
(define (multi-hash-set! hash-tbl value . keys)
  (if (null? (cdr keys)) (hash-set! hash-tbl (car keys) value)
      (if (hash-has-key? hash-tbl (car keys)) (multi-hash-set! (hash-ref hash-tbl (car keys)) value (cdr keys)) 
          (let ([h (make-multi-hash #:weak? (hash-weak? hash-tbl))]) (hash-set! hash-tbl (car keys) h) (multi-hash-set! h value (cdr keys))))))
          
; Retrieve a value given a sequence of keys.
(define (multi-hash-ref hash-tbl . keys) 
  (if (null? (cdr keys)) (if (hash-has-key? hash-tbl (car keys)) (hash-ref hash-tbl (car keys)) #f)
      (if (hash-has-key? hash-tbl (car keys)) (multi-hash-ref (hash-ref hash-tbl (car keys)) (cdr keys)) #f)))
          
; Test if the hash contains the given sequence of keys.
(define (multi-hash-has-key? hash-tbl . keys) 
  (if (null? (cdr keys)) (hash-has-key? hash-tbl (car keys))
      (if (hash-has-key? hash-tbl (car keys)) (multi-hash-ref (hash-ref hash-tbl (car keys)) (cdr keys)) #f)))
   
;;; SQL schema by database system type.
(define (load-schema con schema-nm tbl-nm #:reverse-join? (rev-jn? #f) #:db-system-type dbsys-type)
  (unless (multi-hash-has-key? *data-class-schema* con schema-nm tbl-nm)
    (multi-hash-set! *data-class-schema* 
                     (cond [(eq? dbsys-type 'mysql) (load-mysql-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'oracle) (load-oracle-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'postgresql) (load-postgresql-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'sqlite3) (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)]
                           [(eq? dbsys-type 'sqlserver) (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)])  
                     con schema-nm tbl-nm))                                
  (multi-hash-ref *data-class-schema* con schema-nm tbl-nm))

;;; True if string t contains string s.
;(define (string-contains t s) 