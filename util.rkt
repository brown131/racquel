#lang racket
;;;;
;;;; util - Racquel utilities.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db "metadata.rkt" "schema.rkt")

(provide (all-defined-out))

; Create a multi-dimensional hash table.
(define (make-multi-hash) (make-hash))

;;; Define a global table holding data class schema.
(define *data-class-schema* (make-multi-hash))

;;; Define type checker for a data class.
(define (data-class? cls) (implementation? cls data-class<%>))

;;; Define an empty interface used to identify a data class.
(define data-class<%> (interface ()))

;;; Class of an object
(define (object-class obj) (let-values ([(cls x) (object-info obj)]) cls))

;;; Database system type.
(define-syntax-rule (dbsystem-type con) (dbsystem-name (connection-dbsystem con)))

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
          (let ([h (make-multi-hash)]) (hash-set! hash-tbl (car keys) h) (multi-hash-set! h value (cdr keys))))))
          
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
