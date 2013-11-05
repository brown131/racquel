#lang racket
;;;;
;;;; util - Racquel utilities.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db "metadata.rkt" "schema.rkt")

(provide (all-defined-out))

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
