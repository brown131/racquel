#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; metadata - Data class metadata 
;;;;
;;;; Copyright (c) Scott Brown 2013

(require "util.rkt")

(provide data-class-metadata% *data-class-metadata* data-class<%>
         data-join data-join-foreign-key 
         data-join-class data-join-key data-join-cardinality
         get-class-metadata-object get-class-metadata set-class-metadata!
         dynamic-get-class-metadata dynamic-set-class-metadata!
         data-class-info get-column-ids get-column-names)

;;; Define data class metadata struct.
(define data-class-metadata% 
  (class object% 
    (field [table-name #f] 
           [columns null] 
           [joins null]
           [primary-key #f] 
           [autoincrement-key #f]
           [external-name #f]
           [state-key (generate-member-key)])
    (super-new)
    (inspect #f)))

;;; Define a global table holding data class metadata.
(define *data-class-metadata* (make-weak-hash))

;;; Define data join struct.
(define-struct data-join (foreign-key class key cardinality))

(define-syntax-rule (get-class-metadata-object cls)  
  (begin (unless (hash-has-key? *data-class-metadata* cls) 
           (hash-set! *data-class-metadata* cls (new data-class-metadata%)))
         (hash-ref *data-class-metadata* cls)))

;;; Get a data class metadata field.
(define-syntax-rule (get-class-metadata id cls)
  (get-field id (get-class-metadata-object cls)))

;;; Set a data class metadata field.
(define-syntax-rule (set-class-metadata! id cls val)
  (set-field! id (get-class-metadata-object cls) val))

;;; Dynamically get a data class metadata field.
(define-syntax-rule (dynamic-get-class-metadata id cls)
  (dynamic-get-field id (get-class-metadata-object cls)))

;;; Dynamically set a data class metadata field.
(define-syntax-rule (dynamic-set-class-metadata! id cls val)
  (dynamic-set-field! id (get-class-metadata-object cls) val))

;;; Return info about a data class.
(define-syntax-rule (data-class-info cls)
  (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info data-class-metadata%)])
    (apply values (map (lambda (f) (dynamic-get-class-metadata f cls)) fld-nms))))

;;; Get a list of column ids for a class.
(define-syntax-rule (get-column-ids cls)
  (map car (get-class-metadata columns cls)))

;;; Get a list of column names for a findf class.
(define-syntax-rule (get-column-names cls)
  (map cdr (get-class-metadata columns cls)))
