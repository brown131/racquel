#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; metadata - Data class metadata 
;;;;
;;;; Copyright (c) Scott Brown 2013

(require "schema.rkt")

(provide (all-defined-out))

;;; Define a global hash table holding data class metadata.
(define *data-class-metadata* (make-hash))

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

;;; Get metadata for a class.
(define-syntax-rule (get-class-metadata-object cls)
  (begin (unless (class? cls) (error "key is not a class"))
         (unless (hash-has-key? *data-class-metadata* cls) 
           (hash-set! *data-class-metadata* cls (new data-class-metadata%)))
         (hash-ref *data-class-metadata* cls)))

;;; Get a class from the metadata by name or symbol.
(define-syntax-rule (get-class cls-name)
  (findf (lambda (c) (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info c)])
                       (eq? cls-nm (if (string? cls-name) (string->symbol cls-name) cls-name)))) (hash-keys *data-class-metadata*)))

;;; Get a class from the metadata by name or symbol.
(define-syntax-rule (get-class-name cls)
  (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)]) cls-nm))

;;; Get a class from the metadata by table name.
(define-syntax-rule (get-table-class tbl-name)
  (findf (lambda (c) (equal? (get-class-metadata table-name c) tbl-name)) (hash-keys *data-class-metadata*)))

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
  (map first (get-class-metadata columns cls)))

;;; Get a list of column names for a class.
(define-syntax-rule (get-column-names cls)
  (map second (get-class-metadata columns cls)))
         
;;; Get the column name for a column field in a class.
(define-syntax-rule (get-column-name f cls)
  (let ([col-def (findf (lambda (c) (eq? f (first c))) (get-class-metadata columns cls))])
    (if col-def (second col-def) (error (format "column name for id ~a class ~a not found" f cls)))))
         
;;; Get the column id for a column name in a class.
(define-syntax-rule (get-column-id col-nm cls)
  (let ([col-def (findf (lambda (c) (equal? col-nm (second c))) (get-class-metadata columns cls))])
    (if col-def (first col-def) (error (format "column id for name ~a class ~a not found" col-nm cls)))))

;;; Get a join definition.
(define-syntax-rule (get-join-definition jn-fld cls)
  (findf (lambda (f) (eq? 'jn-fld (first f))) (get-class-metadata joins cls)))
