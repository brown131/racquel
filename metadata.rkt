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

;;; Define data class metadata class.
(define data-class-metadata% 
  (class object% 
    (field [class #f]
           [class-id-key (generate-member-key)]
           [state-key (generate-member-key)]
           [table-name #f] 
           [columns null] 
           [joins null]
           [primary-key #f] 
           [autoincrement-key #f]
           [external-name #f])
    (super-new)
    (inspect #f)))

;;; Get metadata for a class.
(define (get-class-metadata-object cls)
  (if (class? cls)
      (let ([md (findf (lambda (v) (eq? (get-field class v) cls)) (hash-values *data-class-metadata*))])
        (if md md
          (let ([md-pair (findf (lambda (p) (if (get-field class (cdr p)) #f                                        
                                                (eval-syntax #`(with-handlers ([exn:fail? (lambda (e) #f)])
                                                                 (define-member-name #,(car p) (get-field class-id-key #,(cdr p)))
                                                                 (class-field-accessor #,cls #,(car p)))))) (hash->list *data-class-metadata*))])
            (if md-pair (begin (set-field! class (cdr md-pair) cls) (cdr md-pair)) #f))))
      (raise-argument-error 'get-class-metadata-object "argument ~a is not a class" cls)))

;;; Get a class from the metadata by name or symbol.
(define-syntax-rule (get-class cls-name)
  (findf (lambda (v) (let ([cls (get-field class v)])
                       (if cls (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)])
                                 (eq? cls-nm (if (string? cls-name) (string->symbol cls-name) cls-name))) #f))) (hash-values *data-class-metadata*)))

;;; Get a class from the metadata by name or symbol.
(define-syntax-rule (get-class-name cls)
    (if (class? cls) (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)]) cls-nm)
      (raise-argument-error 'get-class-name "argument ~a is not a class" cls)))

;;; Get a data class metadata field.
(define-syntax-rule (get-class-metadata fld cls)
  (if (class? cls) (get-field fld (get-class-metadata-object cls)) 
      (error 'get-class-metadata "argument ~a is not a class. field: ~a" cls 'fld)))

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
  (if (class? cls) (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info data-class-metadata%)])
                     (apply values (map (lambda (f) (dynamic-get-class-metadata f cls)) fld-nms)))
      (raise-argument-error 'data-class-info "argument ~a is not a class" cls)))

;;; Get a list of column ids for a class.
(define-syntax-rule (get-column-ids cls)
  (if (class? cls) (map first (get-class-metadata columns cls))
      (raise-argument-error 'get-column-ids "argument ~a is not a class" cls)))

;;; Get a list of column names for a class.
(define-syntax-rule (get-column-names cls)
  (if (class? cls) (map second (get-class-metadata columns cls))
      (raise-argument-error 'get-column-names "argument ~a is not a class" cls)))
    
;;; Get the column name for a column field in a class.
(define-syntax-rule (get-column-name f cls)
  (if (class? cls) (let ([col-def (findf (lambda (c) (eq? f (first c))) (get-class-metadata columns cls))])
                     (if col-def (second col-def) (error (format "column name for id ~a class ~a not found" f cls))))
      (raise-argument-error 'get-column-name "argument ~a is not a class" cls)))
         
;;; Get the column id for a column name in a class.
(define-syntax-rule (get-column-id col-nm cls)
    (if (class? cls) (let ([col-def (findf (lambda (c) (equal? col-nm (second c))) (get-class-metadata columns cls))])
                       (if col-def (first col-def) (error (format "column id for name ~a class ~a not found" col-nm cls))))
      (raise-argument-error 'get-column-id "argument ~a is not a class" cls)))

;;; Get a join definition.
(define-syntax-rule (get-join-definition jn-fld cls)
  (if (class? cls) (findf (lambda (f) (eq? 'jn-fld (first f))) (get-class-metadata joins cls))
      (raise-argument-error 'get-join-definition "argument ~a is not a class" cls)))

