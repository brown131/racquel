#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; metadata - Data class metadata 
;;;;
;;;; Copyright (c) Scott Brown 2013

(provide data-class-metadata% *data-class-metadata* 
         get-class-metadata set-class-metadata! get-object-metadata set-object-metadata!
         dynamic-get-class-metadata dynamic-set-class-metadata! dynamic-get-object-metadata dynamic-set-object-metadata!
         data-class-info)

;;; Define data class metadata struct.
(define data-class-metadata% 
  (class object% 
    (field (table-name #f) (column-names null) (primary-key #f) (auto-increment-key #f) (external-name #f) (class-name #f))
    (super-new)
    (inspect #f)))

;;; Define a global table holding data class metadata.
(define *data-class-metadata* (make-hash))

;;; Get a data class metadata field.
(define-syntax-rule (get-class-metadata id cls)
  (get-field id (hash-ref *data-class-metadata* cls)))

;;; Set a data class metadata field.
(define-syntax-rule (set-class-metadata! id cls val)
  (set-field! id (hash-ref *data-class-metadata* cls) val))

;;; Get a data class metadata field for an object.
(define-syntax-rule (get-object-metadata id obj)
  (let-values ([(cls x) (object-info obj)]) (get-class-metadata id cls)))

;;; Get a data class metadata field for an object.
(define-syntax-rule (set-object-metadata! id obj val)
  (let-values ([(cls x) (object-info obj)]) (set-class-metadata! id cls val)))

;;; Dynamically get a data class metadata field.
(define-syntax-rule (dynamic-get-class-metadata id cls)
  (dynamic-get-field id (hash-ref *data-class-metadata* cls)))

;;; Dynamically set a data class metadata field.
(define-syntax-rule (dynamic-set-class-metadata! id cls val)
  (dynamic-set-field! id (hash-ref *data-class-metadata* cls) val))

;;; Dynamically get a data class metadata field for an object.
(define-syntax-rule (dynamic-get-object-metadata id obj)
  (let-values ([(cls x) (object-info obj)]) (dynamic-get-class-metadata id cls)))

;;; Dynamically set a data class metadata field for an object.
(define-syntax-rule (dynamic-set-object-metadata! id obj val)
  (let-values ([(cls x) (object-info obj)]) (dynamic-set-class-metadata! id cls val)))
 
;;; Return info about a data class.
(define-syntax-rule (data-class-info cls)
  (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info data-class-metadata%)])
    (apply values (map (lambda (f) (dynamic-get-class-metadata f cls)) fld-nms))))
