#lang racket
;;;;
;;;; mixin - Provides mixins for a data class.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require json "metadata.rkt" (only-in racquel set-data-object!))

(provide json-data-class-mixin)

(define-syntax-rule (json-data-class-mixin cls)
  ;(unless (implementation? cls data-class<%>)
  ;  (error "json-data-class-mixin: not a data-class<%> class"))
  (class* cls (externalizable<%>)
    (define/public (externalize)
      (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info this%)]
                   [(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info this%)])
        (jsexpr->string 
         (hasheq cls-nm (make-hasheq (map (lambda (f) (cons f (dynamic-get-field f this))) fld-nms))))
        ))
    (define/public (internalize str)
      (let* ([jsx (string->jsexpr str)]
             [row (list->vector (hash-values (first (hash-values jsx))))])
        (set-data-object! this row)
        ))
    (inspect #f)
    (super-new)))
