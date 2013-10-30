#lang racket
;;;;
;;;; mixin - Provides mixins for a data class.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require json xml xml/xexpr "metadata.rkt")

(provide json-data-class-mixin xml-data-class-mixin)

;;; Mix-in to serialize a data class using JSON.
(define-syntax-rule (json-data-class-mixin cls)
  (if (implementation? cls data-class<%>)
      (class* cls (externalizable<%>)
        (define/public (externalize)
          (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info this%)]
                       [(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info this%)])
            (jsexpr->string 
             (hasheq (string->symbol ext-nm) (make-hasheq (map (lambda (f) (cons f (dynamic-get-field f this))) fld-nms))))
            ))
        (define/public (internalize str)
          (let* ([jsx (string->jsexpr str)]
                 [cols (first (hash-values jsx))])
            (map (lambda (k) (dynamic-set-field! k this (hash-ref cols k))) (hash-keys cols))
            ))
        (inspect #f)
        (super-new))
      (error "json-data-class-mixin: not a data-class<%> class")))

;;; Mix-in to serialize a data class using XML.
(define-syntax-rule (xml-data-class-mixin cls)
  (if (implementation? cls data-class<%>)
      (class* cls (externalizable<%>)
        (define/public (externalize)
          (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info this%)]
                       [(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info this%)])
            (xexpr->string 
             (append (list (string->symbol ext-nm) '()) 
                     (map (lambda (f) (list f '() (~a (dynamic-get-field f this)))) fld-nms)))
            ))
        (define/public (internalize str)
          (let* ([xmlx (string->xexpr str)])
            (set-class-metadata! external-name this% (symbol->string (first xmlx)))
            (map (lambda (i) (when (and (list? i) (eq? (length i) 2))
                               (dynamic-set-field! (first i) this (third i)))) xmlx)
            ))
        (inspect #f)
        (super-new))
      (error "json-data-class-mixin: not a data-class<%> class")))
