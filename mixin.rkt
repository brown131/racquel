#lang racket
;;;;
;;;; mixin - Provides mixins for a data class.
;;;;
;;;; Copyright (c) Scott Brown 2013

(require json xml xml/xexpr "metadata.rkt" "util.rkt")

(provide json-data-class-mixin xml-data-class-mixin)

;;; Mix-in to serialize a data class using JSON.
(define-syntax-rule (json-data-class-mixin cls)
  (if (data-class? cls)
      (class* cls (externalizable<%>)
        (define/public (externalize)
            (jsexpr->string 
             (hasheq (string->symbol (get-class-metadata external-name this%)) 
                     (make-hasheq (map (lambda (d) (cons (string->symbol (third d)) (dynamic-get-field (first d) this))) 
                                       (get-class-metadata columns this%))))))
        (define/public (internalize str)
          (let* ([jsx (string->jsexpr str)]
                 [cols (first (hash-values jsx))]
                 [col-defs (get-class-metadata columns this%)])
            (hash-map cols (lambda (k v) (dynamic-set-field! (first (findf (lambda (c) (equal? (symbol->string k) (third c))) 
                                                                         col-defs)) this v)))))
        (inspect #f)
        (super-new))
      (error "json-data-class-mixin: not a data-class<%> class")))

;;; Mix-in to serialize a data class using XML.
(define-syntax-rule (xml-data-class-mixin cls)
  (if (data-class? cls)
      (class* cls (externalizable<%>)
        (define/public (externalize)
          (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info this%)]
                       [(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info this%)])
            (xexpr->string 
             (append (list (string->symbol ext-nm) '()) 
                     (map (lambda (d) (list (string->symbol (third d)) '() 
                                            (~a (dynamic-get-field (first d) this)))) (get-class-metadata columns this%))))
            ))
        (define/public (internalize str)
          (let* ([xmlx (string->xexpr str)]
                 [vals (make-list (length xmlx) #f)]
                 [col-defs (get-class-metadata columns this%)])
            (map (lambda (x) (dynamic-set-field! (first (findf (lambda (c) (equal? (symbol->string (first x)) (third c))) col-defs)) this (third x)))
                 (filter (lambda (x) (and (list? x) (> (length x) 1))) xmlx))))
        (inspect #f)
        (super-new))
      (error "xml-data-class-mixin: not a data-class<%> class")))
