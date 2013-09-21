#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse 
         (for-template "keywords.rkt" racket))

(provide data-class-element)

(define-syntax-class column-def
  #:description "column definition" 
  (pattern (id:id val:expr nm:str) 
           #:with expr #'(id (let ([fld (get-field column-names m)]) 
                               (set-field! column-names m (append fld (list nm))) val))))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name external-name column primary-key)
  (pattern (table-name tbl-nm:str) #:with expr #'(set-field! table-name m tbl-nm))
  (pattern (external-name ext-nm:str) #:with expr #'(set-field! external-name m ext-nm))
  (pattern (column col-def:column-def ...) #:with expr #'(field col-def.expr ...))
  (pattern (primary-key pkey:expr) #:with expr #'(set-field! primary-key m pkey))
  (pattern (x:expr ...) #:with expr #'(x ...))
  )
