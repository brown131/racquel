#lang racket

(require syntax/parse 
         (for-template "keywords.rkt" racket))

(provide data-class-element)

(define-syntax-class column-def
  #:description "column definition" 
(pattern (id:id val:expr nm:str) #:with expr #'(id val nm)))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name external-name column)
  (pattern (table-name tbl-nm:str) #:with expr #'(table-name tbl-nm))
  (pattern (external-name ext-nm:str) #:with expr #'(external-name ext-nm))
  (pattern (column col-def:column-def ...) #:with expr #'(column col-def.expr ...)))
