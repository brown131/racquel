#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse 
         (for-template "keywords.rkt" racket))

(provide data-class-element)
#|
(define-syntax-class init-column-def
  #:description "init column definition" 
  (pattern (id:id val:expr nm:str) 
           #:with expr #'(init-field id (let ([fld (get-field column-names m)]) 
                               (set-field! column-names m (append fld (list nm)))))))
|#
(define-syntax-class column-def
  #:description "column definition" 
  (pattern (id:id val:expr nm:str) 
           #:with expr #'(id (let ([fld (get-field column-names m)]) 
                               (set-field! column-names m (append fld (list nm))) val)))
  (pattern ((iid:id xid:id) val:expr nm:str) 
           #:with expr #'((iid xid) (let ([fld (get-field column-names m)]) 
                               (set-field! column-names m (append fld (list nm))) val))))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name external-name column primary-key)
  (pattern (table-name tbl-nm:str) #:with expr #'(set-field! table-name m tbl-nm))
  (pattern (external-name ext-nm:str) #:with expr #'(set-field! external-name m ext-nm))
 ; (pattern (init-column col-def:init-column-def ...) #:with expr #'(col-def.expr ...))
  (pattern (column col-def:column-def ...) #:with expr #'(field col-def.expr ...))
  (pattern (primary-key pkey:expr #:autoincrement flag:boolean) #:with expr 
           #'(begin (set-field! primary-key m pkey) (when flag (set-field! autoincrement-key m pkey))))
  (pattern (primary-key pkey:expr) #:with expr #'(set-field! primary-key m pkey))
  (pattern (x:expr ...) #:with expr #'(x ...))
  )
