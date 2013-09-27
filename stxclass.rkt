#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse 
         (for-template "keywords.rkt" "metadata.rkt"  racket))

(provide data-class-element)

(define-syntax-class init-column-def
  #:description "init column definition" 
  ;(pattern (col:id nm:str) #:with expr #'col)
  ;(pattern ((col:id xcol:id) val:expr nm:str) #:with expr #'((icol xcol)))
  (pattern (col:id val:expr nm:str)
           #:with expr #'(col (let ([fld (get-field column-names m)]) 
                                (set-field! column-names m (append fld (list nm))) val)))
  (pattern ((icol:id xcol:id) val:expr nm:str)
           #:with expr #'((icol xcol) (let ([fld (get-field column-names m)]) 
                                        (set-field! column-names m (append fld (list nm))) val))))

(define-syntax-class column-def
  #:description "column definition" 
  (pattern (col:id val:expr nm:str) 
           #:with expr #'(col (let ([fld (get-field column-names m)]) 
                                (set-field! column-names m (append fld (list nm))) val)))
  (pattern ((icol:id xcol:id) val:expr nm:str) 
           #:with expr #'((icol xcol) (let ([fld (get-field column-names m)]) 
                                        (set-field! column-names m (append fld (list nm))) val))))
(define-syntax-class join-def
  #:description "join definition"
  (pattern (col:id fk:str jcls:id jk:str)
           #:with expr #'(let ([fld (get-field joins m)])
                           (set-field! joins m (append fld (list (make-object data-join% col fk jcls jk)))))))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name external-name init-column column join primary-key)
  (pattern (table-name tbl-nm:str) #:with expr #'(set-field! table-name m tbl-nm))
  (pattern (external-name ext-nm:str) #:with expr #'(set-field! external-name m ext-nm))
  (pattern (init-column col-def:init-column-def ...) #:with expr #'(init-field col-def.expr ...))
  (pattern (column col-def:column-def ...) #:with expr #'(field col-def.expr ...))
  (pattern (join j-def:join-def ...) #:with expr #'(begin j-def.expr ...))
  (pattern (primary-key pkey:expr #:autoincrement flag:boolean) #:with expr 
           #'(begin (set-field! primary-key m pkey) (when flag (set-field! autoincrement-key m pkey))))
  (pattern (primary-key pkey:expr) #:with expr #'(set-field! primary-key m pkey))
  (pattern (x:expr ...) #:with expr #'(x ...)))
