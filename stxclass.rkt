#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse 
         (for-template "keywords.rkt" "metadata.rkt" racket))

(provide data-class-element *column-names*)

(define *column-names* null)

(define-syntax-class init-column-def
  #:description "init column definition"
  (pattern (col:id nm:str) #:with expr #'col #:attr col-nm #'nm)
  (pattern ((col:id xcol:id) val:expr nm:str) #:with expr #'((icol xcol)) #:attr col-nm #'nm)
  (pattern (col:id val:expr nm:str) #:with expr #'(col val) #:attr col-nm #'nm)
  (pattern ((icol:id xcol:id) val:expr nm:str) #:with expr #'((icol xcol) val) #:attr col-nm #'nm))

(define-syntax-class column-def
  #:description "column definition" 
  (pattern (col:id val:expr nm:str) #:with expr #'(col val) #:attr col-nm #'nm)
  (pattern ((icol:id xcol:id) val:expr nm:str) #:with expr #'((icol xcol) val) #:attr col-nm #'nm))

(define-syntax-class join-def
  #:description "join definition"
  (pattern (col:id fk:str jcls:id jk:str) #:with expr #'(cons col (data-join fk jcls jk))))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name external-name init-column column join primary-key)
  #:attributes (expr col-nms)
  (pattern (table-name tbl-nm:str) #:with expr #'(set-field! table-name m tbl-nm) #:attr col-nms #'null)
  (pattern (external-name ext-nm:str) #:with expr #'(set-field! external-name m ext-nm) #:attr col-nms #'null)
  (pattern (init-column col-def:init-column-def ...) #:with expr #'(init-field col-def.expr ...)
           #:attr col-nms #'(list col-def.col-nm ...))
  (pattern (column col-def:column-def ...) #:with expr #'(field col-def.expr ...) 
           #:attr col-nms #'(list col-def.col-nm ...))
  ; TODO: Add to existing hash
  (pattern (join jn-def:join-def ...) 
           #:with expr #'(set-field! joins m (make-hash (list jn-def.expr ...))) #:attr col-nms #'null)
  (pattern (primary-key pkey:expr #:autoincrement flag:boolean) #:with expr 
           #'(begin (set-field! primary-key m pkey) (when flag (set-field! autoincrement-key m pkey))) #:attr col-nms #'null)
  (pattern (primary-key pkey:expr) #:with expr #'(set-field! primary-key m pkey) #:attr col-nms #'null)
  (pattern (x:expr ...) #:with expr #'(x ...) #:attr col-nms #'null))
