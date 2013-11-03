#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse 
         (for-template "keywords.rkt" "metadata.rkt" racket))

(provide data-class-element)

(define-syntax-class init-column-def
  #:description "init column definition"
  (pattern (col:id col-nm:str) 
           #:with expr #'col
           #:attr col-def #'(list 'col col-nm col-nm))
  (pattern ((icol:id xcol:id) col-nm:str) 
           #:with expr #'((icol xcol)) 
           #:attr col-def #'(list 'xcol col-nm col-nm))
  (pattern (col:id val:expr col-nm:str) 
           #:with expr #'(col val) 
           #:attr col-def #'(list 'col col-nm col-nm))
  (pattern ((icol:id xcol:id) val:expr col-nm:str) 
           #:with expr #'((icol xcol) val) 
           #:attr col-def #'(list 'xcol col-nm col-nm))
  (pattern (col:id (col-nm:str ext-nm:str)) 
           #:with expr #'col
           #:attr col-def #'(list 'col col-nm ext-nm))
  (pattern ((icol:id xcol:id) (col-nm:str ext-nm:str)) 
           #:with expr #'((icol xcol)) 
           #:attr col-def #'(list 'xcol col-nm ext-nm))
  (pattern (col:id val:expr (col-nm:str ext-nm:str)) 
           #:with expr #'(col val) 
           #:attr col-def #'(list 'col col-nm ext-nm))
  (pattern ((icol:id xcol:id) val:expr (col-nm:str ext-nm:str)) 
           #:with expr #'((icol xcol) val) 
           #:attr col-def #'(list 'xcol col-nm ext-nm)))

(define-syntax-class column-def
  #:description "column definition" 
  (pattern (col:id val:expr col-nm:str) 
           #:with expr #'(col val) 
           #:attr col-def #'(list 'col col-nm col-nm))
  (pattern ((icol:id xcol:id) val:expr col-nm:str) 
           #:with expr #'((icol xcol) val) 
           #:attr col-def #'(list 'xcol col-nm col-nm))
  (pattern (col:id val:expr (col-nm:str ext-nm:str))
           #:with expr #'(col val) 
           #:attr col-def #'(list 'col col-nm ext-nm))
  (pattern ((icol:id xcol:id) val:expr (col-nm:str ext-nm:str)) 
           #:with expr #'((icol xcol) val) 
           #:attr col-def #'(list 'xcol col-nm ext-nm)))

(define-syntax-class join-def
  #:description "join definition"
  (pattern (jcol:id fk:expr jcls:expr jk:expr) 
           #:with expr #'(jcol #f) 
           #:attr j-def #'(list 'jcol (data-join 'fk jcls 'jk 'one-to-many)))
  (pattern (jcol:id fk:expr jcls:expr jk:expr card:expr) 
           #:with expr #'(jcol #f) 
           #:attr j-def #'(list 'jcol (data-join 'fk jcls 'jk card))))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name init-column column join primary-key)
  #:attributes (expr col-defs jn-defs)
  (pattern (table-name tbl-nm:str) 
           #:with expr #'(begin (set-field! table-name m tbl-nm) (set-field! external-name m tbl-nm))
           #:attr col-defs #'null 
           #:attr jn-defs #'null)
  (pattern (table-name tbl-nm:str extern-nm:str) 
           #:with expr #'(begin (set-field! table-name m tbl-nm) (set-field! external-name m extern-nm))
           #:attr col-defs #'null 
           #:attr jn-defs #'null)
  (pattern (init-column cl-def:init-column-def ...) 
           #:with expr #'(init-field cl-def.expr ...)
           #:attr col-defs #'(list cl-def.col-def ...)
           #:attr jn-defs #'null)
  (pattern (column cl-def:column-def ...) 
           #:with expr #'(field cl-def.expr ...) 
           #:attr col-defs #'(list cl-def.col-def ...) 
           #:attr jn-defs #'null)
  (pattern (join jn-def:join-def ...) 
           #:with expr #'(field jn-def.expr ...) 
           #:attr col-defs #'null 
           #:attr jn-defs #'(list jn-def.j-def ...)) 
  (pattern (primary-key pkey:id #:autoincrement flag:boolean) 
           #:with expr #'(begin (set-field! primary-key m 'pkey) (when flag (set-field! autoincrement-key m 'pkey))) 
           #:attr col-defs #'null 
           #:attr jn-defs #'null)
  (pattern (primary-key pkey:id) 
           #:with expr #'(set-field! primary-key m 'pkey) 
           #:attr col-defs #'null 
           #:attr jn-defs #'null)
  (pattern (x:expr ...)            
           #:with expr #'(x ...) 
           #:attr col-defs #'null 
           #:attr jn-defs #'null))
