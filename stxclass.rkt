#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse
         (for-template db "keywords.rkt" "metadata.rkt" "util.rkt" racket))

(provide (all-defined-out))


;;;; RQL SYNTAX CLASSES


#| Model:
(select (address% a)
        (left-join state% (= abbr (a state)))
        (and (like (person last-name) "A%")
             (or (= (id #,(get-field id obj))
                 (not (in city ("Chicago" "New York"))))
             (between zip-code 10000 60999)))
|#

;;; Parse an RQL expression.
(define-syntax-class rql-expr
  #:description "rql expression"
  #:literals (join where and or not = <> >= <= > < like in unquote)
  (pattern and #:with (expr ...) #'(rql-and))
  (pattern or #:with (expr ...) #'(rql-or))
  (pattern not #:with (expr ...) #'(rql-not))
  (pattern = #:with (expr ...) #'(rql-=))
  (pattern <> #:with (expr ...) #'(rql-<>))
  (pattern >= #:with (expr ...) #'(rql->=))
  (pattern <= #:with (expr ...) #'(rql-<=))
  (pattern > #:with (expr ...) #'(rql->))
  (pattern < #:with (expr ...) #'(rql-<))
  (pattern like #:with (expr ...) #'(rql-like))
  (pattern in #:with (expr ...) #'(rql-in))
  (pattern i:id #:with (expr ...) #'('i))
  (pattern s:str #:with (expr ...) #'(s))
  (pattern n:nat #:with (expr ...) #'(n))
  (pattern (unquote x:expr) #:with (expr ...) #'((rql-unquote x)))
  (pattern (p1:expr p2:expr) #:with (expr ...) #'((rql-column-pair 'p1 'p2)))
  (pattern l:rql-expr-list #:with (expr ...) #'((l.expr ...))))

(define-syntax-class rql-expr-list
  #:description "rql expression list"
  (pattern (rql:rql-expr ...) #:with (expr ...) #'(rql.expr ... ...)))

(define-syntax-class join-expr 
  #:description "rql join expression"
  (pattern (join table:id rql:rql-expr) #:with (expr ...) #'("join " (~a 'table) " on " rql.expr ... " ")))

(define-syntax-class where-expr 
  #:description "rql where expression"
  (pattern (where rql:rql-expr) #:with (expr ...) #'("where " rql.expr ...)))


;;;; DATA CLASS SYNTAX CLASSES


#| Model:
(data-class object% 
            (table-name "TST_Person" "Person") 
            (init-column (id "id"))
            (column (name #f "name")
                    (description #f "description")
                    (address-id #f "address_id"))
            (join (vehicles vehicle% (where (= person-id ?)) id)
                  (address 'address% (where (= id ?)) address-id)
            (primary-key id #:autoincrement #t)
            (field (data #f))
            (super-new)
            (inspect #f))
|#

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

#|
(define-syntax (test stx) (syntax-parse stx
    [(test i:id)
       (with-syntax ([get-join (datum->syntax stx (string->symbol (format "get-join-~a" (syntax->datum #'i))))])
         #'(define (get-join x) (+ x 2)))]))
|#

(define-syntax-class join-def
  #:description "join definition"
  (pattern (jcol:id jcls:expr (~optional (~seq #:cardinality card:expr) #:defaults ([card #''one-to-many])) where:where-expr rest:expr ...) 
           #:with expr #'(jcol #f) 
           #:attr j-row #'((eq? jn-fld 'jcol) (query-rows con (select-sql con jn-cls (string-append where.expr ...)) rest ...))
           #:attr j-def #'(list 'jcol jcls card)))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name init-column column join primary-key)
  #:attributes (cls-expr meta-expr col-defs jn-rows jn-defs)
  (pattern (table-name tbl-nm:str) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(begin (set-field! table-name m tbl-nm) (set-field! external-name m tbl-nm))
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (table-name tbl-nm:str extern-nm:str) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(begin (set-field! table-name m tbl-nm) (set-field! external-name m extern-nm))
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (init-column cl-def:init-column-def ...) 
           #:attr cls-expr #'(init-field cl-def.expr ...)
           #:attr meta-expr #'#f
           #:attr col-defs #'(list cl-def.col-def ...)
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (column cl-def:column-def ...) 
           #:attr cls-expr #'(field cl-def.expr ...) 
           #:attr meta-expr #'#f
           #:attr col-defs #'(list cl-def.col-def ...) 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (join jn-def:join-def ...) 
           #:attr cls-expr #'(field jn-def.expr ...)
           #:attr meta-expr #'#f
           #:attr col-defs #'null 
           #:attr jn-rows #'(cond jn-def.j-row ...)
           #:attr jn-defs #'(list jn-def.j-def ...))
  (pattern (primary-key pkey:id #:autoincrement flag:boolean) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(begin (set-field! primary-key m 'pkey) (when flag (set-field! autoincrement-key m 'pkey)))
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (primary-key pkey:id) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-field! primary-key m 'pkey) 
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (x:expr ...)            
           #:attr cls-expr #'(x ...) 
           #:attr meta-expr #'#f
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null))
