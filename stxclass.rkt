#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013

(require syntax/parse
         (for-template db "keywords.rkt" "metadata.rkt" "schema.rkt" "util.rkt" racket))

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
  (pattern (p1:expr p2:expr) #:with (expr ...) #'((rql-column 'p1 'p2)))
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

(define-syntax-class join-def
  #:description "join definition"
  (pattern (jcol:id jcls:expr where-expr:where-expr rest:expr ...) 
           #:with expr #'(jcol #f) 
           #:attr j-col #'jcol
           #:attr j-def #'(list 'jcol (data-join jcls 'one-to-many 
                  (lambda (con) 
                   (let* ([dbsys-type (dbsystem-name (connection-dbsystem con))]
                          [col-nms (sort (get-column-names jcls) string<?)]
                          [rows (query-rows con (string-append "select " (string-join col-nms ", ")
                                                               " from " (get-class-metadata table-name jcls) " t "
                                                               (sql-placeholder where-expr dbsys-type)) rest ...)]
                          [objs (make-list (length rows) (new jcls))])
                     (map (lambda (o r) (map (lambda (f v) (dynamic-set-field! f o v)) 
                                             (get-column-ids (object-class o)) (vector->list r))
                            (define-member-name data-object-state (get-class-metadata state-key (object-class o)))
                            (set-field! data-object-state o 'loaded)) objs rows)
                     objs))))))

#|  (pattern (jcol:id jcls:id (~optional (~seq #:cardinality card:expr)) where-expr:expr rest:expr ...) 
           #:with expr #'(jcol #f) 
           #:attr j-col #'jcol
           #:attr j-def #'(list 'jcol (data-join jcls card (lambda (con) (set-data-object! con con cls where-expr rest ...))))))|#

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
