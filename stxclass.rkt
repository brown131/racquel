#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; stxclass - Defines syntax classes
;;;;
;;;; Copyright (c) Scott Brown 2013
;;;;
;;;; This file is part of Racquel
;;;;
;;;; Racquel is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require syntax/parse
         (for-template db "keywords.rkt" "metadata.rkt" "util.rkt" racket))

(provide (all-defined-out))


;;;; RQL SYNTAX CLASSES


#| Model:
(select-data-object con address%
                    (left-join state% (= abbr (address% state)))
                    (where (and (like (person last-name) "A%")
                                (or (= (id ?)
                                       (not (in city ("Chicago" "New York"))))
                                    (between zip-code 10000 60999)
                                    (in state (select state-abbr state% 
                                                      (where (= country "US")))))))
                    (get-field id obj))
|#

;;; Parse an RQL expression.
(define-syntax-class rql-expr
  #:description "rql expression"
  #:literals (and or not = <> >= <= > < like in between)
  (pattern (and p:rql-expr ...) #:with expr #'(rql-and p.expr ...))
  (pattern (or p:rql-expr ...) #:with expr #'(rql-or p.expr ...))
  (pattern (not p:rql-expr) #:with expr #'(rql-not p.expr))
  (pattern (= a:rql-expr b:rql-expr) #:with expr #'(rql-= a.expr b.expr))
  (pattern (<> a:rql-expr b:rql-expr) #:with expr #'(rql-<> a.expr b.expr))
  (pattern (>= a:rql-expr b:rql-expr) #:with expr #'(rql->= a.expr b.expr))
  (pattern (<= a:rql-expr b:rql-expr) #:with expr #'(rql-<= a.expr b.expr))
  (pattern (> a:rql-expr b:rql-expr) #:with expr #'(rql-> a.expr b.expr))
  (pattern (< a:rql-expr b:rql-expr) #:with expr #'(rql-< a.expr b.expr))
  (pattern (like a:rql-expr b:rql-expr) #:with expr #'(rql-like a.expr b.expr))
  (pattern (in a:rql-expr b:expr) #:with expr #'(rql-in a.expr b))
  (pattern (between a:rql-expr b:rql-expr c:rql-expr) #:with expr 
           #'(rql-between a.expr b.expr c.expr))
  (pattern i:id #:with expr #'(get-column-name-from-context 'i ctxt))
  (pattern s:str #:with expr #'s)
  (pattern n:nat #:with expr #'(~a n))
  (pattern (p1:expr p2:expr) #:with expr #'(rql-column-pair p1 'p2 ctxt)))

(define-syntax-class join-expr 
  #:description "rql join expression"
  #:literals (join left-join right-join)
  (pattern (join table:id rql:rql-expr) #:with expr 
           #'(string-append "join " (rql-table-name 'table ctxt) " on " rql.expr " "))
  (pattern (left-join table:id rql:rql-expr) #:with expr 
           #'(string-append "left outer join " (rql-table-name 'table ctxt) " on " rql.expr " "))
  (pattern (right-join table:id rql:rql-expr) #:with expr 
           #'(string-append "right outer join " (rql-table-name 'table ctxt) " on " rql.expr " ")))

(define-syntax-class where-expr 
  #:description "rql where expression"
  #:literals (where)
  (pattern (where rql:rql-expr) #:with expr #'(string-append "where " rql.expr)))


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
  (pattern (jcol:id jcls:expr 
                    (~optional (~seq #:cardinality card:expr) 
                               #:defaults ([card #''one-to-many])) where:where-expr rest:expr ...) 
           #:with expr #'(jcol #f) 
           #:attr j-row #'((eq? jn-fld 'jcol) 
                           (query-rows con (make-select-statement con jn-cls where.expr) 
                                       rest ...))
           #:attr j-def #'(list 'jcol jcls card)))

(define-syntax-class data-class-element
  #:description "data class element" 
  #:literals (table-name init-column column join primary-key)
  #:attributes (cls-expr meta-expr col-defs jn-rows jn-defs)
  (pattern (table-name tbl-nm:str) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-tbl-nm-m-data! tbl-nm tbl-nm)
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (table-name tbl-nm:str extern-nm:str) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-tbl-nm-m-data! tbl-nm extern-nm)
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
  (pattern (primary-key pkey:id #:autoincrement flag:expr) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-auto-pkey! 'pkey flag)
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (primary-key pkey:expr #:autoincrement flag:expr) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-auto-pkey! pkey flag)
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (primary-key pkey:id) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-pkey! 'pkey)
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (primary-key pkey:expr) 
           #:attr cls-expr #'#f
           #:attr meta-expr #'(set-pkey! pkey) 
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null)
  (pattern (x:expr ...)            
           #:attr cls-expr #'(x ...)
           #:attr meta-expr #'#f
           #:attr col-defs #'null 
           #:attr jn-rows #'null 
           #:attr jn-defs #'null))
