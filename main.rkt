#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; main - Main module for the project
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

(require db json xml "keywords.rkt" "metadata.rkt" "schema.rkt" "util.rkt"
         (for-syntax racket/syntax syntax/parse "stxclass.rkt"))


;;;; DATA CLASS DEFINITION
   

;;; Return the state of a data object.
(define (data-object-state obj)
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (get-field data-object-state obj))
       
;;; Primary key fields
(define (primary-key-fields cls)
  (let ([pkey (get-class-metadata primary-key cls)]) 
    (if (list? pkey) (sort pkey string<? #:key symbol->string) (list pkey))))
   
;;; Set auto-increment id.
(define (set-autoincrement-id! con obj)
  (let* ([cls (object-class obj)]
         [auto-key (get-class-metadata autoincrement-key cls)])
    (when auto-key
      (dynamic-set-field! (get-class-metadata primary-key cls) obj
                          (query-value con (sql-autoincrement (dbsystem-type con) auto-key))))))

;;; Get the primary key from an object.
(define (get-primary-key obj)
 (map (lambda (f) (dynamic-get-field f obj)) (primary-key-fields (object-class obj))))

;;; Auto-increment key fields
(define (autoincrement-key-fields cls)
  (if (get-class-metadata autoincrement-key cls) (primary-key-fields cls) null))
    
;;; Columns without the autoincrement key
(define (savable-fields con cls)
  (sort (foldl (lambda (f l) 
           (if (member f (autoincrement-key-fields cls)) l (cons f l))) null (get-column-ids cls)) 
        string<? #:key symbol->string))

;;; SQL where-clause for a key.
(define (key-where-clause-sql con cls key)
  (let ([dbsys-type (dbsystem-type con)])
        (string-append " where " 
                       (sql-placeholder (string-join 
                                         (map (lambda (f) (string-append (sql-escape 
                                                                          (get-column-name f cls)
                                                                          dbsys-type) "=?")) 
                                              (if (list? key) key (list key))) " and ") 
                                        dbsys-type))))

;;; Insert SQL.
(define (insert-sql con cls)
  (let* ([dbsys-type (dbsystem-type con)]
         [col-nms (map (lambda (f) (sql-escape (get-column-name f cls) dbsys-type)) 
                       (savable-fields con cls))])
    (string-append "insert into " (sql-escape (get-class-metadata table-name cls) dbsys-type)
                   " (" (string-join col-nms ", ") ")"
                   " values (" (sql-placeholder (string-join (make-list (length col-nms) "?") ", ") 
                                                dbsys-type) ")")))

;;; Update SQL.
(define (update-sql con cls)
  (let* ([dbsys-type (dbsystem-type con)]
         [key (primary-key-fields cls)]
         [values (sort (foldr (lambda (f l) (cons (string-append (sql-escape (get-column-name f cls)
                                                                             dbsys-type) "=?") l)) 
                              null (savable-fields con cls)) string<?)])
    (sql-placeholder 
     (string-append "update " (sql-escape (get-class-metadata table-name cls) dbsys-type)
                    " set " (string-join values ", ")
                    " where " (string-join 
                               (map (lambda (f) (string-append (sql-escape (get-column-name f cls) 
                                                                           dbsys-type) "=?")) 
                                    (if (list? key) key (list key))) " and ")) 
     dbsys-type)))

;;; Delete SQL.
(define (delete-sql con cls)
  (string-append "delete from " (sql-escape (get-class-metadata table-name cls) (dbsystem-type con))
                 (key-where-clause-sql con cls (primary-key-fields cls))))

;;; Define a data class with interfaces.
(define-syntax (data-class* stx)
  (syntax-parse stx 
    [(_ base-cls:id (i-face:id ...) elem:data-class-element ...) 
     (with-syntax ([cls-id (generate-temporary #'class-id-)]
                   [m-data (generate-temporary #'metadata-)])
       #'(let* ([ctxt null]
                [m-data (new data-class-metadata%)]
                [set-tbl-nm-m-data! (λ (tbl-nm extern-nm) (set-field! table-name m-data tbl-nm) 
                                      (set-field! external-name m-data extern-nm))]
                [set-auto-pkey! (λ (pkey flag) (set-field! primary-key m-data pkey) 
                                  (when flag (set-field! autoincrement-key m-data flag)))]
                [set-pkey! (λ (pkey) (set-field! primary-key m-data pkey))])
           (unless (hash-has-key? *data-class-metadata* 'cls-id)
             elem.meta-expr ...
             (set-field! columns m-data (sort (append elem.col-defs ...) string<? 
                                         #:key (lambda (k) (symbol->string (first k)))))
             (set-field! joins m-data (append elem.jn-defs ...))
             (hash-set! *data-class-metadata* 'cls-id m-data))
           (define-member-name cls-id (get-field class-id-key m-data))
           (define-member-name data-object-state (get-field state-key m-data))
           (class* base-cls (data-class<%> i-face ...) 
             elem.cls-expr ...
             (field [cls-id #f]
                    [data-object-state 'new])
             (inspect #f)
             (define/public (set-data-join! con jn-fld jn-cls)
               (let* ([rows (append elem.jn-rows ...)])
                 (map (lambda (r) (let ([obj (new jn-cls)])
                                    (map (lambda (f v) (dynamic-set-field! f obj v)) 
                                         (get-column-ids jn-cls) (vector->list r))
                                    (define-member-name data-object-state 
                                      (get-class-metadata state-key jn-cls))
                                    (set-field! data-object-state obj 'loaded)
                                    obj)) rows)))
             (define/private (base-data-class cls)
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info cls)])
                 (if (data-class? cls) (if sup-cls (base-data-class sup-cls) cls) cls)))            
             (unless (get-field class (hash-ref *data-class-metadata* 'cls-id))
               (set-field! class (hash-ref *data-class-metadata* 'cls-id) 
                           (base-data-class this%))))))]))

;;; Define a data class.
(define-syntax-rule (data-class base-cls elem ...) (data-class* base-cls (data-class<%>) elem ...))

;;; Get a data column.
(define-syntax-rule (get-column col obj) (get-field col obj))

;;; Set a data column.
(define-syntax-rule (set-column! col obj val) (set-field! col obj val))

;;; Get joined data objects. This will select the objects on first use.
(define-syntax (get-join stx)
  (syntax-case stx ()
    ([_ jn-fld obj con] 
     #'(begin
         (when (eq? (get-field jn-fld obj) #f)
           (let* ([jn-def (get-join-definition jn-fld (object-class obj))]     
                  [jn-cls-expr (join-definition-class jn-def)]
                  [jn-cls (cond [(class? jn-cls-expr) jn-cls-expr] 
                                [(symbol? jn-cls-expr) (get-class jn-cls-expr)])])
             (set-field! jn-fld obj (send obj set-data-join! con 'jn-fld jn-cls))
             (when (and (equal? (join-definition-cardinality jn-def) 'one-to-one) 
                        (> (length (get-field jn-fld obj)) 0))
               (set-field! jn-fld obj (first (get-field jn-fld obj))))))
         (get-field jn-fld obj)))))


;;; DATA CLASS GENERATION


;;; Get columns from the schema.
(define (get-schema-columns schema col-nm-norm)
  (let ([col-nms (foldl (lambda (f l) (if (member (schema-column f) l) l 
                                          (cons (schema-column f) l))) null schema)])
    (sort (map (lambda (f) (list (string->symbol (col-nm-norm f)) #f f)) col-nms) 
          string<? #:key (lambda (k) (symbol->string (first k))))))

;;; RQL where-clause syntax for a key.
(define (key-where-clause-rql jn-cls jn-columns tbl-nm-norm col-nm-norm)
  (let ([jns (map (lambda (j) #`(= ('#,(string->symbol (tbl-nm-norm jn-cls)) 
                                    #,(string->symbol (col-nm-norm j))) ?)) jn-columns)])
    (if (eq? (length jn-columns) 1) #`(where #,@jns) #`(where (and #,@jns)))))

;;; Get the cardinality of a join based on schema metadata.
(define (join-cardinality con schema-nm dbsys-type jn-tbl-nm jn-key) 
  (let* ([jn-schema (load-schema con schema-nm jn-tbl-nm #:db-system-type dbsys-type)]
         [row (findf (lambda (r) (equal? (schema-column r) jn-key)) jn-schema)]
         [spk (eq? (count (λ (r) (equal? (schema-constraint-type r) "P")) jn-schema) 1)])
    (unless row (error 'join-cardinality "row not found for join table: ~a key: ~a" jn-tbl-nm jn-key))
    (if (and (equal? (schema-constraint-type row) "P") spk) #''one-to-one #''one-to-many)))

;;; Join information from the schema.
(define (get-join-schema schema)
  (foldl (lambda (r l) 
           (if (sql-null? (schema-join-table r)) l
               (let ([jn-def (findf (lambda (m) (string=? (schema-constraint r) (first m))) l)]) 
                 (if jn-def (cons (append (take jn-def 4) 
                                          (list (append (last jn-def) (list (schema-join-column r)))))
                                  (remove (first jn-def) l (lambda (a b) (string=? a (first b)))))
                     (cons (list (schema-constraint r)
                                 (schema-join-table r)
                                 (schema-join-column r) 
                                 (schema-column r) 
                                 (list (schema-join-column r))) l))))) null schema))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields schema col-nm-norm)
  (let ([pkey (map (lambda (v) (string->symbol (col-nm-norm (schema-column v))))
                   (filter (lambda (f) (if (string? (schema-constraint-type f)) 
                                           (string=? (schema-constraint-type f) "P") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))
    
;;; Get the autoincrement key from the table schema.
(define (get-autoincrement-key schema dbsys-type) 
  (let ([row (findf (lambda (f) (cond [(eq? dbsys-type 'postgresql) 
                                       (string? (schema-autoincrement f))]
                                      [(eq? dbsys-type 'oracle) (string? (schema-autoincrement f))]
                                      [else (eq? (schema-autoincrement f) 1)])) schema)])
    (if row (if (string? (schema-autoincrement row)) (schema-autoincrement row) #t) #f)))

;;; Get joins from the schema.
(define (get-schema-joins con schema-nm schema dbsys-type tbl-nm-norm join-nm-norm col-nm-norm)
  (map (lambda (j) 
         (list (string->symbol (join-nm-norm (second j) 
                                             (eval-syntax (join-cardinality con schema-nm dbsys-type 
                                                                            (second j) (third j))))) 
               #`'#,(string->symbol (tbl-nm-norm (second j)))
               #'#:cardinality (join-cardinality con schema-nm dbsys-type (second j) (third j)) 
               (key-where-clause-rql (second j) (last j) tbl-nm-norm col-nm-norm)
               (string->symbol (col-nm-norm (fourth j))))) (get-join-schema schema)))

;;; Default name normalizer. Replaces underscores and mixed case with hyphens. Returns all lower case.
(define mixed-case-norm-regexp (regexp "([a-z])([A-Z])"))
(define (column-name-normalizer s) 
  (string-downcase (string-replace (regexp-replace* mixed-case-norm-regexp s "\\1-\\2")  "_" "-")))
(define (table-name-normalizer s) 
  (string-append (column-name-normalizer s) "%"))
(define (join-name-normalizer s (c 'one-to-many)) 
  (string-append (column-name-normalizer s) 
                 (if (eq? c 'one-to-many) 
                     (if (string=? (substring s (- (string-length s) 1) (string-length s)) "s") "es" 
                         "s") "")))

;;; Generate a class using database schema information.
(define (gen-data-class con tbl-nm 
                        #:db-system-type (dbsys-type (dbsystem-type con))
                        #:generate-joins? (gen-joins? #t)
                        #:generate-reverse-joins? (gen-rev-joins? #t)
                        #:schema-name (schema-nm #f)
                        #:inherits (base-cls 'object%)
                        #:table-name-normalizer (tbl-nm-norm (lambda (n) (table-name-normalizer n))) 
                        #:column-name-normalizer (col-nm-norm (lambda (n) (column-name-normalizer n)))
                        #:join-name-normalizer (join-nm-norm (lambda (n (c 'one-to-many)) 
                                                               (join-name-normalizer n c))) 
                        #:table-name-externalizer (tbl-nm-extern (lambda (n) (begin n)))
                        #:print? (prnt? #f)
                        . rest) 
  (let* ([schema (load-schema con schema-nm tbl-nm #:reverse-join? gen-rev-joins? 
                              #:db-system-type dbsys-type)]
         [cls-nm (string->symbol (tbl-nm-norm tbl-nm))]
         [pkey (find-primary-key-fields schema col-nm-norm)]
         [jns (if (or gen-joins? gen-rev-joins?)
                  (get-schema-joins con schema-nm schema dbsys-type tbl-nm-norm join-nm-norm 
                                    col-nm-norm) null)]
         [auto-key (get-autoincrement-key schema dbsys-type)]
         [stx #`(let ([#,cls-nm
                       (data-class* #,base-cls (data-class<%>)
                                    (table-name #,tbl-nm #,(tbl-nm-extern tbl-nm))
                                    #,(append '(column) (get-schema-columns schema col-nm-norm))
                                    (primary-key '#,pkey #:autoincrement #,auto-key)
                                    #,(if (and gen-joins? (list? jns) (> (length jns) 0)) 
                                          (append '(join) jns) '(begin #f))
                                    (super-new)
                                    #,@rest)])
                  (get-class-metadata-object #,cls-nm)
                  #,cls-nm)])
    (if prnt? (syntax->datum stx) (eval-syntax stx racquel-namespace))))


;;;; PERSISTENCE


;;; Define a global hash holding data objects.
(define *data-objects* (make-multi-hash #:weak? #t))

;;; Set the data in a data object.
(define (set-data-object! obj row)
  (map (lambda (f v) (dynamic-set-field! f obj v)) 
       (get-column-ids (object-class obj)) (vector->list row))
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (set-field! data-object-state obj 'loaded))

;;; Create a primary key from a database row.
(define (create-primary-key cls row)
  (let* ([pkey-flds (primary-key-fields cls)]
         [pkey (foldl (lambda (r c l) (if (memf (lambda (k) (equal? k (get-column-id c cls))) 
                                                pkey-flds) (cons r l) l)) 
                     null (vector->list row) (sort (get-column-names cls) string<?))])
    (when (eq? (length pkey) 1) (set! pkey (first pkey)))
    pkey))

;;; Create a data object from a database row.
(define (create-data-object con cls row #:primary-key (primary-key #f))
  (let ([pkey (if primary-key primary-key (create-primary-key cls row))])
    (if (multi-hash-has-key? *data-objects* con cls pkey) 
        (let* ([obj (multi-hash-ref *data-objects* con cls pkey)])
          (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
          (set-field! data-object-state obj 'loaded)
          obj)
        (let* ([obj (new cls)])
          (set-data-object! obj row)
          (multi-hash-set! *data-objects* obj con cls pkey)
          obj))))

;;; Select a data object from the database.
(define-syntax (select-data-object stx)
  (syntax-parse stx
    ;; Select with joins, where-clause and arguments.
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) (~optional (~seq #:prepare? prep:expr)) 
        join:join-expr ... where:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                   [prep? (not (or (attribute prep) #'#f))])
       #'(let* ([ctxt (list cls)]
                [sql (make-select-statement con cls #:print? prnt? #:prepare? prep? 
                                            (string-append join.expr ... where.expr))])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]
    ;; Select with SQL string.
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) (~optional (~seq #:prepare? prep:expr)) 
        where-sql:expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                   [prep? (not (or (attribute prep) #'#f))])
       #'(let ([sql (make-select-statement con cls  #:print? prnt? #:prepare? prep? where-sql)])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]
    ))

;;; Select data objects from the database.
(define-syntax (select-data-objects stx)
  (syntax-parse stx
    ;; Select with joins, where-clause and arguments.
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) (~optional (~seq #:prepare? prep:expr))
        join:join-expr ... where:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                   [prep? (not (or (attribute prep) #'#f))])
       #'(let* ([ctxt (list cls)]
                [sql (make-select-statement con cls #:print? prnt? #:prepare? prep? 
                                           (string-append join.expr ... where.expr))])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) 
                              (query-rows con sql rest ...)))))]
    ;; Select all.
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) (~optional (~seq #:prepare? prep:expr)))
     (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                   [prep? (not (or (attribute prep) #'#f))])
       #'(let ([sql (make-select-statement con cls  #:print? prnt? #:prepare? prep? "")])
           (if prnt? sql (map (lambda (r) (create-data-object con cls r)) (query-rows con sql)))))]
    ;; Select with SQL string.
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) (~optional (~seq #:prepare? prep:expr)) 
        where-sql:expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)]
                   [prep? (not (or (attribute prep) #'#f))])
       #'(let ([sql (make-select-statement con cls  #:print? prnt? #:prepare? prep? where-sql)])
           (if prnt? sql (create-data-object con cls (query-row con sql rest ...)))))]
    ))

;;; Load a data object from the database by primary key.
(define (make-data-object con cls pkey) 
  (create-data-object con cls 
    (if (list? pkey)
        (apply query-row con (make-select-statement con cls 
                                                    (key-where-clause-sql con cls 
                                                                          (primary-key-fields cls))) 
               pkey)
        (query-row con (make-select-statement con cls 
                                              (key-where-clause-sql con cls (primary-key-fields cls)))
                   pkey)) #:primary-key pkey))

;;; Save a data object.
(define (save-data-object con obj) 
  (if (eq? (data-object-state obj) 'new) (insert-data-object con obj) 
      (update-data-object con obj)))

;;; Insert a data object
(define (insert-data-object con obj) 
  (let* ([cls (object-class obj)]
         [pst (virtual-statement (insert-sql con cls))]
         [flds (map (lambda (f) (false->sql-null (dynamic-get-field f obj))) 
                    (savable-fields con cls))])
    (apply query-exec con pst flds)
    (set-autoincrement-id! con obj)
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'saved)
    (multi-hash-set! *data-objects* obj con cls (get-primary-key obj))))

;;; Update a data object.
(define (update-data-object con obj) 
  (let* ([cls (object-class obj)]
         [pst (virtual-statement (update-sql con cls))]
         [flds (map (lambda (f) (false->sql-null (dynamic-get-field f obj))) 
                    (savable-fields con cls))]
         [pkey (map (lambda (f) (false->sql-null (dynamic-get-field f obj))) 
                    (primary-key-fields cls))])
    (apply query-exec con pst (append flds pkey))
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'saved)))

;;; Delete a data object.
(define (delete-data-object con obj)
  (let* ([cls (object-class obj)]
         [pst (virtual-statement (delete-sql con cls))]
         [pkey (map (lambda (f) (false->sql-null (dynamic-get-field f obj))) 
                    (primary-key-fields (object-class obj)))])
    (apply query-exec con pst pkey)
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'deleted)))


;;;; SERIALIZATION


;;; Generate a JS-expression from a data object.
(define (data-object->jsexpr obj)
  (let ([cls (object-class obj)])
    (hasheq (string->symbol (get-class-metadata external-name cls)) 
            (make-hasheq (map (lambda (d) (cons (string->symbol (third d)) 
                                                (dynamic-get-field (first d) obj))) 
                              (get-class-metadata columns cls))))))

;;; Create a data object from a JS-expression.
(define (jsexpr->data-object jsx)
  (let* ([md (find-external-name-metadata (first (hash-keys jsx)))]
         [cols (first (hash-values jsx))]
         [col-defs (get-field columns md)]
         [cls (get-field class md)]
         [obj (new cls)])
    (hash-map cols (lambda (k v) (dynamic-set-field! 
                                  (first (findf (lambda (c) (equal? (symbol->string k) (third c))) 
                                                col-defs)) obj v)))
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'deserialized)
    obj))

;;; Generate an X-expression from a data object.
(define (data-object->xexpr obj)
  (let ([cls (object-class obj)])
    (append (list (string->symbol (get-class-metadata external-name cls)) '()) 
            (map (lambda (d) (list (string->symbol (third d)) '() 
                                   (~a (dynamic-get-field (first d) obj)))) 
                 (get-class-metadata columns cls)))))

;;; Create a data object from an X-expression.
(define (xexpr->data-object xmlx)
  (let* ([md (find-external-name-metadata (first xmlx))]
         [col-defs (get-field columns md)]
         [cls (get-field class md)]
         [obj (new cls)])
    (map (lambda (x) (dynamic-set-field! 
                      (first (findf (lambda (c) (equal? (symbol->string (first x)) (third c))) 
                                    col-defs)) obj (third x)))
         (filter (lambda (x) (and (list? x) (> (length x) 1))) xmlx))
    (define-member-name data-object-state (get-class-metadata state-key cls))
    (set-field! data-object-state obj 'deserialized)
    obj))


;;;; CONTRACTS

(provide data-class data-class* data-class-info gen-data-class select-data-object select-data-objects
         get-column set-column! get-join (all-from-out "keywords.rkt")
         (contract-out
          [data-class? (any/c . -> . boolean?)]
          [data-object? (any/c . -> . boolean?)]
          [data-object-state (data-object? . -> . symbol?)]
          [make-data-object (connection? data-class? any/c . -> . data-object?)]
          [save-data-object (connection? data-object? . -> . void?)]
          [insert-data-object (connection? data-object? . -> . void?)]
          [update-data-object (connection? data-object? . -> . void?)]
          [delete-data-object (connection? data-object? . -> . void?)]
          [get-class-metadata-object (data-class? . -> . object?)]
          [table-name-normalizer (string? . -> . string?)]
          [column-name-normalizer (string? . -> . string?)]
          [join-name-normalizer (string? (or/c symbol?) . -> . string?)]
          [data-object->jsexpr (data-object? . -> . jsexpr?)]
          [jsexpr->data-object (jsexpr? . -> . data-object?)]
          [data-object->xexpr (data-object? . -> . xexpr?)]
          [xexpr->data-object (xexpr? . -> . data-object?)]
          [set-odbc-dbsystem-type! (symbol? . -> . void?)]))
 