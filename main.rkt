#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; main - Main module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db "keywords.rkt" "metadata.rkt" (for-syntax syntax/parse "stxclass.rkt"))
 
(provide data-class data-class* data-class? data-class-info data-object-state gen-data-class 
         make-data-object select-data-object select-data-objects save-data-object 
         insert-data-object update-data-object delete-data-object 
         get-join get-column set-column! 
         (all-from-out "keywords.rkt"))

;;; Define namespace anchor.
(define-namespace-anchor anchr)
(define ns (namespace-anchor->namespace anchr))

;;; Define an empty interface used to identify a data class.
(define data-class-internal<%> (interface ()))

;;; Define type checker for a data class.
(define (data-class? cls) (implementation? cls data-class-internal<%>))


;;; DATA CLASS DEFINITION


#| Model:
(data-class object% 
            (table-name "TST_Person") 
            (external-name "Person")
            (init-column (id "id"))
            (column (name #f "name")
                    (description #f "description")
                    (address-id #f "address_id"))
            (join (vehicles id vehicle% person-id)
                  (address address-id 'address% id))
            (primary-key id #:autoincrement #t)
            (field (data #f))
            (super-new)
            (inspect #f))
|#

;;; Class of an object
(define (object-class obj) (let-values ([(cls x) (object-info obj)]) cls))

;;; Return the state of a data object.
(define (data-object-state obj)
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (get-field data-object-state obj))
        
;;; Set autoincrement id.
(define (set-autoincrement-id! con obj)
  (when (get-class-metadata autoincrement-key (object-class obj))
    (cond [(eq? (dbsystem-name (connection-dbsystem con)) 'mysql)
           (dynamic-set-field! (get-class-metadata autoincrement-key (object-class obj))
                               obj (query-value con "select last_insert_id()"))]
          )))

;;; Primary key fields
(define (primary-key-fields cls)
  (let ([pkey (get-class-metadata primary-key cls)]) (if (list? pkey) pkey (list pkey))))

;;; Autoincrement key fields
(define (autoincrement-key-fields cls)
  (let ([pkey (get-class-metadata autoincrement-key cls)]) (if (list? pkey) pkey (list pkey))))
    
;;; Columns without the autoincrement key
(define (savable-fields con cls)
  (foldr (lambda (f l) 
           (if (member f (autoincrement-key-fields cls)) l (cons f l))) null (get-column-ids cls)))
         
;;; Get the column name for a column field in a class.
(define (get-column-name f cls)
   (cdr (findf (lambda (c) (eq? f (car c))) (get-class-metadata columns cls))))

;;; SQL where-clause for a key.
(define (key-where-clause con cls key)
  (string-append " where " (string-join (map (lambda (f) (string-append (get-column-name f cls) "=" (sql-placeholder con))) 
                                             (if (list? key) key (list key))) " and ")))

;;; Insert SQL.
(define (insert-sql con cls)
  (let ([col-nms (map (lambda (f) (get-column-name f cls)) (savable-fields con cls))])
    (string-append "insert " (get-class-metadata table-name cls)
                 " (" (string-join col-nms ", ") ")"
                 " values (" (string-join (make-list (length col-nms) "?") ", ") ")")))

;;; Update SQL.
(define (update-sql con cls)
  (let ([values (foldr (lambda (f l) (cons (string-append (get-column-name f cls) "=" (sql-placeholder con)) l)) 
                       null (savable-fields con cls))])
    (string-append "update " (get-class-metadata table-name cls)
                 " set " (string-join values ", ")
                 (key-where-clause con cls (primary-key-fields cls)))))

;;; Delete SQL.
(define (delete-sql con cls)
  (string-append "delete from " (get-class-metadata table-name cls)
                 (key-where-clause con cls (primary-key-fields cls))))

;;; Select SQL.
(define (select-sql con cls where-clause)
  (string-append "select " (string-join (get-column-names cls) ", ")
                 " from " (get-class-metadata table-name cls) " t "
                 where-clause))

;;; Define a data class.
(define-syntax (data-class stx)
  (syntax-parse stx 
    [(data-class base-cls:id elem:data-class-element ...) 
     #'(let* ([m (new data-class-metadata%)])
         (define-member-name data-object-state-internal (get-field state-key m))
         (class* base-cls (data-class-internal<%>) elem.expr ... 
           (field [data-object-state-internal 'new])
           (unless (hash-has-key? *data-class-metadata* this%)
             (set-field! columns m (append elem.col-defs ...))
             (set-field! joins m (append elem.jn-defs ...))
             (unless (get-field external-name m) 
               (set-field! external-name m (get-field table-name m)))
             (hash-set! *data-class-metadata* this% m))
           this))]
    ))

;;; Define a data class with interfaces.
(define-syntax (data-class* stx)
  (syntax-parse stx 
    [(data-class base-cls:id (i-face:id ...) elem:data-class-element ...) 
     #'(let* ([m (new data-class-metadata%)])
         (define-member-name data-object-state-internal (get-field state-key m))
         (class* base-cls (data-class-internal<%> i-face ...) elem.expr ... 
           (field [data-object-state-internal 'new])
           (unless (hash-has-key? *data-class-metadata* this%)
             (set-field! columns m (append elem.col-defs ...))
             (set-field! joins m (append elem.jn-defs ...))
             (unless (get-field external-name m) 
               (set-field! external-name m (get-field table-name m)))
             (hash-set! *data-class-metadata* this% m))
           this))]
    ))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields con schema)
  (let ([pkey (map (lambda (v) (string->symbol (vector-ref v 0)) )
                   (filter (lambda (f) (if (string? (vector-ref f 1)) (string=? (vector-ref f 1) "P") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))


;;; GENERATE DATA CLASSES


;;; SQL placeholder by database system.
(define (sql-placeholder con) (if (eq? (dbsystem-name (connection-dbsystem con)) 'postgres) "$1" "?"))

;;; Load MySQL schema.
(define (load-mysql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, substring(cons.constraint_type, 1, 1) as constraint_type, fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, fkey.referenced_table_name, fkey.referenced_column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.referenced_column_name, 'R', fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end, cols.table_name, cols.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where fkey.referenced_table_name='" tbl-nm "'"))
             (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.referenced_table_schema='" schema-nm "'")))))
    (set! schema-sql (string-append schema-sql " order by constraint_type, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load Oracle schema
(define (load-oracle-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name,cons.constraint_type
   cc.position, null, rcons.table_name, rcc.column_name
from all_tab_cols cols
join all_cons_columns cc
   on cols.owner=cc.owner
   and cols.table_name=cc.table_name
   and cols.column_name=cc.column_name
left outer join all_constraints cons
   on cc.constraint_name=cons.constraint_name
   and cols.owner=cons.owner
   and cons.constraint_type in ('P','R')
left outer join all_constraints rcons
   on cons.r_constraint_name=rcons.constraint_name
   and cons.r_owner=rcons.owner 
left outer join all_cons_columns rcc
   on rcons.constraint_name=rcc.constraint_name
   and rcons.owner=rcc.owner
   and rcons.table_name=rcc.table_name
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.owner='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select rcc.column_name, 'R', cc.position, 
   case when cols.extra='auto_increment' then 1 end, cols.table_name, cols.column_name
from all_tab_cols cols
join all_cons_columns cc
   on cols.owner=cc.owner
   and cols.table_name=cc.table_name
   and cols.column_name=cc.column_name
left outer join all_constraints cons
   on cc.constraint_name=cons.constraint_name
   and cols.owner=cons.owner
   and cons.constraint_type in ('P','R')
left outer join all_constraints rcons
   on cons.r_constraint_name=rcons.constraint_name
   and cons.r_owner=rcons.owner 
left outer join all_cons_columns rcc
   on rcons.constraint_name=rcc.constraint_name
   and rcons.owner=rcc.owner
   and rcons.table_name=rcc.table_name
where rcons.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and rcols.owner='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_type, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load PostgreSQL schema.
(define (load-postgresql-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, cons.constraint_type, keycols.ordinal_position, 
  case when substring(cols.column_default from 1 for 6) = 'nextval' then 1 end, fkey.table_name, fkey.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, 
   case when substring(cols.column_default from 1 for 6) = 'nextval' then 1 end, cols.table_name, cols.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_type, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load SQLite3 schema.
(define (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "pragma table_info(" tbl-nm ");")])
    (query-rows con schema-sql)))

;;; Load SQL Server schema.
(define (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name, cons.constraint_type, keycols.ordinal_position, 
  case when columnproperty(object_id(table_name), column_nam, 'isidentity')=1 then 1 end,
  fkey.table_name, fkey.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, 
   case when columnproperty(object_id(table_name), column_nam, 'isidentity')=1 then 1 end, 
   cols.table_name, cols.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_type, ordinal_position, column_name"))
    (query-rows con schema-sql)))

;;; Load default schema.
(define (load-default-schema con schema-nm tbl-nm rev-jn?)
  (let ([schema-sql (string-append "select cols.column_name as col_name, cons.constraint_type, keycols.ordinal_position, null,
  fkey.table_name, fkey.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  on cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
left join information_schema.referential_constraints as refs
  on  refs.constraint_schema = cons.constraint_schema
  and refs.constraint_name = cons.constraint_name
left join information_schema.key_column_usage as fkey
  on fkey.constraint_schema = refs.unique_constraint_schema
  and fkey.constraint_name = refs.unique_constraint_name
where cols.table_name='" tbl-nm "'")])
    (when schema-nm (set! schema-sql (string-append schema-sql " and cols.table_schema='" schema-nm "'")))
    (when rev-jn? 
      (begin (set! schema-sql (string-append schema-sql " union 
select fkey.column_name, 'R', fkey.ordinal_position, null, 
   cols.table_name, cols.column_name
from information_schema.columns as cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where fkey.table_name='" tbl-nm "'")))
      (when schema-nm (set! schema-sql (string-append schema-sql " and fkey.table_schema='" schema-nm "'"))))
    (set! schema-sql (string-append schema-sql " order by constraint_type, ordinal_position, col_name"))
    (query-rows con schema-sql)))

;;; SQL schema by database system type.
(define (load-schema con schema-nm tbl-nm #:reverse-join? rev-jn? #:db-system-type db-sys-type)
  (cond [(eq? db-sys-type 'mysql) (load-mysql-schema con schema-nm tbl-nm rev-jn?)]
        [(eq? db-sys-type 'oracle) (load-oracle-schema con schema-nm tbl-nm rev-jn?)]
        [(eq? db-sys-type 'postgresql) (load-postgresql-schema con schema-nm tbl-nm rev-jn?)]
        [(eq? db-sys-type 'sqlite3) (load-sqlite3-schema con schema-nm tbl-nm rev-jn?)]
        [(eq? db-sys-type 'sqlserver) (load-sqlserver-schema con schema-nm tbl-nm rev-jn?)]
        [else (load-default-schema con schema-nm tbl-nm rev-jn?)]
        ))

;;; Generate a class using database schema information.
(define (gen-data-class con tbl-nm 
                        #:db-system-type (db-sys-type (dbsystem-name (connection-dbsystem con)))
                        #:generate-joins? (gen-joins? #t)
                        #:generate-reverse-joins? (gen-rev-joins? #f)
                        #:schema-name (schema-nm #f)
                        #:inherits (base-cls 'object%)
                        #:table-name-normalizer (tbl-nm-norm (lambda (n) (string-append (string-downcase n) "%"))) 
                        #:column-name-normalizer (col-nm-norm (lambda (n) (string-downcase n))) 
                        #:join-name-normalizer (join-nm-norm (lambda (n) (string-downcase n))) 
                        #:external-name-normalizer (ext-nm-norm (lambda (n) (string-downcase n)))
                        #:print? (prnt? #f)
                        . rest) 
  (let* ([schema (load-schema con schema-nm tbl-nm #:reverse-join? gen-rev-joins? #:db-system-type db-sys-type)]
         [col-nms (foldl (lambda (f l) (if (member (vector-ref f 0) l) l 
                                           (cons (vector-ref f 0) l))) null schema)]
         [cols (map (lambda (f) (list (string->symbol (col-nm-norm f)) #f f)) col-nms)]
         [jns (if (or gen-joins? gen-rev-joins?) 
                  (foldl (lambda (f l) (if (or (equal? (vector-ref f 1) "F") (equal? (vector-ref f 1) "R"))
                                           (cons (list (string->symbol (join-nm-norm (vector-ref f 4))) 
                                                       (vector-ref f 0) (vector-ref f 4) (vector-ref f 5)) l) l))
                         null schema) null)]
         [pkey (find-primary-key-fields con schema)]
         [auto-key-found (findf (lambda (f) (eq? (vector-ref f 3) 1)) schema)]
         [auto-key (unless (eq? auto-key-found #f) (vector-ref auto-key-found 0))]
         [ext-nm tbl-nm]
         [cls-nm (string->symbol (tbl-nm-norm tbl-nm))])
    (if prnt? (syntax->datum #`(let ([#,cls-nm
                       (data-class #,base-cls
                                   (table-name #,tbl-nm)
                                   #,(append '(column) cols)
                                   #,(append (if (vector? auto-key-found) 
                                                 (list 'primary-key pkey '#:autoincrement #t)
                                                 (list 'primary-key pkey)))
                                   #,(if (and gen-joins? (list? jns) (> (length jns) 0)) (append '(join) jns) '(begin #f))
                                   (super-new)
                                   (inspect #f)
                                   #,@rest
                                   )])
                  #,cls-nm))
        (eval-syntax #`(let ([#,cls-nm
                              (data-class #,base-cls
                                          (table-name #,tbl-nm)
                                          #,(append '(column) cols)
                                          #,(append (if (vector? auto-key-found) 
                                                        (list 'primary-key pkey '#:autoincrement #t)
                                                        (list 'primary-key pkey)))
                                          #,(if (and gen-joins? (list? jns) (> (length jns) 0)) (append '(join) jns) '(begin #f))
                                          (super-new)
                                          (inspect #f)
                                          #,@rest
                                          )])
                         #,cls-nm) ns)
        )))
  

;;; PERSISTENCE


;;; Set the data in a data object.
(define (set-data-object! con obj row)
  (map (lambda (f v) (dynamic-set-field! f obj v)) 
       (get-column-ids (object-class obj)) (vector->list row))
  (define-member-name data-object-state-internal (get-class-metadata state-key (object-class obj)))
  (set-field! data-object-state-internal obj 'loaded))

;;; Load a data object from the database by primary key.
(define-syntax-rule (make-data-object con cls pkey)
  (let* ([obj (new cls)])
    (set-data-object! con obj (query-row con (select-sql con cls (key-where-clause con cls (primary-key-fields cls))) pkey))
    obj
    ))

;;; Save a data object.
(define-syntax-rule (save-data-object con obj) 
  (if (eq? (data-object-state obj) 'new) (insert-data-object con obj) 
      (update-data-object con obj)))

;;; Insert a data object
(define-syntax-rule (insert-data-object con obj) 
  (let ([sql (insert-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field f obj)) 
                   (savable-fields con (object-class obj)))])
    (apply query-exec con sql flds)
    (set-autoincrement-id! con obj)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)
    ))

;;; Update a data object.
(define-syntax-rule (update-data-object con obj) 
  (let ([sql (update-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field f obj)) 
                   (savable-fields con (object-class obj)))]
        [pkey (map (lambda (f) (dynamic-get-field f obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql (append flds pkey))
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)))

;;; Delete a data object.
(define-syntax-rule (delete-data-object con obj) 
  (let ([sql (delete-sql con (object-class obj))]
        [pkey (map (lambda (f) (dynamic-get-field f obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql pkey)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'deleted)))

;;; Get joined data objects. This will select the objects on first use.
(define-syntax (get-join stx)
  (syntax-case stx ()
    ([_ id obj con] 
     #'(when (eq? (get-field id obj) #f)
         (let* ([cls (object-class obj)]
                [jn-def (cdr (findf (lambda (f) (eq? 'id (car f))) (get-class-metadata joins (object-class obj))))]
                [jn-cls-expr (data-join-class jn-def)]
                [jn-cls (cond
                          [(class? jn-cls-expr) jn-cls-expr]
                          [(symbol? jn-cls-expr) 
                           (findf (lambda (k) (string=? (~a k) (string-append "#<class:" (symbol->string jn-cls-expr) ">"))) 
                                  (hash-keys *data-class-metadata*))])])
           (set-field! id obj (if (eq? (data-join-cardinality jn-def) 'one-to-one) 
                                  (make-data-object con jn-cls (dynamic-get-field (data-join-foreign-key jn-def) obj))
                                  (select-data-objects con jn-cls 
                                                       (key-where-clause con jn-cls (data-join-key jn-def))
                                                       (dynamic-get-field (data-join-foreign-key jn-def) obj)))))
     (get-field id obj)))))

;;; Get a data column.
; TODO: Track state
(define-syntax-rule (get-column col obj) (get-field col obj))

;;; Set a data column.
(define-syntax-rule (set-column! col obj val) (set-field! col obj val))


;;; RQL 


#| Model:
(select (address% a)
        (left-join state% (= abbr (a state)))
        (and (like (person last-name) "A%")
             (or (= (id #,(get-field id obj))
                 (not (in city ("Chicago" "New York"))))
             (between zip-code 10000 60999)))
|#

;;; Define RQL operators.
(define-syntax rql-and [syntax-rules () ((_ a ...) (string-append "(" (string-join (list (~a a) ...) " and " ) ")"))])
(define-syntax rql-or [syntax-rules () ((_ a ...) (string-append "(" (string-join (list (~a a) ...) " or " ) ")"))])
(define-syntax rql-not [syntax-rules () ((_ a ...) (string-append "(not " (~a a) ... ")"))])
(define-syntax rql-= [syntax-rules () ((_ a b) (string-append (~a a) " = " (~a b)))])
(define-syntax rql-column [syntax-rules () ((_ a b) (string-append (~a a) "." (~a b)))])

;;; Parse an RQL expression.
(begin-for-syntax
  (define-syntax-class rql-expr
    #:literals (join where and or not =)
    (pattern and #:with (expr ...) #'(rql-and))
    (pattern or #:with (expr ...) #'(rql-or))
    (pattern not #:with (expr ...) #'(rql-not))
    (pattern = #:with (expr ...) #'(rql-=))
    (pattern i:id #:with (expr ...) #'('i))
    (pattern s:str #:with (expr ...) #'(s))
    (pattern n:nat #:with (expr ...) #'(n))
    (pattern (p1:expr p2:expr) #:with (expr ...) #'((rql-column 'p1 'p2)))
    (pattern l:rql-expr-list #:with (expr ...) #'((l.expr ...))))
  (define-syntax-class rql-expr-list
    (pattern (rql:rql-expr ...) #:with (expr ...) #'(rql.expr ... ...)))
  (define-syntax-class join-expr 
    (pattern (join table:id rql:rql-expr) #:with (expr ...) #'("join " (~a 'table) " on " rql.expr ... " ")))
  (define-syntax-class where-expr 
    (pattern (where rql:rql-expr) #:with (expr ...) #'("where " rql.expr ...))))

;;; Select a data object from the database.
(define-syntax (select-data-object stx)
  (syntax-parse stx
    #:literals (join where)
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) join-expr:join-expr where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls (string-append join-expr.expr ... where-expr.expr ...))])
           (unless prnt? (set-data-object! con obj (query-row con rest ...)))
           (if prnt? sql obj)))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:where-expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls (string-append where-expr.expr ...))])
           (unless prnt? (set-data-object! con obj (query-row con sql rest ...)))
           (if prnt? sql obj)))]
    [(_ con:id cls:id (~optional (~seq #:print? prnt:expr)) where-expr:expr rest:expr ...)
     (with-syntax ([prnt? (or (attribute prnt) #'#f)])
       #'(let* ([obj (new cls)]
                [sql (select-sql con cls where-expr)])
           (unless prnt? (set-data-object! con obj (query-row con sql rest ...)))
           (if prnt? sql obj)))]))

;;; Select data objects from the database.
(define-syntax (select-data-objects stx)
  (syntax-parse stx
    [(_ con:id cls:id where-expr:where-expr rest:expr ...)
     #'(let* ([rows (query-rows con (select-sql con cls where-expr.expr ...) rest ...)]
              [objs (make-list (length rows) (new cls))])
         (map (lambda (o r) (set-data-object! con o r)) objs rows)
         objs)]
    [(_ con:id cls:id where-expr:expr rest:expr ...)
     #'(let* ([rows (query-rows con (select-sql con cls where-expr) rest ...)]
              [objs (make-list (length rows) (new cls))])
         (map (lambda (o r) (set-data-object! con o r)) objs rows)
         objs)]))