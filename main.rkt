#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; main - Main module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013

(require db "keywords.rkt" "metadata.rkt" (for-syntax syntax/parse "stxclass.rkt"))
 
(provide data-class data-class? data-class-info data-object-state gen-data-class 
         make-data-object select-data-object select-data-objects save-data-object 
         insert-data-object update-data-object delete-data-object 
         get-joined-data-object get-joined-data-objects
         (all-from-out "keywords.rkt"))

;;; Define namespace anchor.
(define-namespace-anchor anchr)
(define ns (namespace-anchor->namespace anchr))

;;; Define an empty interface used to identify a data class.
(define data-class<%> (interface ()))

;;; Define type checker for a data class.
(define (data-class? cls) (implementation? cls data-class<%>))

;;; SQL placeholder by database system.
(define (sql-placeholder con) (if (eq? (dbsystem-name (connection-dbsystem con)) 'postgres) "$1" "?"))

;;; SQL schema by database system.
(define (schema-sql con tbl-nm)
  (cond [(eq? (dbsystem-name (connection-dbsystem con)) 'mysql)
         (string-append "select cols.column_name, cons.constraint_type, fkey.ordinal_position, 
   case when cols.extra='auto_increment' then 1 end autoincrement
from information_schema.columns AS cols
left join information_schema.key_column_usage as fkey
   on fkey.column_name=cols.column_name
   and fkey.table_name=cols.table_name
   and fkey.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
   on cons.constraint_name=fkey.constraint_name
   and cons.constraint_schema=fkey.constraint_schema
   and cons.table_name=fkey.table_name
   and cons.table_schema=fkey.table_schema
where cols.table_name='" tbl-nm "'
order by cons.constraint_type desc, fkey.ordinal_position, cols.column_name")]
        [(eq? (dbsystem-name (connection-dbsystem con)) 'sqlite3) (string-append "pragma table_info(" tbl-nm ");")]
        [(eq? (dbsystem-name (connection-dbsystem con)) 'oracle)
         (string-append "select cols.column_name,
   case when cons.constraint_type='P' then 'PRIMARY KEY' end constraint_type, cc.position, null
from all_tab_cols cols
join all_cons_columns cc
   on cols.owner=cc.owner
   and cols.table_name=cc.table_name
   and cols.column_name=cc.column_name
left outer join all_constraints cons
   on cc.constraint_name=cons.constraint_name
   and cons.owner=cols.owner
   and cons.constraint_type='P'
where cols.table_name='" tbl-nm "'
order by constraint_type desc, cc.position, cols.column_name")]
        ;; PostGres, SQL Server, and DB/2.
        [else (string-append "select cols.column_name, cons.constraint_type, keycols.ordinal_position, null
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name=cols.column_name
  and keycols.table_name=cols.table_name
  and keycols.table_schema=cols.table_schema
left join information_schema.table_constraints as cons
  ON cons.constraint_name=keycols.constraint_name
  and cons.constraint_schema=cons.constraint_schema
where cols.table_name='" tbl-nm "'
order by cons.constraint_type desc, keycols.ordinal_position, cols.column_name")]
        ))

;;; Class of an object
(define (object-class obj) (let-values ([(cls x) (object-info obj)]) cls))

;;; Field id from a column name.
;(define (column-field col cls)
;  (get-class-metadata-object cls)

;;; Return the state of a data object.
(define (data-object-state obj)
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (get-field data-object-state obj))
        
;;; Set autoincrement id.
(define (set-autoincrement-id! con obj)
  (when (get-class-metadata autoincrement-key (object-class obj))
    (cond [(eq? (dbsystem-name (connection-dbsystem con)) 'mysql)
           (dynamic-set-field! (string->symbol (get-class-metadata autoincrement-key (object-class obj))) 
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
           (if (member f (autoincrement-key-fields cls)) l (cons f l))) null (get-class-metadata column-names cls)))
         
;;; SQL where-clause for a key.
(define (key-where-clause con cls key)
  (string-append " where " (string-join (foldr (lambda (f l) (cons (string-append f "=" (sql-placeholder con)) l)) 
                       null (if (list? key) key (list key))) " and ")))

;;; Insert SQL.
(define (insert-sql con cls)
  (let ([col-nms (savable-fields con cls)])
    (string-append "insert " (get-class-metadata table-name cls)
                 " (" (string-join col-nms ", ") ")"
                 " values (" (string-join (make-list (length col-nms) "?") ", ") ")")))

;;; Update SQL.
(define (update-sql con cls)
  (let ([values (foldr (lambda (f l) (cons (string-append f "=" (sql-placeholder con)) l)) 
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
  (string-append "select " (string-join (get-class-metadata column-names cls) ", ")
                 " from " (get-class-metadata table-name cls) " t "
                 where-clause))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields con schema)
  (let ([pkey (map (lambda (v) (vector-ref v 0)) 
                   (filter (lambda (f) (if (string? (vector-ref f 1)) (string=? (vector-ref f 1) "PRIMARY KEY") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))

#| Model:
(data-class object% 
            (table-name "TST_Person") 
            (external-name "Person")
            (init-column (id "id"))
            (column (name #f "name")
                    (description #f "description")
                    (address-id #f "address_id"))
            (join (vehicles id vehicle% person-id)
                  (address address-id address% id))
            (primary-key id #:autoincrement #t)
            (field (data #f))
            (super-new)
            (inspect #f))
|#

;;; Creates a data class.
(define-syntax (data-class stx)
  (syntax-parse stx 
    [(data-class base-cls:id elem:data-class-element ...) 
     #'(let* ([m (new data-class-metadata%)])
         (define-member-name data-object-state (get-field state-key m))
         (class* base-cls (data-class<%>) elem.expr ... 
           (field [data-object-state 'new])
           (unless (hash-has-key? *data-class-metadata* this%)
             (set-field! column-names m (append elem.col-nms ...))
             (hash-for-each (make-hash (append elem.jn-defs ...))
                            (lambda (k v) (hash-set! (get-field joins m) k v)))
             (unless (get-field external-name m) 
               (set-field! external-name m (get-field table-name m)))
             (hash-set! *data-class-metadata* this% m))
           this))]
    ))

;;; Generates a class using database schema information.
(define (gen-data-class con tbl-nm . rest) 
  (let* ([schema (query-rows con (schema-sql con tbl-nm))]
         [col-nms (foldl (lambda (f l) (if (member (vector-ref f 0) l) l 
                                           (cons (vector-ref f 0) l))) null schema)]
         [cols (map (lambda (f) (list (string->symbol f) #f f)) col-nms)]
         [pkey (find-primary-key-fields con schema)]
         [auto-key-found (findf (lambda (f) (eq? (vector-ref f 3) 1)) schema)]
         [auto-key (unless (eq? auto-key-found #f) (vector-ref auto-key-found 0))]
         [ext-nm tbl-nm]
         [cls-nm (string->symbol (string-append tbl-nm "%"))])
    (eval-syntax #`(let ([#,cls-nm
                  (data-class object%
                              (table-name #,tbl-nm)
                              #,(append '(column) cols)
                              #,(append (if (vector? auto-key-found) 
                                           (list 'primary-key pkey '#:autoincrement #t)
                                           (list 'primary-key pkey)))
                              (super-new)
                              (inspect #f)
                              #,@rest
                              )])
             #,cls-nm) ns)
    ))

;;; Sets the data in a data object.
(define (set-data-object! con obj row)
  (map (lambda (f d) (dynamic-set-field! (string->symbol f) obj d)) 
       (get-class-metadata column-names (object-class obj)) (vector->list row))
  (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
  (set-field! data-object-state obj 'loaded))

;;; Select a data object from the database.
(define-syntax-rule (select-data-object con cls where-clause rest)
  (let* ([obj (new cls)])
    (set-data-object! con obj (query-row con (select-sql con obj where-clause) rest))
    obj
    ))

;;; Load a data object from the database by primary key.
(define-syntax-rule (make-data-object con cls pkey)
  (let* ([obj (new cls)])
    (set-data-object! con obj (query-row con (select-sql con cls (key-where-clause con cls (primary-key-fields cls))) pkey))
    obj
    ))

;;; Select data objects from the database
(define-syntax-rule (select-data-objects con cls where-clause rest)
  (let* ([obj (new cls)]
         [rows (query-rows con (select-sql con obj where-clause) rest)]
         [objs (make-list (length rows) (new cls))])
    (map (lambda (o r) (set-data-object! con o r)) objs rows)
    objs
    ))

;;; Save a data object.
(define (save-data-object con obj) 
  (if (eq? (data-object-state obj) 'new) (insert-data-object con obj) 
      (update-data-object con obj)))

;;; Insert a data object
(define (insert-data-object con obj) 
  (let ([sql (insert-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field (string->symbol f) obj)) 
                   (savable-fields con (object-class obj)))])
    (apply query-exec con sql flds)
    (set-autoincrement-id! con obj)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)
    ))

;;; Update a data object.
(define (update-data-object con obj) 
  (let ([sql (update-sql con (object-class obj))]
        [flds (map (lambda (f) (dynamic-get-field (string->symbol f) obj)) 
                   (savable-fields con (object-class obj)))]
        [pkey (map (lambda (f) (dynamic-get-field (string->symbol f) obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql (append flds pkey))
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'saved)))

;;; Delete a data object.
(define (delete-data-object con obj) 
  (let ([sql (delete-sql con (object-class obj))]
        [pkey (map (lambda (f) (dynamic-get-field (string->symbol f) obj)) 
                   (primary-key-fields (object-class obj)))])
    (apply query-exec con sql pkey)
    (define-member-name data-object-state (get-class-metadata state-key (object-class obj)))
    (set-field! data-object-state obj 'deleted)))

;;; Get a contained data object. This will join to the object on first use.
(define-syntax (get-joined-data-object stx)
  (syntax-case stx ()
    ([_ id obj con] 
     #'(when (eq? (get-field id obj) #f)
         (let ([jn-def (hash-ref (get-class-metadata joins (object-class obj)) 'id)])
           (make-data-object con (data-join-class jn-def) (dynamic-get-field (data-join-foreign-key jn-def) obj)))))))

;;; Get contained data objects. This will select the objects on first use.
(define (get-joined-data-objects id obj con)
  (when (eq? (dynamic-get-field id obj) #f)
    (let* ([cls (object-class obj)]
           [jn-def (hash-ref (get-class-metadata joins cls) id)])
      (dynamic-set-field! id obj (select-data-objects con (data-join-class jn-def) 
                                                      (key-where-clause con cls (data-join-key jn-def))
                                                      (dynamic-get-field (data-join-foreign-key jn-def) obj)))))
    (dynamic-get-field id obj))
