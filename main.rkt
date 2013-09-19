#lang racket
      
(require db)
(require syntax/parse (for-syntax syntax/parse))
 
(provide data-object% data-class-info gen-data-class data-class make-data-object select-data-object select-data-objects)

;;; Define namespace anchor.
(define-namespace-anchor anchr)
(define ns (namespace-anchor->namespace anchr))

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

;;; Define data class metadata struct.
(define data-class-metadata% 
  (class object% 
    (init-field table-name column-names primary-key auto-increment-key external-name
                new? deleted? class-name)
    (super-new)
    (inspect #f)))

;;; Define a global table holding data class metadata.
(define *data-class-metadata* (make-hash))

;;; Get a data class metadata field.
(define-syntax-rule (get-class-metadata id cls)
  (get-field id (hash-ref *data-class-metadata* cls)))

;;; Set a data class metadata field.
(define-syntax-rule (set-class-metadata! id cls val)
  (set-field! id (hash-ref *data-class-metadata* cls) val))

;;; Get a data class metadata field for an object.
(define-syntax-rule (get-object-metadata id obj)
  (let-values ([(cls x) (object-info obj)]) (get-class-metadata id cls)))

;;; Get a data class metadata field for an object.
(define-syntax-rule (set-object-metadata! id obj val)
  (let-values ([(cls x) (object-info obj)]) (set-class-metadata! id cls val)))

;;; Dynamically get a data class metadata field.
(define-syntax-rule (dynamic-get-class-metadata id cls)
  (dynamic-get-field id (hash-ref *data-class-metadata* cls)))

;;; Dynamically set a data class metadata field.
(define-syntax-rule (dynamic-set-class-metadata! id cls val)
  (dynamic-set-field! id (hash-ref *data-class-metadata* cls) val))

;;; Dynamically get a data class metadata field for an object.
(define-syntax-rule (dynamic-get-object-metadata id obj)
  (let-values ([(cls x) (object-info obj)]) (dynamic-get-class-metadata id cls)))

;;; Dynamically set a data class metadata field for an object.
(define-syntax-rule (dynamic-set-object-metadata! id obj val)
  (let-values ([(cls x) (object-info obj)]) (dynamic-set-class-metadata! id cls val)))
 
;;; Return info about a data class.
(define (data-class-info cls)
  (values (get-class-metadata table-name (hash-ref *data-class-metadata* cls))
          (get-class-metadata column-names (hash-ref *data-class-metadata* cls))
          (get-class-metadata primary-key (hash-ref *data-class-metadata* cls))
          (get-class-metadata auto-increment-key (hash-ref *data-class-metadata* cls))
          (get-class-metadata external-name (hash-ref *data-class-metadata* cls))
          (get-class-metadata new? (hash-ref *data-class-metadata* cls))
          (get-class-metadata deleted? (hash-ref *data-class-metadata* cls))
          (get-class-metadata class-name (hash-ref *data-class-metadata* cls))
  ))
        
;;; Set auto-increment id.
(define (set-auto-increment-id! con obj)
  (when (get-object-metadata auto-increment-key obj)
    (cond [(eq? (dbsystem-name (connection-dbsystem con)) 'mysql)
           (dynamic-set-field! (string->symbol (get-object-metadata auto-increment-key obj)) obj (query-value con "select last_insert_id()"))]
          )))

;;; Primary key fields
(define (primary-key-fields obj)
  (let ([pkey (get-object-metadata primary-key obj)]) (if (list? pkey) pkey (list pkey))))
    
;;; Columns without the primary key.
(define (savable-fields con obj)
  (foldr (lambda (f l) (if (member f (primary-key-fields obj)) l (cons f l))) null (get-object-metadata column-names obj)))
         
;;; SQL where-clause for the primary key.
(define (primary-key-where-clause con obj)
  (string-append " where " (string-join (foldr (lambda (f l) (cons (string-append f "=" (sql-placeholder con)) l)) 
                       null (primary-key-fields obj)) " and ")))

;;; Insert SQL.
(define (insert-sql con obj)
  (let ([col-nms (savable-fields con obj)])
    (string-append "insert " (get-object-metadata table-name obj)
                 " (" (string-join col-nms ", ") ")"
                 " values (" (string-join (make-list (length col-nms) "?") ", ") ")")))

;;; Update SQL.
(define (update-sql con obj)
  (let ([values (foldr (lambda (f l) (cons (string-append f "=" (sql-placeholder con)) l)) 
                       null (savable-fields con obj))])
    (string-append "update " (get-object-metadata table-name obj)
                 " set " (string-join values ", ")
                 (primary-key-where-clause con obj))
    ))

;;; Delete SQL.
(define (delete-sql con obj)
  (string-append "delete from " (get-object-metadata table-name obj)
                 (primary-key-where-clause con obj)))

;;; Select SQL.
(define (select-sql con obj where-clause)
  (string-append "select " (string-join (get-object-metadata column-names obj) ", ")
                 " from " (get-object-metadata table-name obj) " t "
                 where-clause))

;;; Data Object class.
(define-local-member-name table-name column-names primary-key auto-increment-key external-name 
  new? deleted? class-name)
(define data-object% 
  (class object%
    (define/public (save con) (if (get-object-metadata new? this) (insert con) (update con)))
    (define/public (insert con) 
      (let ([sql (insert-sql con this)]
            [flds (map (lambda (f) (dynamic-get-object-metadata (string->symbol f) this)) (savable-fields con this))])
        (apply query-exec con sql flds)
        (set-auto-increment-id! con this)
        (set-field! new? this #f)))
    (define/public (update con) 
      (let ([sql (update-sql con this)]
            [flds (map (lambda (f) (dynamic-get-object-metadata (string->symbol f) this)) (savable-fields con this))]
            [pkey (map (lambda (f) (dynamic-get-object-metadata (string->symbol f) this)) (primary-key-fields this))])
        (apply query-exec con sql (append flds pkey))
        (set-field! new? this #f)))
    (define/public (delete con) 
      (let ([sql (delete-sql con this)]
            [pkey (map (lambda (f) (dynamic-get-object-metadata (string->symbol f) this)) (primary-key-fields this))])
        (apply query-exec con sql pkey)
        (set-field! deleted? this #t)))
    (super-new)
    (inspect #f)
    ))

;;; Find primary key fields in a table schema.
(define (find-primary-key-fields con schema)
  (let ([pkey (map (lambda (v) (vector-ref v 0)) 
                   (filter (lambda (f) (if (string? (vector-ref f 1)) (string=? (vector-ref f 1) "PRIMARY KEY") #f))
                           schema))])
    (if (eq? (length pkey) 1) (first pkey) pkey)))

#| Model:
(data-class data-object% 
            (table-name "TST_Person") 
            (external-name "Person")
            (table-name-casing 'camel-case)
            (table-name-prefix "tst_")
            (column-name-casing 'underscores)
            (columns (id #f "id") 
                     (name #f "name")
                     (description #f "description")
                     (address-id #f "address_id"))
            (primary-key id #:auto-increment #t)
            (join (vehicles vehicle% id person-id)
                  (address address% address-id id))
            (fields (data #f))
            ...
            (super-new)
            (inspect #f))
|#
;;; Creates a data class.
(require syntax/parse (for-syntax syntax/parse))

(define-syntax (data-class2 stx)
  (define-syntax-class tbl-name 
    #:description "table name" 
    #:literals (table-name)
    (pattern (table-name tbl-nm:str) #:with expr #'(list tbl-nm)))
  (syntax-parse stx
    [(data-class tbl-nm:tbl-name) #'(list tbl-nm.expr)]
    ))

(data-class2 (table-name "test"))

(define (data-class tbl-nm col-nms #:primary-key [key (list (first col-nms))]
                    #:auto-increment-key [auto-key #f]
                    #:external-name [ext-nm tbl-nm]
                    . rest) 
  (let* ([flds (map (lambda (f) (list (string->symbol f) #f)) col-nms)]
         [cls (eval `(let ([,(string->symbol (string-append tbl-nm "%"))
                                 (class data-object%
                                   ,(append '(field) flds)
                                   (super-new)
                                   (inspect #f)
                                   ,(unless (eq? rest null) (apply values rest))
                                   )])
                            ,(string->symbol (string-append tbl-nm "%"))) ns)])
    (hash-set! *data-class-metadata* cls ((new data-class-metadata% 
                                               [table-name tbl-nm] 
                                               [column-names col-nms]
                                               [primary-key key]
                                               [auto-increment-key auto-key] 
                                               [external-name ext-nm]
                                               [new? #t]
                                               [deleted? #f]
                                               [class-name (string->symbol (string-append tbl-nm "%"))])))
    cls
    ))

;;; Generates a class using database schema information.
(define (gen-data-class con tbl-nm . rest) 
  (let* ([schema (query-rows con (schema-sql con tbl-nm))]
         [col-nms (foldr (lambda (f l) (if (member (vector-ref f 0) l) l (cons (vector-ref f 0) l))) null schema)]
         [flds (map (lambda (f) (list (string->symbol f) #f)) col-nms)]
         [key (find-primary-key-fields con schema)]
         [auto-key (vector-ref (findf (lambda (f) (eq? (vector-ref f 3) 1)) schema) 0)]
         [ext-nm tbl-nm])
    (eval `(let ([,(string->symbol (string-append tbl-nm "%"))
                  (class data-object%
                    ,(append '(field) flds)
                    (super-new [table-name ,tbl-nm]
                               [column-names ',col-nms]
                               [primary-key ,key]
                               [auto-increment-key ,auto-key]
                               [external-name ,ext-nm])
                    (inspect #f)
                    ,(unless (eq? rest null) (apply values rest))
                    )])
             ,(string->symbol (string-append tbl-nm "%"))) ns)
    ))

;;; Sets the data in a data object.
(define (set-data-object! con obj row)
  (map (lambda (f d) (dynamic-set-field! (string->symbol f) obj d)) 
        (get-field column-names obj) (vector->list row))
   (set-field! new? obj #f))

;;; Select a data object from the database.
(define (select-data-object con cls% where-clause &rest)
  (let* ([obj (new cls%)])
    (set-data-object! con obj (query-row con (select-sql con obj where-clause) &rest))
    obj
    ))

;;; Load a data object from the database by primary key.
(define (make-data-object con cls% pkey)
  (let* ([obj (new cls%)])
    (set-data-object! con obj (query-row con (select-sql con obj (primary-key-where-clause con obj)) pkey))
    obj
    ))

;;; Select data-objects from the database
(define (select-data-objects con class% where-clause pkey)
  (let* ([obj (new class%)]
         [rows (query-rows con (select-sql con obj where-clause) pkey)]
         [objs (make-list (length rows) (new class%))])
    (map (lambda (o r) (set-data-object! con o r)) objs rows)
    objs
    ))
