#lang racket
 
(module racquel racket
      
(require db)
 
(provide data-object% data-class make-data-object select-data-object select-data-objects)

;;; SQL placeholder by database system.
(define (sql-placeholder con) (if (eq? (dbsystem-name (connection-dbsystem con)) 'postgres) "$1" "?"))

;;; SQL schema by database system.
(define (schema-sql con tbl-nm) 
  (cond [(eq? (dbsystem-name (connection-dbsystem con)) 'mysql) 
         (string-append "select cols.column_name, cols.data_type, cons.constraint_type
from information_schema.columns AS cols
left join information_schema.key_column_usage as fkey 
   on fkey.column_name = cols.column_name
   and fkey.table_name = cols.table_name
   and fkey.table_schema = cols.table_schema
left join information_schema.table_constraints AS cons
   on cons.constraint_name = fkey.constraint_name
   and cons.constraint_schema = fkey.constraint_schema
   and cons.table_name = fkey.table_name
   and cons.table_schema = fkey.table_schema
where cols.table_name = '" tbl-nm "'")]
        [(eq? (dbsystem-name (connection-dbsystem con)) 'sqlite3) (string-append "pragma table_info(" tbl-nm ");")]
        [else (string-append "select cols.column_name, cols.data_type, cons.constraint_type
from information_schema.columns as cols
left join information_schema.key_column_usage as keycols
  on keycols.column_name = cols.column_name
  and keycols.table_name = cols.table_name
  and keycols.table_schema = cols.table_schema
left join information_schema.table_constraints as cons
  ON cons.constraint_name  = keycols.constraint_name
  and cons.constraint_schema = cons.constraint_schema
where cols.table_name = '" tbl-nm "'")]
        ))
  
;;; Insert SQL.
(define (insert-sql con obj)
  ;; TODO: Exclude auto-increment key field from columns/values.
  (let ([col-nms (get-field column-names obj)])
    (string-append "insert " (get-field table-name obj)
                 " (" (string-join col-nms ", ") ")"
                 " values (" (string-join (make-list (length col-nms) "?") ", ") ")")))

;;; Update SQL.
; TODO: Exclude primary key column(s)/value(s) from set fields.
(define (update-sql con obj)
  (let ([values (foldr (lambda (f l) (cons (string-append f "=" (sql-placeholder con)) l)) 
                       '() (get-field column-names obj))])
    (string-append "update " (get-field table-name obj)
                 " set " (string-join values ", ")
                 " where " (get-field primary-key obj) "=" (sql-placeholder con))
    ))

;;; Delete SQL.
(define (delete-sql con obj)
  (string-append "delete " (get-field table-name obj)
                 " where " (get-field primary-key obj) "=" (sql-placeholder con)))

;;;; Data Object class.
(define data-object% 
  (class object%
         (init-field table-name
                     column-names
                     column-types
                     [primary-key (first column-names)]
                     [external-name table-name]
                     [new? #t]
                     [deleted? #f]
                     [class-name
                      ;; Inspect the derived class to get its name.
                      (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?)
                                    (class-info (let-values ([(cls x) (object-info this)]) cls))]) cls-nm)])
         (define/public (save con) (if new? (insert con) (update con)))
         (define/public (insert con) (let ([sql (insert-sql con this)])
                                   (query-exec con sql)
                                   (set-field! new? this #f)))
         (define/public (update con) (apply query-exec (append (list con) (dynamic-get-field column-names this) 
                                                               (list (dynamic-get-field (string->symbol (get-field primary-key this) this))))))
         (define/public (delete con) ((query-exec con (delete-sql con this) 
                                             (dynamic-get-field (string->symbol (get-field primary-key this) this)))
                                      (set-field! deleted? this #t)))
         (super-new)
         (inspect #f)
  ))

;;; Creates a class using database schema information.
(define (data-class con tbl-nm) 
    (let* ([schema (query-rows con (schema-sql con tbl-nm))]
           [flds (foldr (lambda (f l) (cons (list (string->symbol (vector-ref f 0)) #f) l)) '() schema)]
           [col-nms (foldr (lambda (f l) (cons (vector-ref f 0) l)) '() schema)]
           [col-types (foldr (lambda (f l) (cons (vector-ref f 1) l)) '() schema)]
           [key (vector-ref (findf (lambda (f) (string=? (vector-ref f 2) "PRIMARY KEY")) schema) 0)]
           [ext-nm tbl-nm])
  (eval `(class data-object%
         ,(append '(field) flds)
         (super-new [table-name ,tbl-nm]
                    [column-names ',col-nms]
                    [column-types ',col-types]
                    [primary-key ,key]
                    [external-name ,ext-nm])
         (inspect #f))
        )))

;;; Sets the data in a data object.
(define (set-data-object! con obj row)
  ((map (lambda (f d) (dynamic-set-field! (string->symbol f) obj d)) 
        (get-field column-names obj) (vector->list row))
   (set-field! new? obj #f)
   ))

;;; Select a data object from the database.
(define (select-data-object con class% where-clause &rest)
    (let* ([obj (new class%)]
           [sql (string-append "select " (string-join (get-field column-names obj) ", ")
                               " from " (get-field table-name obj) " t "
                               where-clause)])
      (set-data-object! con obj (query-row con sql &rest))
      obj
    ))

;;; Load a data object from the database by primary key.
(define (make-data-object con class% pkey)
  (let* ([obj (new class%)]
         [where-clause (string-append " where " (get-field primary-key obj) "=" (sql-placeholder con))])
    (select-data-object con where-clause pkey)
    ))

;;; Select data-objects from the database
(define (select-data-objects con class% where-clause &rest)
  (let* ([obj (new class%)]
         [sql (string-append "select " (string-join (get-field column-names obj) ", ")
                             " from " (get-field table-name obj) " t "
                             where-clause)]
         [rows (query-rows con sql &rest)]
         [objs (make-list (length rows) (new class%))])
    (map (lambda (o r) (set-data-object! con o r)) objs rows)
    objs
    ))

) ; module

