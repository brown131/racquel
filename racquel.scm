#lang racket

(require db)

;;;; Data Object
(define data-object% 
  (class object%
         (init-field table-name
                     column-names
                     column-types
                     [primary-key (first column-names)]
                     [external-name table-name]
                     [class-name
                      ;; Inspect the derived class to get its name.
                      (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?)
                                    (class-info (let-values ([(cls x) (object-info this)]) cls))]) cls-nm)])
         (define/public (save) (#t))
         (define/public (insert) (#t))
         (define/public (update) (#t))
         (define/public (delete) (#t))
         (super-new)
         (inspect #f)
  )
)

;;; Creates a class using database schema information.
(define data-class 
  (lambda (con tbl-nm) 
    (let* ([schema (query-rows con "select cols.column_name, cols.data_type, cons.constraint_type
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
where cols.table_name = ?" tbl-nm)]
           [flds (foldr (lambda (f l) (cons (list (string->symbol (vector-ref f 0)) #f) l)) '() schema)]
           [col-nms (foldr (lambda (f l) (cons (vector-ref f 0) l)) '() schema)]
           [col-types (foldr (lambda (f l) (cons (vector-ref f 1) l)) '() schema)]
           [key (vector-ref (findf (lambda (f) (string=? (vector-ref f 2) "PRIMARY KEY")) schema) 0)]
           [cls-nm (string-append tbl-nm "%")]
           )
  (eval `(class data-object%
         ,(append '(field) flds)
         (super-new [table-name ,tbl-nm]
                    [column-names ',col-nms]
                    [column-types ',col-types]
                    [primary-key ,key]
                    [class-name ,cls-nm]
                    )
         (inspect #f))
    ))
))

   (define phrase-type% (data-class con "phrasetype"))
   (define pt (new phrase-type%))
                
   (class data-object% 
          (field (id #f) 
                 (name #f) 
                 (description #f)) 
          (super-new 
           (table-name "phrasetype")
           (column-names '("id" "name" "description")) 
           (column-types '("int" "varchar" "varchar")) 
           (primary-key "id")) (inspect #f))
   
;;; Load a data object from the database.
(define make-data-object 
  (lambda (com class% pkey)
    (let* ([obj (new class%)]
           [sql (string-append "select " (string-join (get-field column-names obj) ", ")
                               " from " (get-field table-name obj)
                               " where " (get-field primary-key obj) "=" 
                               (if (eq? (dbsystem-name (connection-dbsystem con)) 'postgres) "$1" "?"))]
           [obj-data (query-row con sql pkey)])
      (map (lambda (f d) (dynamic-set-field! (string->symbol f) obj d)) (get-field column-names obj) (vector->list obj-data))
      obj
    )
  ))
  
# Load a data object from the database.
(define select-data-object                           
  (lambda (com class% where-clause &rest)
    (let* ([obj (new class%)]
           [sql (string-append "select " (string-join (get-field column-names obj) ", ")
                               " from " (get-field table-name obj) " "
                               where-clause)]
           [obj-data (query-row con sql &rest)])
      (map (lambda (f d) (dynamic-set-field! (string->symbol f) obj d)) (get-field column-names obj) (vector->list obj-data))
      obj
    )
  ))

# Select data-objects from the database
(define select-data-objects                          
  (lambda (com class% where-clause &rest)
    (let* ([sql (string-append "select " (string-join (get-field column-names obj) ", ")
                               " from " (get-field table-name obj) " "
                               where-clause)]
           [obj-data (query-rows con sql &rest)]
           [objs (make-list (length obj-data) (new class%))])
      (map (lambda (r o) (map (lambda (f d) (dynamic-set-field! (string->symbol f) o d)) 
                                (get-field column-names obj) (vector->list r)))
           obj-data objs)
      objs
    )
  ))                           

(select-data-objects con phrase-type% "where id>?" 1)

(select-data-object con phrase-type% "where name=?" "Verb Phrase")

(define con (mysql-connect #:server "localhost" #:port 3306 #:database "babelbuilder" #:user "root" #:password "wurzel"))
(define phrase-type% (data-class con "phrasetype"))
(disconnect con)                             

(define affix-type% 
  (class data-object%     
         (field [id 0] [name ""] [description ""]) 
         (super-new [table-name "affixtype"]
                    [column-names '(id name description)]
                    [column-types '(exact-integer? string? string?)]
                    )
         (inspect #f)
   )
)

(define obj (new affix-type%))


(get-field column-names obj)

(set-field! name obj "crow")
(get-field name obj)