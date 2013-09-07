#lang racket

(require db)

;;;; Data Object
(define data-object% 
  (class object%
         (init-field table-name 
                     column-names 
                     column-types
                     [primary-key (first column-names)]
                     [external-name table-name])
         (field [class-name 
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

# Creates a class using database schema information.
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
           [col-nms (foldr (lambda (f l) (cons (list (vector-ref f 0) #f) l)) '() schema)]
           [col-types (foldr (lambda (f l) (cons (vector-ref f 1) l)) '() schema)]
           [key (vector-ref (findf (lambda (f) (string=? (vector-ref f 2) "PRIMARY KEY")) schema) 0)])
  (eval `(class data-object%
         ,(append '(field) flds)
         (super-new [table-name ,tbl-nm]
                    [column-names ,col-nms]
                    [column-types ,col-types]
                    [primary-key ,key]
                    )
         (inspect #f))
    ))
))

   (define phrase-type% (data-class con "phrasetype"))
                                                            
# Load a data object from the database.
(define make-data-object (connection primary-key) #t)                            

# Load a data object from the database.
(define select-data-object (connection query-string) #t)                            

# Select data-objects from the database
(define select-data-objects (connection query-string) #t)                            
                               
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