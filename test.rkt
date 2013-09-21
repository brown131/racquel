#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test - Tests the project
;;;;
;;;; Copyright (c) Scott Brown 2013
(require rackunit rackunit/text-ui db racquel)

(require/expose racquel (savable-fields 
                         primary-key-where-clause 
                         insert-sql 
                         update-sql 
                         delete-sql 
                         select-sql))

(require/expose "metadata.rkt" (data-class-metadata% *data-class-metadata*))
  
;;;; SETUP
 
;;; Test database connection
(define con (mysql-connect #:server "localhost" #:port 3306 #:database "racquel_test" #:user "test" #:password "test"))

;;;; TESTS
#|
(data-class data-object%
            (table-name "test")
            (column [id #f "id"] [name #f "name"] [description #f "description"])
            (primary-key "id")
            (inspect #f)
            (super-new)
            )
|#
(define-test-suite test-define-data-object
 (let* ([test-class% (data-class data-object%
                                 (table-name "test")
                                 (column [id #f "id"] [name #f "name"] [description #f "description"])
                                 (primary-key "id")
                                 (inspect #f)
                                 (super-new)
                                 )]
        [obj (new test-class%)])
   (test-case "test class created?" (check-not-eq? test-class% #f))
   (test-case "test object created?" (check-not-eq? obj #f))
   
   (test-case "data object metadata set?" 
              (let-values ([(tbl-nm col-nms pkey auto-key ext-nm cls-nm) (data-class-info test-class%)])
                (check-eq? tbl-nm "test")
                (check-equal? col-nms '("id" "name" "description"))
                (check-eq? pkey "id")
                (check-eq? auto-key #f)
                (check-eq? ext-nm "test")
                (check-eq? cls-nm 'test-class%)
                ))
   
   (test-case "fields set?" 
              (set-field! id obj 1)
              (set-field! name obj "Test")
              (set-field! description obj "This is a test")
              (check-eq? (get-field id obj) 1)
              (check-eq? (get-field name obj) "Test")
              (check-eq? (get-field description obj) "This is a test"))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con obj) '("name" "description")))
   (test-case "where clause correct?" (check-equal? (primary-key-where-clause con obj) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con obj) "insert test (name, description) values (?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con obj) "update test set name=?, description=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con obj) "delete from test where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con obj "where id=?") "select id, name, description from test t where id=?"))
   )
)

(define-test-suite test-make-data-object
 (let* ([simple% (gen-data-class con "simple")]
        [obj (new simple%)])
   (test-case "simple object created?" (check-not-eq? obj #f))
  
   (test-case "fields set?"
              (set-field! name obj "test")
              (set-field! description obj "this is a test")
              (check-eq? (get-field name obj) "test")
              (check-eq? (get-field description obj) "this is a test"))
   
   (test-case "object inserted?" 
              (send obj insert con)
              (check-not-eq? (get-field id obj) #f))
                 
   (test-case "object changed?" 
              (set-field! name obj "test2")
              (check-eq? (get-field name obj) "test2"))
   
   (test-case "object updated?"
              (send obj update con)
              (check-equal? (query-value con "select name from simple where id=?" (get-field id obj)) "test2"))  
   
   (test-case "object loaded?"
              (let ([s (make-data-object con simple% (get-field id obj))])
                (check-equal? (get-field name s) "test2")))
   
   (test-case "object deleted?" 
              (send obj delete con)
              (check-eq? (query-value con "select count(*) from simple where id=?" (get-field id obj)) 0)) 
   ))

#|
(define ds (data-class data-object% 
             (table-name "test") 
             (external-name "Test") 
             (column (name #f "name") (color #f "color"))
             (field (x 1) (y 2))
             (super-new)
             (inspect #f)))
ds
|#

(define-test-suite test-data-class
 (let* ([cls (data-class data-object% 
             (table-name "test") 
             (external-name "Test") 
             (column (name #f "name") (color #f "color"))
             (field (x 1) (y 2))
             (super-new)
             (inspect #f))])
   (test-true "data class parsed?" (class? cls))
  ; cls
))

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)
(run-tests test-data-class 'verbose)
  
(disconnect con)
