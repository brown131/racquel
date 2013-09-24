#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test - Tests the project
;;;;
;;;; Copyright (c) Scott Brown 2013
(require rackunit rackunit/text-ui db racquel)

(require/expose racquel (savable-fields 
                         primary-key-where-clause 
                         schema-sql ;
                         find-primary-key-fields ;
                         insert-sql 
                         update-sql 
                         delete-sql 
                         select-sql
                         object-class))

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
   (test-true "test class is a data class?" (data-class? test-class%))
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
   
   (test-case "object class correct?" (check-equal? (object-class obj) test-class%))
   
   (test-case "fields set?" 
              (set-field! id obj 1)
              (set-field! name obj "Test")
              (set-field! description obj "This is a test")
              (check-eq? (get-field id obj) 1)
              (check-eq? (get-field name obj) "Test")
              (check-eq? (get-field description obj) "This is a test"))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con test-class%) '("id" "name" "description")))
   (test-case "where clause correct?" (check-equal? (primary-key-where-clause con test-class%) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con test-class%) "insert test (id, name, description) values (?, ?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con test-class%) "update test set id=?, name=?, description=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con test-class%) "delete from test where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con test-class% "where id=?") "select id, name, description from test t where id=?"))
   )
)

(define-test-suite test-make-data-object
 (let* ([simple% (gen-data-class con "simple")]
        [obj (new simple%)])
   (test-case "simple class created?" (check-not-eq? simple% #f))
   (test-true "simple class is a data class?" (data-class? simple%))
   (test-case "simple object created?" (check-not-eq? obj #f))
   
   (test-case "simple class metadata set?" 
              (let-values ([(tbl-nm col-nms pkey auto-key ext-nm cls-nm) (data-class-info simple%)])
                (check-eq? tbl-nm "simple")
                (check-equal? col-nms '("name" "description" "id"))
                (check-eq? pkey "id")
                (check-eq? auto-key #f)
                (check-eq? ext-nm "simple")
                (check-eq? cls-nm 'simple%)
                ))
   
   (test-case "object class correct?" (check-equal? (object-class obj) simple%))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con simple%) '("name" "description" "id")))
   (test-case "where clause correct?" (check-equal? (primary-key-where-clause con simple%) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con simple%) "insert simple (name, description, id) values (?, ?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con simple%) "update simple set name=?, description=?, id=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con simple%) "delete from simple where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con simple% "where id=?") "select name, description, id from simple t where id=?"))
  
   (test-case "fields set?"
              (set-field! id obj 23)
              (set-field! name obj "test")
              (set-field! description obj "this is a test")
              (check-eq? (get-field id obj) 23)
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

(define-test-suite test-autoincrement-data-object
 (let* ([auto% (gen-data-class con "auto")]
        [obj (new auto%)])
   (test-case "auto class created?" (check-not-eq? auto% #f))
   (test-true "auto class is a data class?" (data-class? auto%))
   (test-case "auto object created?" (check-not-eq? obj #f))
   
   (test-case "auto class metadata set?" 
              (let-values ([(tbl-nm col-nms pkey auto-key ext-nm cls-nm) (data-class-info auto%)])
                (check-eq? tbl-nm "auto")
                (check-equal? col-nms '("name" "description" "id"))
                (check-eq? pkey "id")
                (check-eq? auto-key "id")
                (check-eq? ext-nm "auto")
                (check-eq? cls-nm 'auto%)
                ))
   
   (test-case "object class correct?" (check-equal? (object-class obj) auto%))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con auto%) '("name" "description")))
   (test-case "where clause correct?" (check-equal? (primary-key-where-clause con auto%) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con auto%) "insert auto (name, description) values (?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con auto%) "update auto set name=?, description=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con auto%) "delete from auto where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con auto% "where id=?") "select name, description, id from auto t where id=?"))
  
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
              (check-equal? (query-value con "select name from auto where id=?" (get-field id obj)) "test2"))  
   
   (test-case "object loaded?"
              (let ([s (make-data-object con auto% (get-field id obj))])
                (check-equal? (get-field name s) "test2")))
   
   (test-case "object deleted?" 
              (send obj delete con)
              (check-eq? (query-value con "select count(*) from auto where id=?" (get-field id obj)) 0)) 
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
             (primary-key "name" #:autoincrement #t)
             (field (x 1) (y 2))
             (super-new)
             (inspect #f))])
   (test-true "data class parsed?" (class? cls))
  ; cls
))

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)
(run-tests test-autoincrement-data-object 'verbose)
(run-tests test-data-class 'verbose)
  
(disconnect con)
