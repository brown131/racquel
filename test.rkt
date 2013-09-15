#lang racket

(require rackunit rackunit/text-ui db racquel)

(require/expose racquel (savable-fields 
                         primary-key-where-clause 
                         insert-sql 
                         update-sql 
                         delete-sql 
                         select-sql))

;;;; SETUP
 
;;; Test database connection
(define con (mysql-connect #:server "localhost" #:port 3306 #:database "racquel_test" #:user "root" #:password "wurzel"))

;;; Test object class
(define test-object% (class data-object% 
          (field (id #f) 
                 (name #f) 
                 (description #f)) 
          (super-new (table-name "test")
                     (column-names '("id" "name" "description")) 
                     (primary-key "id"))
          (inspect #f))
  )

;;;; TESTS

(define-test-suite test-define-data-object
 (let* ([obj (new test-object%)])
   (test-case "test object created?" (check-not-eq? test-object% #f))
   
   (test-case "super fields set?" 
              (check-eq? (get-field table-name obj) "test")
              (check-equal? (get-field column-names obj) '("id" "name" "description"))
              (check-eq? (get-field primary-key obj) "id"))
   
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
 (let* ([simple% (data-class con "simple")]
        [obj (new simple%)]
        [id #f])
   (test-case "simple object created?" (check-not-eq? obj #f))
   
  
   (test-case "fields set?"
              (set-field! name obj "test")
              (set-field! description obj "this is a test")
              (check-eq? (get-field name obj) "test"))
   
   
   (test-case "object inserted?" 
              (send obj insert con)
              (check-not-eq? (get-field id obj) #f))
   
   
   (test-case "object changed?" 
              (set-field! name obj "test2")
              (check-eq? (get-field name obj) "test2"))
   
   (test-case "object updated?"
              (send obj update con)
              (check-equal? (query-value con "select name from simple where id=?" (get-field id obj)) "test2"))  
   
   
   (test-case "object deleted?" 
              (send obj delete con)
              (check-eq? (query-value con "select count(*) from simple where id=?" (get-field id obj)) 0)) 
   ))

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)

(disconnect con)
