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
          (super-new 
           (table-name "test")
           (column-names '("id" "name" "description")) 
           (primary-key "id"))
          (inspect #f))
  )


;;;; TESTS

(define-test-suite test-define-data-object
 (let* ([obj (new test-object%)])
   (set-field! id obj 1)
   (set-field! name obj "Test")
   (set-field! description obj "This is a test")
   
   (check-not-eq? test-object% #f)
   (check-eq? (get-field table-name obj) "test")
   
   (check-equal? (savable-fields con obj) '("name" "description"))
   (check-equal? (primary-key-where-clause con obj) " where id=?")
   (check-equal? (insert-sql con obj) "insert test (name, description) values (?, ?)")
   (check-equal? (update-sql con obj) "update test set name=?, description=? where id=?")
   (check-equal? (delete-sql con obj) "delete from test where id=?")
   (check-equal? (select-sql con obj "where id=?") "select id, name, description from test t where id=?")  
   )
)

(define-test-suite test-make-data-object
 (let* ([simple% (data-class con "simple")]
        [obj (new simple%)]
        [id #f])
   (check-not-eq? obj #f)
   (set-field! name obj "test")
   (check-eq? (get-field name obj) "test")
   (set-field! description obj "this is a test")
   (send obj insert con)
   (set-field! id obj (query-value con "select last_insert_id()"))
   (check-not-eq? (get-field id obj) #f)
   (send obj delete con)
     
   )
)

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)

(disconnect con)
