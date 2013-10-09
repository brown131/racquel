#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test - Tests the project
;;;;
;;;; Copyright (c) Scott Brown 2013
(require rackunit rackunit/text-ui db racquel)

(require/expose racquel (savable-fields 
                         key-where-clause
                         primary-key-fields
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
(data-class object%
            (table-name "test")
            (column [id #f "id"] [name #f "name"] [description #f "description"])
            (primary-key id)
            (join (object id object% id))
            (inspect #f)
            (super-new))

(define-test-suite test-define-data-object
 (let* ([test-class% (data-class object%
                                 (table-name "test")
                                 (column [id #f "id"] 
                                         [name #f "name"] 
                                         [description #f "description"])
                                 (init-column [x "x"])
                                 (join [object id object% id])
                                 (primary-key id)
                                 (inspect #f)
                                 (super-new))]
        [obj (new test-class% [x 2])])
   (test-case "test class created?" (check-not-eq? test-class% #f))
   (test-true "test class is a data class?" (data-class? test-class%))
   (test-case "test object created?" (check-not-eq? obj #f))
   (test-case "test class is correct?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info test-class%)]) 
                (check-eq? cls-nm 'test-class%)
                (check-equal? fld-nms '(id name description object x))))

   (test-case "data object metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) 
                            (data-class-info test-class%)])
                (check-eq? tbl-nm "test")
                (check-equal? col-defs (make-hash '((x . "x") (description . "description") (id . "id") (name . "name"))))
                (check-eq? (hash-count j-defs) 1)
                (check-eq? pkey 'id)
                (check-eq? auto-key #f)
                (check-eq? ext-nm "test")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class correct?" (check-equal? (object-class obj) test-class%))
   
   (test-case "fields set?" 
              (set-field! id obj 1)
              (set-field! name obj "Test")
              (set-field! description obj "This is a test")
              (check-eq? (get-field id obj) 1)
              (check-eq? (get-field name obj) "Test")
              (check-eq? (get-field description obj) "This is a test")
              (check-eq? (get-field object obj) #f))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con test-class%) '(x description id name)))
   (test-case "where clause correct?" (check-equal? (key-where-clause con test-class% (primary-key-fields test-class%)) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con test-class%) "insert test (x, description, id, name) values (?, ?, ?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con test-class%) "update test set x=?, description=?, id=?, name=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con test-class%) "delete from test where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con test-class% "where id=?") "select x, description, id, name from test t where id=?"))
   )
)

(define-test-suite test-make-data-object
 (let* ([simple% (gen-data-class con "simple")]
        [obj (new simple%)])
   (test-case "simple class created?" (check-not-eq? simple% #f))
   (test-true "simple class is a data class?" (data-class? simple%))
   (test-case "simple object created?" (check-not-eq? obj #f))
   (test-case "simple class is correct?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info simple%)]) 
                (check-eq? cls-nm 'simple%)))
   
   (test-case "simple class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info simple%)])
                (check-eq? tbl-nm "simple")
                (check-equal? col-defs (make-hash '((x . "x") (description . "description") (id . "id") (name . "name"))))
                (check-eq? (hash-count j-defs) 0)
                (check-eq? pkey 'id)
                (check-eqv? auto-key #f)
                (check-eq? ext-nm "simple")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class correct?" (check-equal? (object-class obj) simple%))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con simple%) '(x description id name)))
   (test-case "where clause correct?" (check-equal? (key-where-clause con simple% (primary-key-fields simple%)) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con simple%) "insert simple (x, description, id, name) values (?, ?, ?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con simple%) "update simple set x=?, description=?, id=?, name=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con simple%) "delete from simple where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con simple% "where id=?") "select x, description, id, name from simple t where id=?"))
  
   (test-case "fields set?"
              (set-field! id obj 23)
              (set-field! name obj "test")
              (set-field! description obj "this is a test")
              (set-field! x obj 1.7)
              (check-eq? (get-field id obj) 23)
              (check-eq? (get-field name obj) "test")
              (check-eq? (get-field description obj) "this is a test"))
              (check-eq? (get-field x obj) 1.7)
   
   (test-case "object inserted?" 
              (insert-data-object con obj)
              (check-not-eq? (get-field id obj) #f))
                 
   (test-case "object changed?" 
              (set-field! name obj "test2")
              (check-eq? (get-field name obj) "test2"))
   
   (test-case "object updated?"
              (update-data-object con obj)
              (check-equal? (query-value con "select id from simple where id=?" (get-field id obj)) 23)
              (check-equal? (query-value con "select name from simple where id=?" (get-field id obj)) "test2")
              (check-equal? (query-value con "select description from simple where id=?" (get-field id obj)) "this is a test")
              (check-equal? (query-value con "select x from simple where id=?" (get-field id obj)) 1.7)
              )  
   
   (test-case "object loaded?"
              (let ([s (make-data-object con simple% (get-field id obj))])
                (check-equal? (get-field id s) 23)
                (check-equal? (get-field name s) "test2")
                (check-equal? (get-field description s) "this is a test")
                (check-equal? (get-field x s) 1.7)))
   
   (test-case "object deleted?" 
              (delete-data-object con obj)
              (check-eq? (query-value con "select count(*) from simple where id=?" (get-field id obj)) 0)) 
   ))

(define-test-suite test-autoincrement-data-object
 (let* ([auto% (gen-data-class con "auto")]
        [obj (new auto%)])
   (test-case "auto class created?" (check-not-eq? auto% #f))
   (test-true "auto class is a data class?" (data-class? auto%))
   (test-case "auto object created?" (check-not-eq? obj #f))
   (test-case "auto class is correct?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info auto%)]) 
                (check-eq? cls-nm 'auto%)))
   
   (test-case "auto class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info auto%)])
                (check-eq? tbl-nm "auto")
                (check-equal? col-defs (make-hash '((description . "description") (id . "id") (name . "name"))))
                (check-eq? (hash-count j-defs) 0)
                (check-eq? pkey 'id)
                (check-eq? auto-key 'id)
                (check-eq? ext-nm "auto")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class correct?" (check-equal? (object-class obj) auto%))
   
   (test-case "savable field correct?" (check-equal? (savable-fields con auto%) '(description name)))
   (test-case "where clause correct?" (check-equal? (key-where-clause con auto% (primary-key-fields auto%)) " where id=?"))
   (test-case "insert sql correct?" (check-equal? (insert-sql con auto%) "insert auto (description, name) values (?, ?)"))
   (test-case "update sql correct?" (check-equal? (update-sql con auto%) "update auto set description=?, name=? where id=?"))
   (test-case "delete sql correct?" (check-equal? (delete-sql con auto%) "delete from auto where id=?"))
   (test-case "select sql correct?" 
              (check-equal? (select-sql con auto% "where id=?") "select description, id, name from auto t where id=?"))
  
   (test-case "fields set?"
              (set-field! name obj "test")
              (set-field! description obj "this is a test")
              (check-eq? (get-field name obj) "test")
              (check-eq? (get-field description obj) "this is a test")
              (check-eq? (data-object-state obj) 'new))
   
   (test-case "object inserted?" 
              (insert-data-object con obj)
              (check-not-eq? (get-field id obj) #f)
              (check-eq? (data-object-state obj) 'saved))
                 
   (test-case "object changed?" 
              (set-field! name obj "test2")
              (check-eq? (get-field name obj) "test2"))
   
   (test-case "object updated?"
              (update-data-object con obj)
              (check-equal? (query-value con "select name from auto where id=?" (get-field id obj)) "test2"))  
   
   (test-case "object loaded?"
              (let ([a (make-data-object con auto% (get-field id obj))])
                (check-equal? (get-field name a) "test2")
                (check-eq? (data-object-state a) 'loaded)))
   
   (test-case "object deleted?" 
              (delete-data-object con obj)
              (check-eq? (query-value con "select count(*) from auto where id=?" (get-field id obj)) 0)
              (check-eq? (data-object-state obj) 'deleted)) 
   ))

(define-test-suite test-joins
 (let* ([person% (data-class object% 
             (table-name "person") 
             (external-name "Person") 
             (column (id #f "id") (first-name #f "first_name") (last-name #f "last_name") (age #f "age"))
             (primary-key id #:autoincrement #t)
             ;(join (addresses id address% person-id))
             (super-new)
             (inspect #f))]
        [address% (data-class object% 
             (table-name "address") 
             (external-name "Address") 
             (column (id #f "id") (person-id 1 "person_id") (line #f "line") (city #f "city") (state #f "state") (zip-code #f "zip_code"))
             (primary-key id #:autoincrement #t)
             (join (person person-id person% id))
             (super-new)
             (inspect #f))]
        [person-obj (new person%)]
        [address-obj (new address%)])

   (test-case "address class is correct?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info address%)]) 
                (check-eq? cls-nm 'address%)
                (check-equal? fld-nms '(id person-id line city state zip-code person))))
   
   (test-case "address class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info address%)])
                (check-eq? tbl-nm "address")
                (check-equal? col-defs (make-hash '((person-id . "person_id") (state . "state") (city . "city")
                                                    (line . "line") (id . "id") (zip-code . "zip_code"))))
                (check-equal? (hash-keys j-defs) '(person))
                (check-eq? pkey 'id)
                (check-eq? auto-key 'id)
                (check-eq? ext-nm "Address")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "person not joined?" (check-eq? (get-field person address-obj) #f))
    
   (test-case "person joined?" (check-true (is-a? (get-joined-data-object person address-obj con) person%)))
))

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)
(run-tests test-autoincrement-data-object 'verbose)
(run-tests test-joins 'verbose)

(disconnect con)
