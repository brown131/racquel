#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test - Test module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013
(require rackunit rackunit/text-ui db racquel "metadata.rkt")

(require/expose racquel (savable-fields 
                         key-where-clause
                         primary-key-fields
                         insert-sql 
                         update-sql 
                         delete-sql 
                         select-sql
                         object-class))

;(require/expose "metadata.rkt" (data-class-metadata% *data-class-metadata*))
  
;;;; SETUP
 
;;; Test database connection
(define con (mysql-connect #:server "localhost" #:port 3306 #:database "racquel_test" #:user "test" #:password "test"))

;;;; TESTS

(define test-interface<%> (interface ()))

(define-test-suite test-define-data-object
 (let* ([test-class% (data-class* object% (test-interface<%>)
                                 (table-name "test")
                                 (column [id #f "id"] 
                                         [name #f "name"] 
                                         [description #f "description"])
                                 (init-column [x "x"])
                                 (join [object id object% id])
                                 (primary-key id)
                                 (define/public (test) (x + 1))
                                 (inspect #f)
                                 (super-new))]
        [obj (new test-class% [x 2])])
   (test-case "test class created?" (check-not-eq? test-class% #f))
   (test-true "test class is a data class?" (data-class? test-class%))
   (test-case "test object created?" (check-not-eq? obj #f))
   (test-case "test class info ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info test-class%)]) 
                (check-eq? cls-nm 'test-class%)
                (check-equal? fld-nms '(id name description object x))))
   
   (test-case "data object metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) 
                            (data-class-info test-class%)])
                (check-eq? tbl-nm "test")
                (check-equal? col-defs '((id . "id") (name . "name") (description . "description") (x . "x")))
                (check-eq? (length j-defs) 1)
                (check-eq? pkey 'id)
                (check-eq? auto-key #f)
                (check-eq? ext-nm "test")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class ok?" (check-equal? (object-class obj) test-class%))
  ; (test-case "get column name?" (check-eq? (get-column-name id test-class%) "id"))
   
   (test-case "columns set?" 
              (set-column! id obj 1)
              (set-column! name obj "Test")
              (set-column! description obj "This is a test")
              (check-eq? (get-column id obj) 1)
              (check-eq? (get-column name obj) "Test")
              (check-eq? (get-column description obj) "This is a test")
              (check-eq? (get-column object obj) #f))
   
   (test-case "savable field ok?" (check-equal? (savable-fields con test-class%) '(id name description x)))
   (test-case "primary key fields ok?" (check-equal? (primary-key-fields test-class%) '(id)))
   (test-case "where clause ok?" (check-equal? (key-where-clause con test-class% (primary-key-fields test-class%)) " where id=?"))
   (test-case "insert sql ok?" (check-equal? (insert-sql con test-class%) "insert test (id, name, description, x) values (?, ?, ?, ?)"))
   (test-case "update sql ok?" (check-equal? (update-sql con test-class%) "update test set id=?, name=?, description=?, x=? where id=?"))
   (test-case "delete sql ok?" (check-equal? (delete-sql con test-class%) "delete from test where id=?"))
   (test-case "select sql ok?" 
              (check-equal? (select-sql con test-class% "where id=?") "select id, name, description, x from test t where id=?"))
   )
)

(define (table-name-normalizer n) (string-downcase (string-append (regexp-replace* "([a-z])([A-Z])" n "\\1-\\2") "%")))

(define (column-name-normalizer n) (string-downcase (string-replace n "_" "-")))

(define-test-suite test-make-data-object
 (let* ([simple% (gen-data-class con "simple" 
                                  #:schema-name "racquel_test"
                                  #:generate-joins? #t
                                  #:table-name-normalizer table-name-normalizer
                                  #:column-name-normalizer column-name-normalizer)]
        [obj (new simple%)])
   (test-case "simple class created?" (check-not-eq? simple% #f))
   (test-true "simple class is a data class?" (data-class? simple%))
   (test-case "simple object created?" (check-not-eq? obj #f))
   (test-case "simple class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info simple%)]) 
                (check-eq? cls-nm 'simple%)))
   
   (test-case "simple class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info simple%)])
                (check-eq? tbl-nm "simple")
                (check-equal? col-defs '((id . "id") (x . "x") (name . "name") (description . "description")))
                (check-eq? (length j-defs) 0)
                (check-eq? pkey 'id)
                (check-eqv? auto-key #f)
                (check-eq? ext-nm "simple")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class ok?" (check-equal? (object-class obj) simple%))
   
   (test-case "savable field ok?" (check-equal? (savable-fields con simple%) '(id x name description)))
   (test-case "where clause ok?" (check-equal? (key-where-clause con simple% (primary-key-fields simple%)) " where id=?"))
   (test-case "insert sql ok?" (check-equal? (insert-sql con simple%) "insert simple (id, x, name, description) values (?, ?, ?, ?)"))
   (test-case "update sql ok?" (check-equal? (update-sql con simple%) "update simple set id=?, x=?, name=?, description=? where id=?"))
   (test-case "delete sql ok?" (check-equal? (delete-sql con simple%) "delete from simple where id=?"))
   (test-case "select sql ok?" 
              (check-equal? (select-sql con simple% "where id=?") "select id, x, name, description from simple t where id=?"))
  
   (test-case "columns set?"
              (set-column! id obj 23)
              (set-column! name obj "test")
              (set-column! description obj "this is a test")
              (set-column! x obj 1.7)
              (check-eq? (get-column id obj) 23)
              (check-eq? (get-column name obj) "test")
              (check-eq? (get-column description obj) "this is a test"))
              (check-eq? (get-column x obj) 1.7)
   
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
 (let* ([auto% (gen-data-class con "auto" #:schema-name "racquel_test")]
        [obj (new auto%)])
   (test-case "auto class created?" (check-not-eq? auto% #f))
   (test-true "auto class is a data class?" (data-class? auto%))
   (test-case "auto object created?" (check-not-eq? obj #f))
   (test-case "auto class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info auto%)]) 
                (check-eq? cls-nm 'auto%)))
   
   (test-case "auto class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info auto%)])
                (check-eq? tbl-nm "auto")
                (check-equal? col-defs '((id . "id") (name . "name") (description . "description")))
                (check-equal? j-defs null)
                (check-eq? pkey 'id)
                (check-eq? auto-key 'id)
                (check-eq? ext-nm "auto")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class ok?" (check-equal? (object-class obj) auto%))
   
   (test-case "savable field ok?" (check-equal? (savable-fields con auto%) '(name description)))
   (test-case "where clause ok?" (check-equal? (key-where-clause con auto% (primary-key-fields auto%)) " where id=?"))
   (test-case "insert sql ok?" (check-equal? (insert-sql con auto%) "insert auto (name, description) values (?, ?)"))
   (test-case "update sql ok?" (check-equal? (update-sql con auto%) "update auto set name=?, description=? where id=?"))
   (test-case "delete sql ok?" (check-equal? (delete-sql con auto%) "delete from auto where id=?"))
   (test-case "select sql ok?" 
              (check-equal? (select-sql con auto% "where id=?") "select id, name, description from auto t where id=?"))
  
   (test-case "columns set?"
              (set-column! name obj "test")
              (set-column! description obj "this is a test")
              (check-eq? (get-column name obj) "test")
              (check-eq? (get-column description obj) "this is a test")
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
   
   (test-case "object selected?"
              (let ([a (select-data-object con auto% (where (= name ?)) (get-field name obj))])
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
             (column (id 1 "id") (first-name #f "first_name") (last-name #f "last_name") (age #f "age"))
             (primary-key id #:autoincrement #t)
             (join (addresses id 'address% person-id))
             (super-new)
             (inspect #f))]
        [address% (data-class object% 
             (table-name "address") 
             (external-name "Address") 
             (column (id 1 "id") (person-id 1 "person_id") (line #f "line") (city #f "city") (state #f "state") (zip-code #f "zip_code"))
             (primary-key id #:autoincrement #t)
             (join (person person-id person% id 'one-to-one))
             (super-new)
             (inspect #f))]
        [person-obj (new person%)]
        [address-obj (new address%)])

   (test-case "person class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info person%)]) 
                (check-eq? cls-nm 'person%)
                (check-equal? fld-nms '(id first-name last-name age addresses))))
   
   (test-case "person class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info person%)])
                (check-eq? tbl-nm "person")
                (check-equal? col-defs '((id . "id") (first-name . "first_name") (last-name . "last_name") (age . "age")))
                (check-equal? (map car j-defs) '(addresses))
                (check-equal? (data-join-foreign-key (first (map cdr j-defs))) 'id)
                (check-equal? (data-join-class (first (map cdr j-defs))) 'address%)
                (check-equal? (data-join-key (first (map cdr j-defs))) 'person-id)
                (check-equal? (data-join-cardinality (first (map cdr j-defs))) 'one-to-many)
                (check-eq? pkey 'id)
                (check-eq? auto-key 'id)
                (check-eq? ext-nm "Person")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "addresses not joined?" (check-eq? (get-field addresses person-obj) #f))
   (test-case "addresses joined?" (check-true (is-a? (first (get-join addresses person-obj con)) address%)))

   (test-case "address class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info address%)]) 
                (check-eq? cls-nm 'address%)
                (check-equal? fld-nms '(id person-id line city state zip-code person))))
   
   (test-case "address class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info address%)])
                (check-eq? tbl-nm "address")
                (check-equal? col-defs '((id . "id") (person-id . "person_id") (line . "line") (city . "city")
                                         (state . "state") (zip-code . "zip_code")))
                (check-equal? (map car j-defs) '(person))
                (check-equal? (data-join-foreign-key (first (map cdr j-defs))) 'person-id)
                (check-equal? (data-join-class (first (map cdr j-defs))) person%)
                (check-equal? (data-join-key (first (map cdr j-defs))) 'id)
                (check-equal? (data-join-cardinality (first (map cdr j-defs))) 'one-to-one)
                (check-eq? pkey 'id)
                (check-eq? auto-key 'id)
                (check-eq? ext-nm "Address")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "person not joined?" (check-eq? (get-field person address-obj) #f))
   (test-case "person joined?" (check-true (is-a? (get-join person address-obj con) person%)))
))

(define-test-suite test-generate-join
 (let* ([address% (gen-data-class con "address" 
                                   #:schema-name "racquel_test"
                                   #:table-name-normalizer table-name-normalizer
                                   #:column-name-normalizer column-name-normalizer)]
        [obj (new address%)])
   (test-case "generated class ok?" (check-equal? (gen-data-class con "address" #:print? #t
                                                                   #:schema-name "racquel_test"
                                                                   #:table-name-normalizer table-name-normalizer
                                                                   #:column-name-normalizer column-name-normalizer)
                                                  '(let ((address%
                                                          (data-class
                                                           object%
                                                           (table-name "address")
                                                           (external-name "address")
                                                           (column
                                                            (id #f "id")
                                                            (person-id #f "person_id")
                                                            (zip-code #f "zip_code")
                                                            (state #f "state")
                                                            (line #f "line")
                                                            (city #f "city"))
                                                           (primary-key id #:autoincrement #t)
                                                           (join (person "person_id" "person" "id"))
                                                           (super-new)
                                                           (inspect #f))))
                                                     address%)))
   (test-case "address class created?" (check-not-eq? address% #f))
   (test-true "address class is a data class?" (data-class? address%))
   (test-case "address object created?" (check-not-eq? obj #f))
   (test-case "address class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info address%)]) 
                (check-eq? cls-nm 'address%)))
   
   (test-case "address class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info address%)])
                (check-eq? tbl-nm "address")
                (check-equal? col-defs '((id . "id") (person-id . "person_id") (zip-code . "zip_code")
                                         (state . "state") (line . "line") (city . "city")))
                (check-equal? (caar j-defs) 'person)
                (check-eq? pkey 'id)
                (check-eqv? auto-key 'id)
                (check-eq? ext-nm "address")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class ok?" (check-equal? (object-class obj) address%))
))

(define-test-suite test-generate-reverse-join
 (let* ([person% (gen-data-class con "person" 
                                 #:schema-name "racquel_test"
                                 #:generate-reverse-joins? #t
                                 #:table-name-normalizer table-name-normalizer
                                 #:column-name-normalizer column-name-normalizer)]
        [obj (new person%)])
   (test-case "person class created?" (check-not-eq? person% #f))
   (test-true "person class is a data class?" (data-class? person%))
   (test-case "person object created?" (check-not-eq? obj #f))
   (test-case "person class ok?" 
              (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info person%)]) 
                (check-eq? cls-nm 'person%)))
   
   (test-case "person class metadata set?" 
              (let-values ([(tbl-nm col-defs j-defs pkey auto-key ext-nm st-key) (data-class-info person%)])
                (check-eq? tbl-nm "person")
                (check-equal? col-defs '((id . "id") (last-name . "last_name") (first-name . "first_name") (age . "age")))
                (check-equal? (caar j-defs) 'address)
                (check-eq? pkey 'id)
                (check-eqv? auto-key #f)
                (check-eq? ext-nm "person")
                (check-not-eq? st-key #f)
                ))
   
   (test-case "object class ok?" (check-equal? (object-class obj) person%))
))

(define-test-suite test-rql-parsing
   (let* ([address% (gen-data-class con "address" 
                                    #:schema-name "racquel_test"
                                    #:table-name-normalizer table-name-normalizer
                                    #:column-name-normalizer column-name-normalizer)]
          [obj (new address%)])
     (test-case "select sql ok?" (check-equal? (select-data-object con address% #:print? #t (where (= id 1))) 
                                               "select id, person_id, zip_code, state, line, city from address t where id = 1"))
     (test-case "rql select runs?" 
                (check-true (is-a? (select-data-object con address% (where (and (= id ?) (= city ?))) 1 "Chicago") address%)))
     (test-case "selected with rql?"
                (let ([a (select-data-object con address% (where (= id 1)))])
                  (check-equal? (get-field city a) "Chicago")
                  (check-eq? (data-object-state a) 'loaded)))
     (test-case "selected with sql?"
                (let ([a (select-data-object con address% (string-append "where id = " "1"))])
                  (check-equal? (get-field city a) "Chicago")
                  (check-eq? (data-object-state a) 'loaded)))
     (test-case "select join sql ok?" (check-equal? (select-data-object con address% #:print? #t 
                                                                        (join person (= (person id) person_id)) (where (= id 1)))
                                                    "select id, person_id, zip_code, state, line, city from address t \
join person on person.id = person_id where id = 1"))
     
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (= city ?)))
"select id, person_id, zip_code, state, line, city from address t where city = ?"))
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (<> city ?)))
"select id, person_id, zip_code, state, line, city from address t where city <> ?"))
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (>= city ?)))
"select id, person_id, zip_code, state, line, city from address t where city >= ?"))
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (<= city ?)))
"select id, person_id, zip_code, state, line, city from address t where city <= ?"))
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (> city ?)))
"select id, person_id, zip_code, state, line, city from address t where city > ?"))
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (< city ?)))
"select id, person_id, zip_code, state, line, city from address t where city < ?"))                
     (test-case "select like ok?" (check-equal? (select-data-object con address% #:print? #t (where (like city ?)))
"select id, person_id, zip_code, state, line, city from address t where city like ?"))
     (test-case "select quote ok?" (check-equal? (select-data-object con address% #:print? #t 
                                                                     (where (in id ,(make-list 3 "?"))))
                                                 "select id, person_id, zip_code, state, line, \
city from address t where id in (?,?,?)"))
     ))


(define-test-suite test-mixins
  (let* ([test-class% (data-class object%
                                 (table-name "test")
                                 (column [id 1 "id"] 
                                         [name "test" "name"] 
                                         [description "Test" "description"])
                                 (primary-key id)
                                 (inspect #f)
                                 (super-new))]
         [test-mixed-class% (json-data-class-mixin test-class%)]
         [test-mixed-object (new test-mixed-class%)])
    (test-case "externalized ok?" (check-equal? (send test-mixed-object externalize) 
"{\"test-mixed-class%\":{\"id\":1,\"name\":\"test\",\"description\":\"Test\"}}"))
    (test-case "internalized ok?" (check-equal? (send (new test-mixed-class%) internalize (send test-mixed-object externalize)) #f))
  ))

(run-tests test-define-data-object 'verbose)
(run-tests test-make-data-object 'verbose)
(run-tests test-autoincrement-data-object 'verbose)
(run-tests test-joins 'verbose)
(run-tests test-generate-join 'verbose)
(run-tests test-rql-parsing 'verbose)
(run-tests test-mixins 'verbose)

(disconnect con)
