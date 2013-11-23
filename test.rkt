#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test - Test module for the project
;;;;
;;;; Copyright (c) Scott Brown 2013
(require rackunit rackunit/text-ui racket/trace db json racquel "metadata.rkt" "schema.rkt" "util.rkt")

(require/expose racquel (savable-fields 
                         key-where-clause-sql
                         primary-key-fields
                         insert-sql 
                         update-sql 
                         delete-sql
                         join-cardinality
                         get-schema-columns
                         key-where-clause-rql
                         get-join-schema
                         get-schema-joins
                         find-primary-key-fields
                         create-data-object
                         has-autoincrement-key?))
 

;;;; SETUP
 

;;; Database system to test.
(define *test-dbsys-type* 
  ;'mysql
  ;'postgresql
  ;'sqlite3
  ;'sqlserver
  'oracle
  ;'db2
  )

(when (equal? *test-dbsys-type* 'oracle) (set-odbc-dbsystem-type! *test-dbsys-type*))
(when (equal? *test-dbsys-type* 'db2) (set-odbc-dbsystem-type! *test-dbsys-type*))

;;; Database connection for testing.
(define *con* 
  (cond [(eq? *test-dbsys-type* 'mysql) 
         (mysql-connect #:server "localhost" #:port 3306 #:database "racquel_test" #:user "test" #:password "test")]
        [(eq? *test-dbsys-type* 'postgresql) 
         (postgresql-connect #:server "localhost" #:port 5432 #:database "racquel_test" #:user "test" #:password "test")]
        [(eq? *test-dbsys-type* 'sqlite3) 
         (sqlite3-connect #:database "schema/racquel_test.sqlite")]        
        [(eq? *test-dbsys-type* 'sqlserver) 
         (odbc-connect #:dsn "racquel_test" #:user "test" #:password "test")]
        [(eq? *test-dbsys-type* 'oracle) 
         (odbc-connect #:dsn "racquel_test_oracle" #:user "test" #:password "test")]
        [(eq? *test-dbsys-type* 'db2) 
         (odbc-connect #:dsn "racquel_test_db2" #:user "test" #:password "test")]
        ))

;;; Schema name for testing.
(define *schema-name* 
  (cond [(eq? *test-dbsys-type* 'mysql) "racquel_test"]
        [(eq? *test-dbsys-type* 'postgresql) "public"]
        [(eq? *test-dbsys-type* 'sqlserver) "dbo"]
        [(eq? *test-dbsys-type* 'oracle) "TEST"]
        ))
  
;;;; TEST DATA OBJECT DEFINITION


(define test-interface<%> (interface ()))

(define-test-suite test-define-data-object
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([test-class% (data-class* object% (test-interface<%>)
                                   (table-name "test")
                                   (column [id #f ("id" "Id")] 
                                           [name #f ("name" "Name")] 
                                           [description #f ("description" "Description")])
                                   (init-column [x ("x" "X")])
                                   (join [object object% #:cardinality 'one-to-one (where (= id ?)) 1])
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
   
    (test-case "test class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 1))
    (test-case "test class metadata ok?" (check-eq? (get-class-metadata table-name test-class%) "test"))
   
    (test-case "data object metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) 
                             (data-class-info test-class%)])
                 (check-eq? tbl-nm "test")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((description "description" "Description") (id "id" "Id") (name "name" "Name") (x "x" "X")))
                 (check-eq? (length j-defs) 1)
                 (check-eq? pkey 'id)
                 (check-eq? auto-key #f)
                 (check-eq? ext-nm "test")
                 (check-not-eq? st-key #f)
                 ))
   
    (test-case "object class ok?" (check-equal? (object-class obj) test-class%))
   
    (test-case "columns set?" 
               (set-column! id obj 1)
               (set-column! name obj "Test")
               (set-column! description obj "This is a test")
               (check-eq? (get-column id obj) 1)
               (check-eq? (get-column name obj) "Test")
               (check-eq? (get-column description obj) "This is a test")
               (check-eq? (get-column object obj) #f))
    
    (test-case "savable field ok?" (check-equal? (savable-fields *con* test-class%) '(description id name x)))
    (test-case "primary key fields ok?" (check-equal? (primary-key-fields test-class%) '(id)))
    (test-case "where clause ok?" (check-equal? (key-where-clause-sql *con* test-class% (primary-key-fields test-class%)) 
                                                (sql-placeholder" where id=?" *test-dbsys-type*)))
    (test-case "insert sql ok?" (check-equal? (insert-sql *con* test-class%) 
                                              (sql-placeholder "insert into test (description, id, name, x) values (?, ?, ?, ?)" *test-dbsys-type*)))
    (test-case "update sql ok?" (check-equal? (update-sql *con* test-class%) 
                                              (sql-placeholder "update test set description=?, id=?, name=?, x=? where id=?" *test-dbsys-type*)))
    (test-case "delete sql ok?" (check-equal? (delete-sql *con* test-class%) 
                                              (sql-placeholder "delete from test where id=?" *test-dbsys-type*)))
    (test-case "select sql ok?"  (check-equal? (select-sql *con* test-class% "where id=?") 
                                               (sql-placeholder "select description, id, name, x from test where id=?" *test-dbsys-type*)))
    ))


;;;; TEST DATABASE SCHEMA


(define (table-name-normalizer n) (string-downcase (string-append (regexp-replace* "([a-z])([A-Z])" n "\\1-\\2") "%")))
(define (column-name-normalizer n) (string-downcase (string-replace n "_" "-")))
(define (join-name-normalizer n) (string-downcase n))
                                     
(define-test-suite test-schema
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([simple-schema (load-schema *con* *schema-name* "simple" #:reverse-join? #f #:db-system-type *test-dbsys-type*)]
         [address-schema (load-schema *con* *schema-name* "address" #:reverse-join? #f #:db-system-type *test-dbsys-type*)])
    (test-case "simple schema loaded?" (check-eq? (length simple-schema) 4))
    (test-case "simple schema columns ok?" 
               (check-equal? (sort (get-schema-columns simple-schema column-name-normalizer) 
                                   string<? #:key (lambda (k) (symbol->string (first k))))
                             '((description #f "description") (id #f "id") (name #f "name") (x #f "x"))))
    (test-case "simple schema joins ok?" 
               (check-equal? (get-schema-joins *con* *schema-name* simple-schema *test-dbsys-type* table-name-normalizer
                                               join-name-normalizer column-name-normalizer) null))
    (test-case "simple primary key fields found?" (check-eq? (find-primary-key-fields simple-schema) 'id))
    (test-case "simple autoincrement key found?" (check-false (has-autoincrement-key? simple-schema)))
    
    (test-case "address schema loaded?" (check-eq? (length address-schema) 6))
    (test-case "address schema columns ok?" 
               (check-equal? (sort (get-schema-columns address-schema column-name-normalizer) 
                                   string<? #:key (lambda (k) (symbol->string (first k))))
                             '((city #f "city") (id #f "id") (line #f "line")
                               (person-id #f "person_id")(state #f "state") (zip-code #f "zip_code"))))
    ;(test-case "schema?" (check-eq? address-schema 1))
    (test-case "address join schema ok?" 
               (check-equal? (get-join-schema address-schema)
                             '(("address_person_id_fkey" "person" "id" "person_id" ("id")))))
    (test-case "address join cardinality ok?" 
               (check-equal? (eval-syntax (join-cardinality *con* *schema-name* *test-dbsys-type* "person" "id"))
                             'one-to-one))
    (test-case "address schema join name ok?" 
               (check-eq? (first (first (get-schema-joins *con* *schema-name* address-schema *test-dbsys-type* table-name-normalizer join-name-normalizer column-name-normalizer))) 
                          'person))
    (test-case "address schema join cardinality ok?" 
               (check-equal? (eval-syntax (fourth (first (get-schema-joins *con* *schema-name* address-schema *test-dbsys-type* 
                                                                           table-name-normalizer join-name-normalizer column-name-normalizer))) racquel-namespace)
                             'one-to-one))
    (test-case "address schema join cardinality ok?" 
               (check-equal? (syntax->datum #`#,(fifth (first (get-schema-joins *con* *schema-name* address-schema *test-dbsys-type* 
                                                                 table-name-normalizer join-name-normalizer column-name-normalizer))))
                             '(where (= ('person% id) ?))))
    (test-case "address primary key fields found?" (check-eq? (find-primary-key-fields address-schema) 'id))
    (test-case "address autoincrement key found?" (check-true (has-autoincrement-key? address-schema)))
    
    (let ([test-schema (list (vector "id" "P" 1 1 sql-null sql-null "PK")
                             (vector "join1_id" "F" 1 sql-null "Joined" "id" "FK_Join1")
                             (vector "join1_str" "F" 2 sql-null "Joined" "str" "FK_Join1")
                             (vector "join2_id" "F" 1 sql-null "Joined" "id" "FK_Join2")
                             (vector "join2_str" "F" 2 sql-null "Joined" "str" "FK_Join2")
                             (vector "simple_id" "F" 1 sql-null "Employer" "id" "FK_Employer")
                             (vector "id" "R" 1 sql-null "Address" "person_id" "FK_Person"))])
      (multi-hash-set! *data-class-schema* (list (vector "id" "P" 1 1 sql-null sql-null "PK")) *con* *schema-name* "Employer")
      (multi-hash-set! *data-class-schema* (list (vector "id" "P" 1 1 sql-null sql-null "PK")) *con* *schema-name* "Joined")
      
      (test-case "address join schema ok?" 
                 (check-equal? (get-join-schema test-schema) '(("FK_Person" "Address" "person_id" "id" ("person_id"))
                                                               ("FK_Employer" "Employer" "id" "simple_id" ("id"))
                                                               ("FK_Join2" "Joined" "id" "join2_id" ("id" "str"))
                                                               ("FK_Join1" "Joined" "id" "join1_id" ("id" "str")))))
    
      (test-case "key-where-clause-rql ok?" )(check-equal?
                                         (syntax->datum #`#,(key-where-clause-rql "Joined" '("id") table-name-normalizer))
                                          '(where (= ('joined% id) ?)))     
      (test-case "key-where-clause-rql ok?" )(check-equal?
                                         (syntax->datum #`#,(key-where-clause-rql "Joined" '("id" "str") table-name-normalizer))
                                          '(where (and (= ('joined% id) ?) (= ('joined% str) ?))))

      (test-case "test schema 1st join cardinality ok?" 
               (check-equal? (syntax->datum #`#,(fifth (first (get-schema-joins *con* *schema-name* test-schema *test-dbsys-type* 
                                                                 table-name-normalizer join-name-normalizer column-name-normalizer))))
                             '(where (= ('address% person_id) ?))))
      (test-case "test schema 2nd join cardinality ok?" 
               (check-equal? (syntax->datum #`#,(fifth (second (get-schema-joins *con* *schema-name* test-schema *test-dbsys-type* 
                                                                 table-name-normalizer join-name-normalizer column-name-normalizer))))
                             '(where (= ('employer% id) ?))))
      (test-case "test schema 3rd join cardinality ok?" 
               (check-equal? (syntax->datum #`#,(fifth (third (get-schema-joins *con* *schema-name* test-schema *test-dbsys-type* 
                                                                 table-name-normalizer join-name-normalizer column-name-normalizer))))
                             '(where (and (= ('joined% id) ?) (= ('joined% str) ?)))))
      (test-case "test schema 4th join cardinality ok?" 
               (check-equal? (syntax->datum #`#,(fifth (fourth (get-schema-joins *con* *schema-name* test-schema *test-dbsys-type* 
                                                                 table-name-normalizer join-name-normalizer column-name-normalizer))))
                             '(where (and (= ('joined% id) ?) (= ('joined% str) ?)))))
    )))


;;;; TEST DATA OBJECT GENERATION


;;; Name externalizer. Returns mixed case.
(define mixed-case-extern-regexp (regexp "-[a-z]"))
(define (name-externalizer s) 
  (string-append (string-upcase (substring s 0 1)) 
                 (substring (regexp-replace* mixed-case-extern-regexp s (lambda (s) (substring (string-upcase s) 1 2))) 1)))

(define-test-suite test-make-data-object
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([test-class% (data-class* object% (test-interface<%>)
                                   (table-name "test")
                                   (column [id #f ("id" "Id")] 
                                           [name #f ("name" "Name")] 
                                           [description #f ("description" "Description")])
                                   (init-column [x ("x" "X")])
                                   (join [object object% #:cardinality 'one-to-one (where (= id ?)) 1])
                                   (primary-key id)
                                   (define/public (test) (x + 1))
                                   (inspect #f)
                                   (super-new))]
         [simple% (gen-data-class *con* "simple" 
                                  #:schema-name *schema-name*
                                  #:generate-joins? #t
                                  #:table-name-normalizer table-name-normalizer
                                  #:column-name-normalizer column-name-normalizer
                                  #:table-name-externalizer name-externalizer)]
         [obj (new simple%)])
    (test-case "simple class created?" (check-not-eq? simple% #f))
    (test-true "simple class is a data class?" (data-class? simple%))
    (test-case "simple object created?" (check-not-eq? obj #f))
    (test-case "simple class ok?" 
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info simple%)]) 
                 (check-eq? cls-nm 'simple%)))
   
    (test-case "simple class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 2))
    (test-case "simple class metadata ok?" (check-eq? (get-class-metadata table-name simple%) "simple"))
   
    (test-case "simple class metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm ) (data-class-info simple%)])
                 (check-eq? tbl-nm "simple")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((description "description" "description") (id "id" "id") (name "name" "name") (x "x" "x")))
                 (check-eq? (length j-defs) 0)
                 (check-eq? pkey 'id)
                 (check-eqv? auto-key #f)
                 (check-eq? ext-nm "Simple")
                 (check-not-eq? st-key #f)
                 ))
   
    (test-case "object class ok?" (check-equal? (object-class obj) simple%))
   
    (test-case "savable field ok?" (check-equal? (sort (savable-fields *con* simple%) string<? #:key symbol->string) 
                                                 '(description id name x)))
    (test-case "where clause ok?" (check-equal? (key-where-clause-sql *con* simple% (primary-key-fields simple%)) 
                                                (sql-placeholder " where id=?" *test-dbsys-type*)))
    (test-case "insert sql ok?" (check-equal? (insert-sql *con* simple%) 
                                              (sql-placeholder "insert into simple (description, id, name, x) values (?, ?, ?, ?)" *test-dbsys-type*)))
    (test-case "update sql ok?" (check-equal? (update-sql *con* simple%) 
                                              (sql-placeholder "update simple set description=?, id=?, name=?, x=? where id=?" *test-dbsys-type*)))
    (test-case "delete sql ok?" (check-equal? (delete-sql *con* simple%) 
                                              (sql-placeholder "delete from simple where id=?" *test-dbsys-type*)))
    (test-case "select sql ok?" 
               (check-equal? (select-sql *con* simple% "where id=?") 
                             (sql-placeholder "select description, id, name, x from simple where id=?" *test-dbsys-type*)))
  
    (test-case "columns set?"
               (set-column! id obj 23)
               (set-column! name obj "test")
               (set-column! description obj "this is a test")
               (set-column! x obj 1.70)
               (check-eq? (get-column id obj) 23)
               (check-eq? (get-column name obj) "test")
               (check-eq? (get-column description obj) "this is a test")
               (check-eq? (get-column x obj) 1.70))
   
    (test-case "object inserted?" 
               (insert-data-object *con* obj)
               (check-not-eq? (get-field id obj) #f))
                 
    (test-case "object changed?" 
               (set-field! name obj "test2")
               (check-eq? (get-field name obj) "test2"))
   
    (test-case "object updated?"
               (update-data-object *con* obj)
               (check-equal? (query-value *con* (sql-placeholder "select id from simple where id=?" *test-dbsys-type*) (get-field id obj)) 23)
               (check-equal? (query-value *con* (sql-placeholder "select name from simple where id=?" *test-dbsys-type*) (get-field id obj)) "test2")
               (check-equal? (query-value *con* (sql-placeholder "select description from simple where id=?" *test-dbsys-type*) (get-field id obj)) "this is a test")
               (check-equal? (query-value *con* (sql-placeholder "select x from simple where id=?" *test-dbsys-type*) (get-field id obj)) 1.7)
               )  
   
    (test-case "object loaded?"
               (let ([s (make-data-object *con* simple% (get-field id obj))])
                 (check-equal? (get-field id s) 23)
                 (check-equal? (get-field name s) "test2")
                 (check-equal? (get-field description s) "this is a test")
                 (check-equal? (get-field x s) 1.7)))
   
    (test-case "object deleted?" 
               (delete-data-object *con* obj)
               (check-true (= (query-value *con* (sql-placeholder "select count(*) from simple where id=?" *test-dbsys-type*) (get-field id obj)) 0)))
    ))


;;;; TEST AUTOINCREMENT KEY


(define-test-suite test-autoincrement-data-object
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let ([auto% (gen-data-class *con* "auto" #:schema-name *schema-name*)])
    (test-case "auto class created?" (check-not-eq? auto% #f))
    (test-true "auto class is a data class?" (data-class? auto%))
    (test-case "auto class ok?" 
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info auto%)]) 
                 (check-eq? cls-nm 'auto%)))
   
    (test-case "auto class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 1))
    (test-case "auto class metadata ok?" (check-eq? (get-class-metadata table-name auto%) "auto"))
   
    (test-case "auto class metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) (data-class-info auto%)])
                 (check-eq? tbl-nm "auto")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((description "description" "description") (id "id""id") (name "name" "name")))
                 (check-equal? j-defs null)
                 (check-eq? pkey 'id)
                 (check-eq? auto-key 'id)
                 (check-eq? ext-nm "auto")
                 (check-not-eq? st-key #f)
                 ))
   
    (let ([obj (new auto%)])
      (test-case "auto object created?" (check-not-eq? obj #f))
      (test-case "object class ok?" (check-equal? (object-class obj) auto%))
      
      (test-case "savable field ok?" (check-equal? (savable-fields *con* auto%) '(description name)))
      (test-case "where clause ok?" (check-equal? (key-where-clause-sql *con* auto% (primary-key-fields auto%)) 
                                                  (sql-placeholder " where id=?" *test-dbsys-type*)))
      (test-case "insert sql ok?" (check-equal? (insert-sql *con* auto%) 
                                                (sql-placeholder "insert into auto (description, name) values (?, ?)" *test-dbsys-type*)))
      (test-case "update sql ok?" (check-equal? (update-sql *con* auto%) 
                                                (sql-placeholder "update auto set description=?, name=? where id=?" *test-dbsys-type*)))
      (test-case "delete sql ok?" (check-equal? (delete-sql *con* auto%) 
                                                (sql-placeholder "delete from auto where id=?" *test-dbsys-type*)))
      (test-case "select sql ok?" 
                 (check-equal? (select-sql *con* auto% "where id=?") 
                               (sql-placeholder "select description, id, name from auto where id=?" *test-dbsys-type*)))
      
      (test-case "columns set?"
                 (set-column! name obj "test")
                 (set-column! description obj "this is a test")
                 (check-eq? (get-column name obj) "test")
                 (check-eq? (get-column description obj) "this is a test")
                 (check-eq? (data-object-state obj) 'new))
      
      (test-case "object inserted?" 
                 (insert-data-object *con* obj)
                 (check-not-eq? (get-field id obj) #f)
                 (check-eq? (data-object-state obj) 'saved))
      
      (test-case "object changed?" 
                 (set-field! name obj "test2")
                 (check-eq? (get-field name obj) "test2"))
      
      (test-case "object updated?"
                 (update-data-object *con* obj)
                 (check-equal? (query-value *con* (sql-placeholder "select name from auto where id=?" *test-dbsys-type*) 
                                            (get-field id obj)) "test2"))  
      
      (test-case "object loaded?"
                 (let ([a (make-data-object *con* auto% (get-field id obj))])
                   (check-equal? (get-field name a) "test2")
                   (check-eq? (data-object-state a) 'loaded)))
      
      (test-case "object selected?"
                 (let ([a (select-data-object *con* auto% (where (= name ?)) (get-field name obj))])
                   (check-equal? (get-field name a) "test2")
                   (check-eq? (data-object-state a) 'loaded)))
      
      (test-case "object deleted?" 
                 (delete-data-object *con* obj)
                 (check-true (= (query-value *con* (sql-placeholder "select count(*) from auto where id=?" *test-dbsys-type*) 
                                         (get-field id obj)) 0))
                 (check-eq? (data-object-state obj) 'deleted)) 
      )))


;;;; TEST JOINS


(define-test-suite test-joins
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([person% (data-class object% 
             (table-name "person" "Person")  
             (column (id 1 "id") (first-name #f "first_name") (last-name #f "last_name") (age #f "age"))
             (primary-key id #:autoincrement #t)
             (join (addresses 'address% (where (= ('address% person-id) ?)) id))
             (super-new)
             (inspect #f))]
        [address% (data-class object% 
             (table-name "address"  "Address")  
             (column (id 1 "id") (person-id 1 "person_id") (line #f "line") (city #f "city") (state #f "state") (zip-code #f "zip_code"))
             (primary-key id #:autoincrement #t)
             (join (person person% #:cardinality 'one-to-one (where (= (person% id) ?)) person-id))
             (super-new)
             (inspect #f))]
        [person-obj (new person%)]
        [address-obj (new address%)])

    (test-case "person class ok?" 
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info person%)]) 
                 (check-eq? cls-nm 'person%)
                 (check-equal? fld-nms '(id first-name last-name age addresses))))
    
    (test-case "class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 2))
    (test-case "person class metadata ok?" (check-eq? (get-class-metadata table-name person%) "person"))
    (test-case "address class metadata ok?" (check-eq? (get-class-metadata table-name address%) "address"))
   
    (test-case "person class metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) (data-class-info person%)])
                 (check-eq? tbl-nm "person")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((age "age" "age") (first-name "first_name" "first_name") (id "id" "id") 
                                 (last-name "last_name" "last_name")))
                 (check-equal? (map first j-defs) '(addresses))
                 (check-equal? (first (map second j-defs)) 'address%)
                 (check-equal? (first (map third j-defs)) 'one-to-many)
                 (check-eq? pkey 'id)
                 (check-eq? auto-key 'id)
                 (check-eq? ext-nm "Person")
                 (check-not-eq? st-key #f)
                 ))
    
    (test-case "addresses not joined?" (check-eq? (get-field addresses person-obj) #f))
    (test-case "person object class ok?" (check-eq? (object-class person-obj) person%))
    (test-case "person object ok?" (check-equal? (get-join-definition addresses (object-class person-obj)) 
                                              '(addresses address% one-to-many)))
    (test-case "person object ok?" (check-equal? (get-class (second (get-join-definition addresses (object-class person-obj)))) 
                                                 address%)) 
    (test-case "addresses joined?" (check-true (is-a? (first (get-join addresses person-obj *con*)) address%)))
    
    (test-case "address class ok?" 
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info address%)]) 
                 (check-eq? cls-nm 'address%)
                 (check-equal? fld-nms '(id person-id line city state zip-code person))))
    
    (test-case "address class metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) (data-class-info address%)])
                 (check-eq? tbl-nm "address")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((city "city" "city") (id "id" "id") (line "line" "line") (person-id "person_id" "person_id")
                                 (state "state" "state") (zip-code "zip_code" "zip_code")))
                 (check-equal? (map first j-defs) '(person))
                 (check-equal? (first (map second j-defs)) person%)
                 (check-equal? (first (map third j-defs)) 'one-to-one)
                 (check-eq? pkey 'id)
                 (check-eq? auto-key 'id)
                 (check-eq? ext-nm "Address")
                 (check-not-eq? st-key #f)
                 ))
   
    (test-case "person not joined?" (check-eq? (get-field person address-obj) #f))
    (test-case "person joined?" (check-true (is-a? (get-join person address-obj *con*) person%)))
    ))


;;;; TEST JOIN GENERATION


(define-test-suite test-generate-join
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([address% (gen-data-class *con* "address" 
                                   #:schema-name *schema-name*
                                   #:table-name-normalizer table-name-normalizer
                                   #:column-name-normalizer column-name-normalizer
                                   #:table-name-externalizer name-externalizer)]
         [obj (new address%)])   
    (test-case "generated class ok?" (check-equal? (gen-data-class *con* "address" #:print? #t
                                                                   #:schema-name *schema-name*
                                                                   #:table-name-normalizer table-name-normalizer
                                                                   #:column-name-normalizer column-name-normalizer
                                                                   #:table-name-externalizer name-externalizer)
                                                   '(let ((address%
                                                           (data-class
                                                            object%
                                                            (table-name "address" "Address")
                                                            (column
                                                             (city #f "city")
                                                             (id #f "id")
                                                             (line #f "line")
                                                             (person-id #f "person_id")
                                                             (state #f "state")
                                                             (zip-code #f "zip_code"))
                                                            (primary-key id #:autoincrement #t)
                                                            (join (person 'person% #:cardinality 'one-to-one (where (= ('person% id) ?)) person-id))
                                                            (super-new)
                                                            (inspect #f))))
                                                      address%)))
    (test-case "address class created?" (check-not-eq? address% #f))
    (test-true "address class is a data class?" (data-class? address%))
    (test-case "address object created?" (check-not-eq? obj #f))
    (test-case "address class ok?" 
               (let-values ([(cls-nm fld-cnt fld-nms fld-acc fld-mut sup-cls skpd?) (class-info address%)]) 
                 (check-eq? cls-nm 'address%)))
    
    (test-case "address class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 1))
    (test-case "address class metadata ok?" (check-eq? (get-class-metadata table-name address%) "address"))
    
    (test-case "address class metadata set?" 
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) (data-class-info address%)])
                 (check-eq? tbl-nm "address")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((city "city" "city") (id "id" "id") (line "line" "line") (person-id "person_id" "person_id")
                                                      (state "state" "state") (zip-code "zip_code" "zip_code")))
                 (check-equal? (caar j-defs) 'person)
                 (check-eq? pkey 'id)
                 (check-eqv? auto-key 'id)
                 (check-eq? ext-nm "Address")
                 (check-not-eq? st-key #f)
                 ))
    
    (test-case "object class ok?" (check-equal? (object-class obj) address%))
    ))


;;;; TEST REVERSE JOIN GENERATION


(define-test-suite test-generate-reverse-join
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([person% (gen-data-class *con* "person" 
                                  #:schema-name *schema-name*
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
               (let-values ([(cls cls-id-key st-key tbl-nm col-defs j-defs pkey auto-key ext-nm) (data-class-info person%)])
                 (check-eq? tbl-nm "person")
                 (check-equal? (sort col-defs string<? #:key (lambda (k) (symbol->string (first k))))
                               '((age "age") (first-name "first_name") (id "id") (last-name "last_name")))
                 (check-equal? (caar j-defs) 'address)
                 (check-eq? pkey 'id)
                 (check-eqv? auto-key #f)
                 (check-eq? ext-nm "person")
                 (check-not-eq? st-key #f)
                 ))
   
    (test-case "object class ok?" (check-equal? (object-class obj) person%))
    ))


;;;; TEST RQL PARSING

 
(define-test-suite test-rql-parsing
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([person% (gen-data-class *con* "person" 
                                  #:schema-name *schema-name*
                                  #:generate-reverse-joins? #t
                                  #:table-name-normalizer table-name-normalizer
                                  #:column-name-normalizer column-name-normalizer)]
         [address% (gen-data-class *con* "address" 
                                   #:schema-name *schema-name*
                                   #:table-name-normalizer table-name-normalizer
                                   #:column-name-normalizer column-name-normalizer)]
         [obj (new address%)])
    (test-case "address class metadata added?" (check-eq? (length (hash->list *data-class-metadata*)) 2))
    (test-case "address class metadata ok?" (check-eq? (get-class-metadata table-name address%) "address"))

    (test-case "select sql ok?" (check-equal? (select-data-object *con* address% #:print? #t 
                                                                  (where (and (= id ?) (= city ?))) 1 "Chicago") 
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where (id = ? and city = ?)" *test-dbsys-type*)))
    (test-case "rql select runs?" 
               (check-true (is-a? (select-data-object *con* address% 
                                                      (where (and (= id ?) (= city ?))) 1 "Chicago") address%)))
    (test-case "selected with rql?"
               (let ([a (select-data-object *con* address% (where (= id 1)))])
                 (check-equal? (get-field city a) "Chicago")
                 (check-eq? (data-object-state a) 'loaded)))
    (test-case "selected with sql?"
               (let ([a (select-data-object *con* address% (string-append "where id = " "1"))])
                 (check-equal? (get-field city a) "Chicago")
                 (check-eq? (data-object-state a) 'loaded)))

    (test-case "select join sql ok?" 
               (check-equal? (select-data-object *con* address% #:print? #t 
                                                 (join person (= (person% id) person_id)) (where (= id 1)))
"select city, id, line, person_id, state, zip_code from address join person on person.id = person_id where id = 1"))
     
    (test-case "select = ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (= city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city = ?" *test-dbsys-type*)))
    (test-case "select <> ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (<> city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city <> ?" *test-dbsys-type*)))
    (test-case "select >= ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (>= city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city >= ?" *test-dbsys-type*)))
    (test-case "select <= ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (<= city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city <= ?" *test-dbsys-type*)))
    (test-case "select > ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (> city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city > ?" *test-dbsys-type*)))
    (test-case "select < ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (< city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city < ?" *test-dbsys-type*)))                
    (test-case "select like ok?" (check-equal? (select-data-object *con* address% #:print? #t (where (like city ?)))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where city like ?" *test-dbsys-type*)))
    (test-case "select in ok?" 
               (check-equal? (select-data-object *con* address% #:print? #t (where (in id ,(make-list 3 "?"))))
(sql-placeholder "select city, id, line, person_id, state, zip_code from address where id in (?,?,?)" *test-dbsys-type*)))
     ))


;;;; TEST MIXINS


(define-test-suite test-mixins
  (map (lambda (k) (hash-remove! *data-class-metadata* k)) (hash-keys *data-class-metadata*))
  (let* ([test-class% (data-class object%
                                 (table-name "test" "Test")
                                 (column [id 1 ("id" "Id")] 
                                         [name "test" ("name" "Name")] 
                                         [description "Test" "Description"])
                                 (primary-key id)
                                 (inspect #f)
                                 (super-new))]
         [json-mixed-class% (json-data-class-mixin test-class%)]
         [json-extern-obj (new json-mixed-class%)]
         [json-intern-obj (new json-mixed-class%)]
         [xml-mixed-class% (xml-data-class-mixin test-class%)]
         [xml-extern-obj (new xml-mixed-class%)]
         [xml-intern-obj (new xml-mixed-class%)])
    
    (set-column! name json-extern-obj "new name")
    (test-case "column name set?" (check-equal? (get-column name json-extern-obj) "new name"))
    
    (test-case "json externalized ok?" (check-equal? (string->jsexpr (send json-extern-obj externalize))
(string->jsexpr"{\"Test\":{\"Description\":\"Test\",\"Id\":1,\"Name\":\"new name\"}}"))) 
    (send json-intern-obj internalize (send json-extern-obj externalize))
    (test-case "json internalized ok?" (check-equal? (get-column name json-intern-obj) "new name"))
    
    (set-column! name xml-extern-obj "a new name")
    (test-case "column name set?" (check-equal? (get-column name xml-extern-obj) "a new name"))
    (test-case "xml externalized ok?" (check-equal? (send xml-extern-obj externalize) 
"<Test><Description>Test</Description><Id>1</Id><Name>a new name</Name></Test>"))
    (send xml-intern-obj internalize (send xml-extern-obj externalize))
    (test-case "xml internalized ok?" (check-equal? (get-column name xml-intern-obj) "a new name"))
  ))


;;;; TEST MULTI-PART KEYS


;;;; TEST MULTIPLE KEYS IN SAME TABLE

 
;;;; TEST INHERITANCE


;;;; RUN ALL TESTS


(run-tests test-define-data-object 'verbose)
(run-tests test-schema 'verbose)
(run-tests test-make-data-object 'verbose)
(run-tests test-autoincrement-data-object 'verbose)
(run-tests test-joins 'verbose)
(run-tests test-generate-join 'verbose)
(run-tests test-rql-parsing 'verbose)
(run-tests test-mixins 'verbose)


;;;; TEAR-DOWN


(disconnect *con*)
