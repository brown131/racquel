#lang racket

(require rackunit rackunit/text-ui db racquel)

(require/expose racquel (savable-fields primary-key-where-clause insert-sql update-sql delete-sql select-sql))

;;;; SETUP

;;; Test connection
(define con (mysql-connect #:server "localhost" #:port 3306 #:database "babelbuilder" #:user "root" #:password "wurzel"))

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

(define-test-suite racquel
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
   (check-equal? (delete-sql con obj) "delete test where id=?")
   (check-equal? (select-sql con obj "where id=?") "select id, name, description from test t where id=?")
   
   
   ;(select-data-object con test-object%% "where name=?" "Verb Phrase")
   (print con)
  ; (check-eq? 1 0)
   )
)

(run-tests racquel 'verbose)

(disconnect con)

#|
(select-data-object con phrase-type% "where name=?" "Verb Phrase")

(define con (mysql-connect #:server "localhost" #:port 3306 #:database "babelbuilder" #:user "root" #:password "wurzel"))

(define phrase-type% (data-class con "phrasetype"))
(define pt (new phrase-type%))

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
|#
