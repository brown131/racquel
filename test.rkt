#lang racket

(require test-engine/racket-tests)
(require racquel)

;;;; TESTS

(class data-object% 
  (field (id #f) 
         (name #f) 
         (description #f)) 
  (super-new 
   (table-name "phrasetype")
   (column-names '("id" "name" "description")) 
   (column-types '("int" "varchar" "varchar")) 
   (primary-key "id")) (inspect #f))
#|
(select-data-objects con phrase-type% "where id>?" 1)

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
