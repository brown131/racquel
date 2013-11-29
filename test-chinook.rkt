#lang racket

(require racket/base db racquel)

(define *con* (mysql-connect #:server "localhost" #:port 3306 #:database "Chinook" #:user "test" #:password "test"))

(gen-data-class *con* "Album" #:print? #t
                #:schema-name "Chinook"
                #:generate-joins? #t #:generate-reverse-joins? #t)

(define album% (gen-data-class *con* "Album"
                               #:schema-name "Chinook"
                               #:generate-joins? #t #:generate-reverse-joins? #t))

(define albums (select-data-objects *con* album%))

albums