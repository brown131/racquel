#lang racket

(require racket/base db racquel)

(define *con* (mysql-connect #:server "localhost" #:port 3306 #:database "Chinook" #:user "test" #:password "test"))

(gen-data-class *con* "Album" #:print? #t
                #:schema-name "Chinook"
                #:generate-joins? #t #:generate-reverse-joins? #t)
(gen-data-class *con* "Track" #:print? #t
                #:schema-name "Chinook"
                #:generate-joins? #t #:generate-reverse-joins? #t)

(define album% (gen-data-class *con* "Album"
                               #:schema-name "Chinook"
                               #:generate-joins? #t #:generate-reverse-joins? #t))
(define track% (gen-data-class *con* "Track"
                               #:schema-name "Chinook"
                               #:generate-joins? #t #:generate-reverse-joins? #t))
(define *albums* (select-data-objects *con* album%))
(length *albums*)

(define *tracks* (select-data-objects *con* track%))
(length *tracks*)

(map (lambda (a) (get-column title a)) (select-data-objects *con* album% (where (like (album% title) ?)) "Q%"))

(map (lambda (t) (get-column name t)) (get-join tracks (first *albums*) *con*))

(select-data-objects *con* track% #:print? #t
                          (join album% (= (album% album-id) (track% album-id))) 
                          (where (like (track% name) ?)) "A%")

(map (lambda (a) (get-column name a)) 
     (select-data-objects *con* track% 
                          (join album% (= (album% album-id) (track% album-id))) 
                          (where (like (track% name) ?)) "A%"))
