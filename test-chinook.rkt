#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test-chinook - Test module that using the Chinook database.
;;;;
;;;; Copyright (c) Scott Brown 2013
;;;;
;;;; This file is part of Racquel
;;;;
;;;; Racquel is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/base db racquel)

(define *con* (mysql-connect #:server "localhost" #:port 3306 #:database "Chinook" #:user "test" #:password "test"))

;;; Define classes for all the tables.
(define-values (album% artist% customer% employee% genre% invoice% invoice-line% media-type% playlist% playlist-track% track%)
  (apply values (map (lambda (t) (gen-data-class *con* t #:schema-name "Chinook"
                                                 #:generate-joins? #t #:generate-reverse-joins? #t)) (list-tables *con*))))

(define *albums* (select-data-objects *con* album%))
(length *albums*)

(define *tracks* (select-data-objects *con* track%))
(length *tracks*)

(map (lambda (a) (get-column title a)) (select-data-objects *con* album% (where (like (album% title) ?)) "Q%"))

(map (lambda (t) (get-column name t)) (get-join tracks (first *albums*) *con*))

(select-data-objects *con* track% #:print? #t
                          (join album% (and (= (album% album-id) (track% album-id)) (like (album% title) ?))) 
                          (where (like (track% name) ?)) "A%" "B%")

(map (lambda (a) (cons (get-column title (first (get-join albums a *con*))) (get-column name a))) 
     (select-data-objects *con* track% 
                          (join album% (and (= (album% album-id) (track% album-id)) (like (album% title) ?))) 
                          (where (like (track% name) ?)) "A%" "B%"))
