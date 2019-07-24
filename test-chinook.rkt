#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; test-chinook - Test module that using the Chinook database.

(require racket/base rackunit rackunit/text-ui db racquel)


(define *con* (sqlite3-connect #:database "Chinook_Sqlite_AutoIncrementPKs.sqlite"))

;;; Define classes for all the tables.
(define-values (album% sqlite_sequence% artist% customer% employee% genre% invoice% invoice-line%
                media-type% playlist% playlist-track% track%)
  (apply values (map (λ (t) (gen-data-class *con* t #:schema-name #f
                                            #:generate-joins? #t #:generate-reverse-joins? #t)) 
                     (list-tables *con*))))


;;; TEST SELECT OBJECTS


(define-test-suite test-select-objects
  (let ([*albums* (select-data-objects *con* album%)]
        [*tracks* (select-data-objects *con* track%)])
    (test-equal? "album count correct?" (length *albums*) 347)
    (test-equal? "track count correct?" (length *tracks*) 3503)

    (test-equal? "Albums starting with 'Q' found?" 
                 (map (λ (a) (get-column title a)) 
                      (select-data-objects *con* album% (where (like (album% title) ?)) "Q%"))
                 '("Quanta Gente Veio Ver (Live)"
                   "Quanta Gente Veio ver--Bônus De Carnaval"
                   "Quiet Songs"))

    (test-equal? "Tracks joined to the 1st album found?" 
                 (map (λ (t) (get-column name t)) (get-join tracks (first *albums*) *con*))
                 '("For Those About To Rock (We Salute You)"
                   "Put The Finger On You"
                   "Let's Get It Up"
                   "Inject The Venom"
                   "Snowballed"
                   "Evil Walks"
                   "C.O.D."
                   "Breaking The Rules"
                   "Night Of The Long Knives"
                   "Spellbound"))
    
    (test-equal? "Tracks SQL OK?" 
                 (select-data-objects *con* track% #:print? #t
                                      (join album% (and (= (album% album-id) (track% album-id)) 
                                                        (like (album% title) ?))) 
                                      (where (like (track% name) ?)) "A%" "B%")
                "select \"Track\".\"AlbumId\", \"Track\".\"Bytes\", \"Track\".\"Composer\", \
\"Track\".\"GenreId\", \"Track\".\"MediaTypeId\", \"Track\".\"Milliseconds\", \"Track\".\"Name\", \
\"Track\".\"TrackId\", \"Track\".\"UnitPrice\" from \"Track\" \
join \"Album\" on (\"Album\".\"AlbumId\" = \"Track\".\"AlbumId\" and \"Album\".\"Title\" like ?) \
where \"Track\".\"Name\" like ?")

    (test-equal? "Tracks starting 'D' from albums starting with 'B' found?" 
                 (map (λ (a) (cons (get-column title (get-join album a *con*)) (get-column name a))) 
                      (select-data-objects *con* track% 
                                           (join album% (and (= (album% album-id) (track% album-id)) 
                                                             (like (album% title) ?))) 
                                           (where (like (track% name) ?)) "D%" "B%"))
                 '(("Da Lama Ao Caos" . "Banditismo Por Uma Questa")
                   ("Da Lama Ao Caos" . "Banditismo Por Uma Questa")
                   ("Deep Purple In Rock" . "Bloodsucker")
                   ("Djavan Ao Vivo - Vol. 02" . "Boa Noite")
                   ("Demorou..." . "Beijo do Olhar")
                   ("Dark Side Of The Moon" . "Brain Damage")
                   ("Diver Down" . "Big Bad Bill (Is Sweet William Now)")))
    
    (test-equal? "album ids < 4 found?"
                 (map (λ (a) (get-column album-id a))
                      (select-data-objects *con* album% (where (< albumid 4))))
                 '(1 2 3))
    
    (test-equal? "album ids between 4 and 6 found?"
                 (map (λ (a) (get-column album-id a)) 
                      (select-data-objects *con* album% (where (between albumid 4 6))))
                 '(4 5 6))
        
    (test-equal? "album ids in 1, 3, 5 found?"
                 (map (λ (a) (get-column album-id a)) 
                      (select-data-objects *con* album% 
                                           (where (in title (make-list 3 '?))) 
                                           "For Those About To Rock We Salute You" 
                                           "Restless and Wild" "Big Ones"))
                 '(1 3 5))

    (test-equal? "all albums selected?" (length (select-data-objects *con* album%)) 347)
    
    (test-equal? "albums sorted by title?"
                 (map (λ (a) (get-column title a)) 
                      (take (select-data-objects *con* album% "order by title desc") 3))
                 '("[1997] Black Light Syndrome" "Zooropa" "Worlds"))
    ))


;;;; RUN ALL TESTS


(run-tests test-select-objects 'verbose)
