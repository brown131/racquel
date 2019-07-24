#lang setup/infotab
;;;; Racquel - An ORM for Racket
;;;;
;;;; info - Package information.

(define name "Racquel: An Object/Relational Mapper for Racket")
(define blurb '("Racquel is an object/relational mapper for Racket."))
(define version "1.0")
(define release-notes '((p "1.0")))
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.3.16")
(define scribblings '(("doc/racquel.scrbl" (multi-page))))
(define primary-file "main.rkt")
(define deps (list "base" "db-lib" "rackunit-lib"))
(define build-deps '("racket-doc"))
