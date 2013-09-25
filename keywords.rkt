#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; keywords - Defines syntactic keywords
;;;;
;;;; Copyright (c) Scott Brown 2013

(provide table-name init-column column external-name primary-key join)

(define-syntax table-name (syntax-rules ()))
(define-syntax init-column (syntax-rules ()))
(define-syntax column (syntax-rules ()))
(define-syntax external-name (syntax-rules ()))
(define-syntax primary-key (syntax-rules ()))
(define-syntax join (syntax-rules ()))
