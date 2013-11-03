#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; keywords - Defines syntactic keywords
;;;;
;;;; Copyright (c) Scott Brown 2013

(provide (all-defined-out))

(define-syntax table-name (syntax-rules ()))
(define-syntax init-column (syntax-rules ()))
(define-syntax column (syntax-rules ()))
(define-syntax primary-key (syntax-rules ()))
(define-syntax join (syntax-rules ()))
(define-syntax where (syntax-rules ()))
(define-syntax <> (syntax-rules ()))
(define-syntax like (syntax-rules ()))
(define-syntax in (syntax-rules ()))
