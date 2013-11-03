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

;;; Define RQL operators.
(define-syntax rql-and [syntax-rules () ((_ a ...) (string-append "(" (string-join (list (~a a) ...) " and " ) ")"))])
(define-syntax rql-or [syntax-rules () ((_ a ...) (string-append "(" (string-join (list (~a a) ...) " or " ) ")"))])
(define-syntax rql-not [syntax-rules () ((_ a ...) (string-append "(not " (~a a) ... ")"))])
(define-syntax rql-= [syntax-rules () ((_ a b) (string-append (~a a) " = " (~a b)))])
(define-syntax rql-<> [syntax-rules () ((_ a b) (string-append (~a a) " <> " (~a b)))])
(define-syntax rql->= [syntax-rules () ((_ a b) (string-append (~a a) " >= " (~a b)))])
(define-syntax rql-<= [syntax-rules () ((_ a b) (string-append (~a a) " <= " (~a b)))])
(define-syntax rql-> [syntax-rules () ((_ a b) (string-append (~a a) " > " (~a b)))])
(define-syntax rql-< [syntax-rules () ((_ a b) (string-append (~a a) " < " (~a b)))])
(define-syntax rql-like [syntax-rules () ((_ a b) (string-append (~a a) " like " (~a b)))])
(define-syntax rql-in [syntax-rules () ((_ a b) (string-append (~a a) " in (" (string-join (map ~a b) ",") ")"))])
(define-syntax rql-unquote [syntax-rules () ((_ x) (eval-syntax #`x))])
(define-syntax rql-column [syntax-rules () ((_ a b) (string-append (~a a) "." (~a b)))])
