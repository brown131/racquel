#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; keywords - Defines syntactic keywords

(require "util.rkt" "metadata.rkt" "schema.rkt")

(provide (all-defined-out))
 
(define-syntax table-name (syntax-rules ()))
(define-syntax init-column (syntax-rules ()))
(define-syntax column (syntax-rules ()))
(define-syntax primary-key (syntax-rules ()))
(define-syntax join (syntax-rules ()))
(define-syntax left-join (syntax-rules ()))
(define-syntax right-join (syntax-rules ()))
(define-syntax where (syntax-rules ()))
(define-syntax <> (syntax-rules ()))
(define-syntax like (syntax-rules ()))
(define-syntax in (syntax-rules ()))
(define-syntax between (syntax-rules ()))

;;; Define RQL operators.
(define-syntax rql-and [syntax-rules () 
                         ((_ a ...) (string-append "(" (string-join (list a ...) " and " ) ")"))])
(define-syntax rql-or [syntax-rules () 
                        ((_ a ...) (string-append "(" (string-join (list a ...) " or " ) ")"))])
(define-syntax rql-not [syntax-rules () ((_ a) (string-append "(not " a ")"))])
(define-syntax rql-= [syntax-rules () ((_ a b) (string-append a " = " b))])
(define-syntax rql-<> [syntax-rules () ((_ a b) (string-append a " <> " b))])
(define-syntax rql->= [syntax-rules () ((_ a b) (string-append a " >= " b))])
(define-syntax rql-<= [syntax-rules () ((_ a b) (string-append a " <= " b))])
(define-syntax rql-> [syntax-rules () ((_ a b) (string-append a " > " b))])
(define-syntax rql-< [syntax-rules () ((_ a b) (string-append a " < " b))])
(define-syntax rql-like [syntax-rules () ((_ a b) (string-append a " like " b))])
(define-syntax rql-in 
  [syntax-rules ()  ((_ a b) (string-append (~a a) " in (" (string-join (map ~a b) ",") ")"))])
(define-syntax rql-between [syntax-rules () ((_ a b c)  (string-append a " between " b " and " c))])
(define-syntax rql-unquote [syntax-rules () ((_ x) (eval-syntax #`x))])
(define-syntax rql-table-name 
  [syntax-rules () ((_ a b) (begin (set! b (cons a b))
                                    (get-class-metadata table-name (get-class a))))])
(define-syntax rql-column-pair 
  [syntax-rules () ((_ a b c) (string-append 
                               (sql-escape (begin (set! c (cons a c))
                                                  (get-class-metadata table-name (get-class a))) 
                                           dbsys-type) 
                               "." (sql-escape (get-column-name b (get-class a)) dbsys-type)))])
