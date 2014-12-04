#lang racket
;;;; Racquel - An ORM for Racket
;;;;
;;;; keywords - Defines syntactic keywords
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

(require "metadata.rkt")

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
  [syntax-rules () ((_ a b) (string-append "`" (begin (set! b (cons a b))
                                                      (get-class-metadata table-name (get-class a))) 
                                           "`"))])
(define-syntax rql-column-name 
  [syntax-rules () ((_ a) (string-append "`" (get-column-name-from-context a ctxt) "`"))])
(define-syntax rql-column-pair 
  [syntax-rules () ((_ a b c) (string-append "`" (begin (set! c (cons a c))
                                                        (get-class-metadata table-name (get-class a)))
                                             "`.`" (get-column-name b (get-class a)) "`"))])
