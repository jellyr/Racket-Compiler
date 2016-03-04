#lang racket
(require "utilities.rkt")
(require "uncover-types.rkt")
(provide (all-from-out "utilities.rkt"))
(provide (all-from-out "uncover-types.rkt"))


(provide int? var? reg? stack? scalar? define? trivial-func? HEAP-LEN)
(define (int? e)
  (eqv? (car e) 'int))


(define (var? e)
  (eqv? (car e) 'var))

(define (reg? e)
  (eqv? (car e) 'reg))

(define (stack? e)
  (eqv? (car e) 'stack))

(define (scalar? e)
  (or (fixnum? e) (symbol? e) (boolean? e)))

(define (define? e)
  (eqv? (car e) 'define))

(define HEAP-LEN 10000) ;; For Debugging GC

(define (trivial-func? e)
  (set-member? (set '+ '- 'and 'not 'or 'eq?) e))


