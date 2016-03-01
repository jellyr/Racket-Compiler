#lang racket
(require "utilities.rkt")
(require "uncover-types.rkt")
(provide (all-from-out "utilities.rkt"))
(provide (all-from-out "uncover-types.rkt"))


(provide int? var? reg? stack? scalar? HEAP-LEN)
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


(define HEAP-LEN 10000) ;; For Debugging GC

