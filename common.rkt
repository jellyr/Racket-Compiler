#lang racket
(require "utilities.rkt")
(require "uncover-types.rkt")
(provide (all-from-out "utilities.rkt"))
(provide (all-from-out "uncover-types.rkt"))


(provide int? var? reg? stack? scalar? define? trivial-func? reg-colors HEAP-LEN tagof)
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
  (set-member? (set '+ '- '* 'and 'not 'or 'eq? '< '<= '>= '> 'read 'lambda 'void) e))

(define reg-colors
  '((rax . -1) (__flag . -1)
    (rbx . 0) (rcx . 1) (rdx . 2) (rsi . 3) (rdi . 4)
    (r8 . 5) (r9 . 6) (r10 . 7) (r11 . 8) (r12 . 9) (r13 . 10)
    (r14 . 11) (r15 . 12)))


(define (tagof T)
  (match T
    ['Integer 0]
    ['Boolean 1]
    [`(Vector . ,e1) 2]
    [`(Vectorof . ,e1) 2]
    ['Void 4]
    [else 3]))
