#lang racket
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        [(? symbol?) (lookup e alist)]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body)
         (let* ([newx (gensym x)]
               [newlist (cons `(,x . ,newx) alist)])
           `(let ([,newx ,((uniquify alist) e)])
              ,((uniquify newlist) body)))]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))

(define (flatten e)
  (match e
    ;[(? fixnum?) e]
    ;[`(read) `(read)]
    [`(- ,e1) `(- ,(flatten e1))]
    [`(+ ,e1 ,e2) __]
    [`(program ,e) `(program ,(flatten e))]
    [`(let ([,x ,e]) ,body) __]
    ))
