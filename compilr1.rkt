#lang racket
(require racket/fixnum)
(require "interp.rkt")
(require "utilities.rkt")

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        [(? symbol?) __]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body) ___]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))
