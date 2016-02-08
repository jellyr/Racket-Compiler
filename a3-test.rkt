#lang racket
(require "inspect.rkt")
(require "compiler.rkt")




(define expr '(if #f 0 42))
(define expr1 '(if #f (if #f #t #t) #t))


(test r2-passes expr)
(test r2-passes expr1)