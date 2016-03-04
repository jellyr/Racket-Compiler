#lang racket
'(program
  (type Integer)
  (define (add [x : Integer] [y : Integer]) : Integer
    (+ x y))
  (add 40 2))

`(program
  (type Integer)
  (define (add [x : int] [y : int]) : int (+ x y))
  (let ([vec (vector add)])
    (let ([myfun (vector-ref vec 0)])
      (myfun 40 2))))
