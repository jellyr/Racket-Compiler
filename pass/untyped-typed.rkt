#lang racket
(require "../utilities.rkt")

(provide untyped-typed)

(define (conv-helper e)
  (match e
    [e #:when (boolean? e) `(inject ,e Boolean)]
    [e #:when (fixnum? e) `(inject ,e Integer)]
    [`(+ ,e1 ,e2) `(inject (+ (project ,(conv-helper e1) Integer)
                              (project ,(conv-helper e2) Integer)) Integer)]))

(define (untyped-typed e)
  (match-define `(program . ,expr) e)
  `(program . ,(map conv-helper expr)))
