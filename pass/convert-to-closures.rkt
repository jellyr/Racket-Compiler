#lang racket
(require "../utilities.rkt")

(provide convert-to-closures)



(define (convert-to-closures expr)
  (match-define `(program ,ret . ,e) expr)
  (define defs (drop-right e 1))
  )
