#lang racket
(require "../common.rkt")

(provide inline-func)


(define (substution instr env)
  (define recur (curryr substution env))
  (match instr
    [`(has-type ,e^ ,type) (has-type ,(recur e^) ,type)]
    [(? symbol?) (lookup instr env instr)]
    [`(lambda: ,paras : ,ret-t ,body) `(lambda: ,paras : ,ret-t ,(map recur body))]
    [`(,func . ,arg) `(,func ,@(map recur arg))]))

(define (inline-helper instr)
 (match instr
   [1 1]) )

(define (inline-func prog)
  (match-define `(program ,type . ,expr) prog)
  (define defs (drop-right expr 1))
  (define body (last expr))
  (define defs-inline (filter (lambda (x) (eq? 'define-inline x)) defs))
  0
  )
