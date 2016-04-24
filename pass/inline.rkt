#lang racket
(require "../common.rkt")

(provide inline-func)


(define (substution instr env def-env)
  (display "substution instr:") (displayln instr)
  (display "env: ")(displayln env)
  (define recur (lambda (x) (substution x env def-env)))
  (match instr
    [`(has-type ,e^ ,type) `(has-type ,(recur e^) ,type)]
    [(? symbol?) (lookup instr env instr)]
    [(? fixnum?) instr]
    [`(lambda: ,paras : ,ret-t ,body) `(lambda: ,paras : ,ret-t ,(map recur body))]
    [`((has-type ,funame ,ftype) . ,args)
     (define fdata (lookup funame def-env #f))
     (if fdata
         ;; then
         (let* ([vars (map recur args)]
                [fargs (car fdata)]
                [fbody (cdr fdata)]
                [new-env (map (lambda (x y) `(,x . ,y)) fargs vars)])
           (substution fbody (append new-env env) def-env))
         ;; else
         `((has-type ,funame ,ftype) ,@(map recur args)))]
    [else instr]
    ))

(define (inline-env definline)
  ; (display "definline: ") (displayln definline)
  (match definline
    [`(define-inline (,funame . ,var-defs) : ,ret-type ,body)
     `(,funame . (,(map car var-defs) . ,body))]))

(define (inline-func prog)
  (match-define `(program ,type . ,exprs) prog)
  (define defs (drop-right exprs 1))
  (define body (last exprs))
  (define defs-inline (filter (lambda (x)
                                ;(display "x: ")(displayln x)
                                (equal? 'define-inline (car x))) defs))
  (define def-env (map inline-env defs-inline))
  ;(display "defs: ")(displayln defs)
  ;(display "defs-inline: ")(displayln defs-inline)
  ;(display "def-env: ")(displayln def-env)
  
  `(program ,type . ,(map (lambda (x) (substution x '() def-env)) exprs)))
