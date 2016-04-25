#lang racket
(require "../common.rkt")

(provide inline-func)


(define (substution instr env def-env)
  ;(display "substution instr:") (displayln instr)
  ;(display "env: ")(displayln env)
  ;(display "def-env: ") (displayln def-env)
  (define recur (lambda (x) (substution x env def-env)))
  (match instr
    [`(has-type ,e^ ,type) (let ([recur-e (recur e^)])
                             (if (and (pair? e^) (equal? (car recur-e) 'has-type))
                                 recur-e
                                 `(has-type ,recur-e ,type)))]
    [(? symbol?) (lookup instr env instr)]
    [(? fixnum?) instr]
    [`(lambda: ,paras : ,ret-t ,body) `(lambda: ,paras : ,ret-t ,(map recur body))]
    [`((has-type ,funame ,ftype) . ,args) #:when (lookup funame env #f)
     (define new-fun (lookup funame env))
     `((has-type ,new-fun ,ftype) ,@(map recur args))]
    
    [`((has-type ,funame ,ftype) . ,args)
     (define fdata (lookup funame def-env #f))
     (if fdata
         ;; then
         (let* ([vars (map recur args)]
                [fargs (car fdata)]
                [fbody (cdr fdata)]
                [new-env (map (lambda (x y) `(,x . ,y)) fargs vars)])
           (match-define `(has-type ,expr ,t) (substution fbody (append new-env env) def-env))
           expr)
         ;; else
         `((has-type ,funame ,ftype) ,@(map recur args)))]
    [`(let ([,var-def (has-type ,para ,var-t)]) ,body)
     (let* ([var^ (recur para)]
            [temp* (lookup var^ def-env #f)]
            [new-def-env (if temp*
                             (cons `(,var-def . ,temp*) def-env)
                             def-env)]
            [new-env (if temp*
                         env
                         (cons `(,var-def . ,temp*) env))])
       `(let ([,var-def (has-type ,var^ ,var-t)]) ,(substution body new-env new-def-env)))]
    [`(,def (,funame . ,var-defs) : ,ret-type ,body) #:when (or (equal? def 'define)
                                                                (equal? def 'define-inline))
     `(define (,funame . ,var-defs) : ,ret-type ,(map recur body))]
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
  (if (null? def-env)
      prog
      `(program ,type . ,(map (lambda (x) (substution x '() def-env)) exprs))))









