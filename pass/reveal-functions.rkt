#lang racket
(require "../common.rkt")
(provide reveal-functions)


(define (get-func define-list)
  (list->set (map (lambda (e)
                    (match e
                      [`(define (,funame . ,var-defs) ,body) funame]
                      [else (error "in reveal-func, get-func")]))
                  define-list)))

;; env -> funame set : (set 'myfun 'myfun1)
(define (helper instr env)
  (match instr
    [(? (curry set-member? env) ?) `(function-ref ,instr)]
    ;;[`(has-type ,instr ,t) `(has-type ,(helper instr env) ,t)]
    [`(let ([,x ,e]) ,body) `(let ([,x ,(helper e env)]) ,(helper body env))]
    [`(if ,econd ,ethen ,eelse) `(if ,(helper econd env) ,(helper ethen env) ,(helper eelse env))]
    [`(lambda ,vars ,body) `(lambda ,vars ,(helper body env))]
    [`(define (,funame . ,var-defs) ,body) `(define (,funame . ,var-defs) ,(helper body env))]
    [`(vector . ,e1) `(vector . ,(map (curryr helper env) e1))]
    [`(vector-ref ,expr ,number) `(vector-ref ,(helper expr env) ,number)]
    [`(vector-set! ,expr1 ,number ,expr2) `(vector-set! ,(helper expr1 env) ,number ,(helper expr2 env))]
    [`(,op . ,e1) #:when(not (trivial-func? op)) `(app ,(helper op env) . ,(map (curryr helper env) e1))] ;; consider non-parameters situation
    [`(,op . ,e1) `(,op . ,(map (curryr helper env) e1))]
    [else instr]))

(define (reveal-functions e)
  (match e
    [`(program . ,expr)
     (define defs (drop-right expr 1))
     (define env (get-func defs))
     `(program . ,(map (curryr helper env) expr))]))
