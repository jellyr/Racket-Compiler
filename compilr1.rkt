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

(define (flattenhelper e)
  (match e
    [(or (? fixnum?) (? symbol?)) (values e '() '())]
    [`(- ,e1) (let [(newvar (gensym))]
                (let-values ([(e^ statements^ alist) (flattenhelper e1)])
                  (values newvar (append statements^ `((assign ,newvar (- ,e^)))) (cons newvar alist))))]
    [`(+ ,e1 ,e2) (let [(newvar (gensym))]
                    (let-values (((e1^ stmt1^ alist1^) (flattenhelper e1))
                                 ((e2^ stmt2^ alist2^) (flattenhelper e2)))
                      (values newvar
                              (append stmt1^ (append stmt2^ `((assign ,newvar (+ ,e1^ ,e2^)))))
                              (append (cons newvar alist1^) alist2^))))]
    ;[`(program ,e) `(program ,(flatten e))]
    [`(let ([,x ,e]) ,body) ]
    ))
