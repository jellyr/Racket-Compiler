#lang racket
(require "../utilities.rkt")

(provide uniquify)

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        [(? symbol?) (lookup e alist)]
        [(? boolean?) e]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body)
         (let* ([newx (gensym x)]
               [newlist (cons `(,x . ,newx) alist)])
           `(let ([,newx ,((uniquify alist) e)])
              ,((uniquify newlist) body)))]
        [`(type ,type) e]
        [`(,x : ,type) (let ([var-check (lookup x alist)])
                         (if var-check
                             `(,var-check : ,type)
                             (let ([newx newvar]))))]
        [`(define (,fname ,params) : ,ret-type ,body)
         (let ([newvar (gensym fname)]
               [newlist (cons `(,fname . ,newvar) alist)])
           `(define (,newvar ,@(map (uniquify newlist) params) : ,ret-type ,((uniquify newlist) body))))]
        [`(program ,e) `(program ,((uniquify alist) e))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))
