#lang racket
(require "../utilities.rkt")

(provide uniquify)

(define (uniquify-func def-expr alist)
  (let ([func-vars (foldl (lambda (v res)
                            (match v
                              [`(define (,fname . ,params) : ,ret ,b) (let ([newvar (gensym fname)])
                                                                      (cons `(,fname . ,newvar) res))]))
                          alist
                          def-expr)])
    `(,(map (uniquify func-vars) def-expr) ,func-vars)))

(define (fun-param-helper expr alist)
  (foldl (lambda (p res)
           (match p
             [`(,x : ,type) (let ([var-check (lookup x (last res) #f)])
                              (if var-check
                                  `(,(cons `(,var-check : ,type) (car res)) ,(last res))
                                  (let ([newx (gensym x)])
                                    `(,(cons `(,newx : ,type) (car res)) ,(cons `(,x . ,newx) (last res)))
                                    )))])) `(() ,alist) expr))

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
        [`(type ,ty) e]
        [`(define (,fname . ,params) : ,ret-type ,body)
         (let ([newvar ((uniquify alist) fname)])
           (match-define `(,pstmt ,plist) (fun-param-helper params alist))
           `(define (,newvar ,@pstmt) : ,ret-type ,((uniquify plist) body)))]
        [`(program ,ret-type . ,e) (begin
                           (match-define
                             `(,define-stmt ,flist) (uniquify-func (drop-right e 1) alist))
                           `(program ,ret-type ,@define-stmt ,((uniquify flist) (last e))))]
        [`(,op ,es ...) #:when (lookup op alist #f)
         `(,((uniquify alist) op) ,@(map (uniquify alist) es))]
        [`(,op ,es ...)
          `(,op ,@(map (uniquify alist) es))]))))
