#lang racket
(require "../utilities.rkt")

(provide uniquify)

(define (uniquify-func def-expr alist)
  (let ([func-vars (foldl (lambda (v res)
                            (match v
                              [`(define (,fname . ,params) ,b) (let ([newvar (gensym 'fun)])
                                                                      (cons `(,fname . ,newvar) res))]))
                          alist
                          def-expr)])
    `(,(map (uniquify func-vars) def-expr) ,func-vars)))

(define (fun-param-helper expr alist)
  (foldr (lambda (x res)
           (let ([newx (gensym x)])
             `(,(cons newx (car res)) ,(cons `(,x . ,newx) (last res))))) `(() ,alist) expr))

(define uniquify
  (lambda (alist)
    (lambda (e)
      (match e
        ;[`(has-type ,expr ,ht) `(has-type ,((uniquify alist) expr) ,ht)]
        [(? symbol?) (lookup e alist)]
        [(? boolean?) e]
        [(? integer?) e]
        [`(let ([,x ,e]) ,body)
         (let* ([newx (gensym x)]
                [newlist (cons `(,x . ,newx) alist)])
           `(let ([,newx ,((uniquify alist) e)])
              ,((uniquify newlist) body)))]
        [`(type ,ty) e]
        [`(define (,fname . ,params) ,body)
         (let ([newvar ((uniquify alist) fname)])
           (match-define `(,pstmt ,plist) (fun-param-helper params alist))
           `(define (,newvar ,@pstmt) ,((uniquify plist) body)))]
        [`(lambda ,params ,body)
         (match-define `(,pstmt ,plist) (fun-param-helper params alist))
         `(lambda ,pstmt ,((uniquify (append plist alist)) body))]
        [`(program . ,e) (match-define `(,define-stmt ,flist) (uniquify-func (drop-right e 1) alist))
                         `(program ,@define-stmt ,((uniquify flist) (last e)))]
        [`(,op ,es ...) #:when (lookup op alist #f)
         `(,((uniquify alist) op) ,@(map (uniquify alist) es))]
        [`(,op ,es ...)#:when (pair? op)
         `(,((uniquify alist) op) ,@(map (uniquify alist) es))]
        [`(,op ,es ...)
         `(,op ,@(map (uniquify alist) es))]))))
