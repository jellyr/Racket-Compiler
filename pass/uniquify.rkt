#lang racket
(require "../utilities.rkt")

(provide uniquify)

(define (uniquify-func def-expr alist)
  (let ([func-vars (foldl (lambda (v res)
                            (match v
                              [`(define (,fname . ,params) : ,ret ,b) (let ([newvar (gensym 'fun)])
                                                                      (cons `(,fname . ,newvar) res))]))
                          alist
                          def-expr)])
    `(,(map (uniquify func-vars) def-expr) ,func-vars)))

(define (fun-param-helper expr alist)
  (foldr (lambda (p res)
           (match p
             [`(,x : ,type)
              (let ([newx (gensym x)])
                `(,(cons `(,newx : ,type) (car res)) ,(cons `(,x . ,newx) (last res))))]))                 `(() ,alist)
                expr))

;; Not needed as the func params should have new parameters should always be new variables.
;; (let ([var-check (lookup x (last res) #f)])
;;                               (if var-check
;;                                   `(,(cons `(,var-check : ,type) (car res)) ,(last res))
;;                                   ))


(define uniquify
  (lambda (alist)
    (lambda (e)
      ;(pretty-display e)
      (match e
        [`(has-type ,expr ,ht) `(has-type ,((uniquify alist) expr) ,ht)]
        [(? symbol?)
;;         (displayln alist)
         (lookup e alist)]
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
        [`(define-inline (,fname . ,params) : ,ret-type ,body)
         (let ([newvar ((uniquify alist) fname)])
           (match-define `(,pstmt ,plist) (fun-param-helper params alist))
           `(define-inline (,newvar ,@pstmt) : ,ret-type ,((uniquify plist) body)))]
        [`(lambda: ,params : ,ret-type ,body)
         (match-define `(,pstmt ,plist) (fun-param-helper params alist))
         `(lambda: ,pstmt : ,ret-type ,((uniquify (append plist alist)) body))]
        [`(program ,ret-type . ,e) (begin
                           (match-define
                             `(,define-stmt ,flist) (uniquify-func (drop-right e 1) alist))
                           `(program ,ret-type ,@define-stmt ,((uniquify flist) (last e))))]
        [`(,op ,es ...) #:when (lookup op alist #f)
         `(,((uniquify alist) op) ,@(map (uniquify alist) es))]
        [`(,op ,es ...)#:when (pair? op)
         `(,((uniquify alist) op) ,@(map (uniquify alist) es))]
        [`(,op ,es ...)
         `(,op ,@(map (uniquify alist) es))]))))
