#lang racket
(require "../utilities.rkt")

(provide convert-to-closures)

(define (get-free-vars body env free)
  (match body
    [(? symbol?) (if (member body env) free (set-union free body))]
    [`(let ,vars ,body) (get-free-vars body (append env (map car vars)) free)]
    [`(lambda ,vars ,body) (get-free-vars body (append env vars) free)]
    [`(,op ,e1 ,e2) (set-union (get-free-vars e1 env free) (get-free-vars e2 env free))]
    [`(,op ,e1) (get-free-vars e1 env free)]
    [`(if ,con ,thn ,els) (set-union (get-free-vars con env free)
                                     (get-free-vars thn env free)
                                     (get-free-vars els env free))]))

(define (fvs-let-builder body cvar fvs idx)
  (cond
    [(> idx (length fvs) (clos-conv-helper body))]
    [else `(let ([,(list-ref fvs idx) (vector-ref ,cvar ,idx)])
             ,(fvs-let-builder fvs (add1 idx)))]))

(define (clos-conv-helper expr)
  (match expr
    [`(let ,vars ,body) `(let ,(map (lambda (v)
                                      (match-define `(,var ,e) v)
                                      `(,var ,(clos-conv-helper e))) vars)
                           ,(clos-conv-helper body))]
    [`(define (,fname . ,vars) : ,ret ,body) `(define (,fname . ,vars) : ,ret ,(clos-conv-helper body))]
    [`(function-ref ,f) `(vector (function-ref ,f))]
    [`(app ,e . ,es) (let ([e^ (clos-conv-helper e)])
                       `(app (vector-ref ,e^ 0) ,e^ ,@(map clos-conv-helper es)))]
    [`(lambda: ,vars : ,ret ,body) (let ([lamvar (gensym 'lam)]
                                         [closvar (gensym 'clos)]
                                         [fvs (get-free-vars body vars)])
                                     `(vector ,lamvar ,@fvs)
                                     `(define (,lamvar [,closvar : _] . ,vars)
                                        ,(fvs-let-builder body closvar fvs 1)))]
    [`(,op ,e1 ,e2) `(,op ,(clos-conv-helper e1) ,(clos-conv-helper e2))]
    [`(,op ,e1) `(,op ,(clos-conv-helper e1))]
    [else expr]))

(define (convert-to-closures expr)
  (match-define `(program ,ret . ,e) expr)
  (define defs (drop-right e 1))
  `(program ,(map clos-conv-helper e)))
