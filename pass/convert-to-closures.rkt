#lang racket
(require "../utilities.rkt")

(provide convert-to-closures)

(define lambda-functions '())

(define (get-free-vars body env free)
  (match body

    [`(has-type ,expr ,ht) (get-free-vars expr env free)]
    [(? symbol?) (if (assoc body env) free (set-union free (set body)))]
    [`(let ,vars ,body) (get-free-vars body (append env vars) free)]
    [`(lambda ,vars ,body) (get-free-vars body (append env vars) free)]
    [`(,op ,e1 ,e2) (set-union (get-free-vars e1 env free) (get-free-vars e2 env free))]
    [`(,op ,e1) (get-free-vars e1 env free)]
    [`(if ,con ,thn ,els) (set-union (get-free-vars con env free)
                                     (get-free-vars thn env free)
                                     (get-free-vars els env free))]))

(define (fvs-let-builder body cvar fvs idx)
  (cond
    [(> idx (length fvs)) (clos-conv-helper body)]
    [else `(let ([,(list-ref fvs (sub1 idx)) (vector-ref ,cvar ,idx)])
             ,(fvs-let-builder body cvar fvs (add1 idx)))]))

(define (clos-conv-helper expr)
  (match expr
    [`(has-type ,instr ,ht) `(has-type ,(clos-conv-helper instr) ,ht)]
    [`(let ,vars ,body) `(let ,(map (lambda (v)
                                      (match-define `(,var ,e) v)
                                      `(,var ,(clos-conv-helper e))) vars)
                           ,(clos-conv-helper body))]
    [`(define (,fname . ,vars) : ,ret ,body) (let ([closvar (gensym 'clos)])
                                               `(define (,fname [,closvar : _] . ,vars) : ,ret ,(clos-conv-helper body)))]
    [`(function-ref ,f) `(vector (function-ref ,f))]
    [`(app ,e . ,es) (let ([newvar (gensym)])
                       `(let ([,newvar ,(clos-conv-helper e)])
                          (app (vector-ref ,newvar 0) ,newvar ,@(map clos-conv-helper es))))]
    [`(lambda: ,vars : ,ret ,body) (let ([lamvar (gensym 'lam)]
                                         [closvar (gensym 'clos)]
                                         [fvs (set->list (get-free-vars body vars (set)))])
                                     (set! lambda-functions (append lambda-functions
                                                                    `((define (,lamvar [,closvar : _] . ,vars)
                                                                        ,(fvs-let-builder body closvar fvs 1)))))
                                     `(vector (function-ref ,lamvar) ,@fvs))]
    [`(,op ,e1 ,e2) `(,op ,(clos-conv-helper e1) ,(clos-conv-helper e2))]
    [`(,op ,e1) `(,op ,(clos-conv-helper e1))]
    [else expr]))

(define (convert-to-closures expr)
  (match-define `(program ,ret . ,e) expr)
  (define defs (drop-right e 1))
  (let ([clos (map clos-conv-helper e)])
    `(program ,ret ,@lambda-functions ,@clos)))
