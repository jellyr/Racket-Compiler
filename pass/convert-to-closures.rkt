#lang racket
(require "../utilities.rkt")

(provide convert-to-closures)

(define lambda-functions '())

(define (get-free-vars body env free)
  (match body
    [`(has-type ,expr ,ht) #:when (symbol? expr) (if (assoc expr env)
                                                     free
                                                     (set-union free (set `(,expr . ,ht))))]
    [`(has-type (let ,vars ,b) ,ht) (get-free-vars b (append env vars) free)]
    [`(has-type (lambda: ,vars : ,ret ,b) ,ht) (get-free-vars b (append env vars) free)]
    [`(has-type (,op ,e1 ,e2) ,ht) (set-union (get-free-vars e1 env free)
                                              (get-free-vars e2 env free))]
    [`(has-type (,op ,e1) ,ht) (get-free-vars e1 env free)]
    [`(has-type (if ,con ,thn ,els) ,ht) (set-union (get-free-vars con env free)
                                                    (get-free-vars thn env free)
                                                    (get-free-vars els env free))]
    [else free]))

(define (fvs-let-builder body cvar clos-var-types fvs idx)
  (cond
    [(> idx (length fvs)) (clos-conv-helper body)]
    [else (let ([expr (fvs-let-builder body cvar clos-var-types fvs (add1 idx))])
            (match-define `(has-type ,b ,ht) expr)
            `(has-type (let ([,(car (list-ref fvs (sub1 idx))) (has-type (vector-ref ,cvar ,idx) ,(list-ref clos-var-types (sub1 idx)))])
                         ,expr) ,ht))]))

(define (clos-conv-helper expr)
  (match expr
    ;;[`(has-type ,instr ,ht) `(has-type ,(clos-conv-helper instr) ,ht)]
    [`(has-type (let ,vars ,body) ,ht) `(has-type (let ,(map (lambda (v)
                                                               (match-define `(,var ,e) v)
                                                               `(,var ,(clos-conv-helper e))) vars)
                                                    ,(clos-conv-helper body)) ,ht)]
    [`(define (,fname . ,vars) : ,ret ,body) (let ([closvar (gensym 'clos)])
                                               `(define (,fname [,closvar : (Vector _)] . ,vars) : ,ret ,(clos-conv-helper body)))]
    [`(has-type (function-ref ,f) ,ht) `(has-type (vector (has-type (function-ref ,f) ,ht)) (Vector ,ht))]
    [`(has-type (app ,e . ,es) ,ht) (let ([newvar (gensym)]
                                          [e^ (clos-conv-helper e)])
                                      (match-define `(has-type ,expr1 ,ht1) e)
                                      (match-define `(has-type ,expr2 ,ht2) e^)
                                      ;;(print "Hello ")
                                      ;;(displayln expr2)
                                      `(has-type (let ([,newvar ,e^])
                                                   (has-type (app (has-type (vector-ref
                                                                             (has-type ,newvar ,ht2)
                                                                             (has-type 0 Integer)) ,(cadr `(Vector ht2)))
                                                                  ;;ht2 replaced with _
                                                                  (has-type ,newvar (Vector _))
                                                                  ,@(map clos-conv-helper es)) ,ht)) ,ht))]
    [`(has-type (lambda: ,vars : ,ret ,body) ,ht)
     (let* ([lamvar (gensym 'lam)]
            [closvar (gensym 'clos)]
            [fvs (set->list (get-free-vars body vars (set)))]
            [clos-var-types (map cdr fvs)]
            [var-types (map last vars)]
            [def-stmt (fvs-let-builder body closvar clos-var-types fvs 1)])
       (match-define `(has-type ,b ,htb) def-stmt)
       (define ret-type `(Vector ,ht ,@clos-var-types))
       (define lam-types `(,ret-type  ,@var-types -> ,htb))
       (set! lambda-functions (append lambda-functions ;;,ret-type replaced with _
                                      `((define (,lamvar [,closvar : (Vector _)] . ,vars) : ,htb ,def-stmt))))
       `(has-type (vector (has-type (function-ref ,lamvar) (Vector _)) ,@(map (lambda (x)
                                                                                `(has-type ,(car x) ,(cdr x)))
                                                                              fvs))
                  (Vector (Vector _) ,@clos-var-types)))]
    [`(has-type (if ,con ,thn ,els) ,ht) `(has-type (if ,(clos-conv-helper con)
                                                        ,(clos-conv-helper thn)
                                                        ,(clos-conv-helper els)) ,ht)]
    [`(has-type (vector . ,e) ,t) (let ([vec-vals (map clos-conv-helper e)])
                                    `(has-type (vector . ,vec-vals) (Vector . ,(map last vec-vals))))]
    [`(has-type (,op ,e1 ,e2) ,ht) ;;(displayln op) (displayln e2)
     `(has-type (,op ,(clos-conv-helper e1) ,(clos-conv-helper e2)) ,ht)]
    [`(has-type (,op ,e1) ,ht) `(has-type (,op ,(clos-conv-helper e1)) ,ht)]
    [else expr]))

(define (convert-to-closures expr)
  (set! lambda-functions '())
  (match-define `(program ,ret . ,e) expr)
  (let ([clos (map clos-conv-helper e)])
    `(program ,ret ,@lambda-functions ,@clos)))
