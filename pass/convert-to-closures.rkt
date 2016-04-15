#lang racket
(require "../utilities.rkt")

(provide convert-to-closures)

(define lambda-functions '())

(define fun-env '())
(define (envend v)
  (set! fun-env (cons v fun-env)))

(define (get-free-vars body env free)
  (match body
    [`(has-type (let ,vars ,b) ,ht) (set-union (foldr (lambda (e res)
                                                       (set-union res
                                                                  (get-free-vars e env free)))
                                                     (set) (map last vars))
                                               (get-free-vars b (append env (map car vars)) free))]
    [`(has-type (vector-set! ,e1 ,e2 ,e3) ,ht) (set-union (get-free-vars e1 env free)
                                                          (get-free-vars e2 env free)
                                                          (get-free-vars e3 env free))]
    [`(has-type ,expr ,ht) #:when (symbol? expr) (if (assoc expr env)
                                                     free
                                                     (set-union free (set `(,expr . ,ht))))]
    
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
    [`(has-type (let ,vars ,body) ,ht) (let ([vars^ (map (lambda (v)
                                                           (match-define `(,var ,e) v)
                                                           (define instre^ (clos-conv-helper e))
                                                           (envend `(,var . ,(last instre^)))
                                                           `(,var ,instre^)) vars)]
                                             [body^ (clos-conv-helper body)])
                                         (match-define `(has-type ,b-expr ,ntype) body^)
                                         `(has-type (let ,vars^ ,body^) ,ntype))]
    [`(define (,fname . ,vars) : ,ret ,body) (let ([closvar (gensym 'clos)]
                                                   [body^ (clos-conv-helper body)])
                                               (match-define `(has-type ,b-expr^ ,ntype) body^)
                                               (envend `(,fname . (Any ,@(map last vars) -> ,ntype)))
                                               ; (display "env: ") (displayln fun-env)
                                               `(define (,fname [,closvar : Any] . ,vars) : ,ntype ,body^))]
    [`(has-type (function-ref ,f) ,t1) (let ([ntype (lookup f fun-env t1)])
                                           `(has-type (vector
                                                       (has-type
                                                        (inject (has-type (function-ref ,f) ,ntype) ,ntype)
                                                        Any)) (Vector Any)))]
    [`(has-type (app ,e . ,es) ,ht) (let ([newvar (gensym)]
                                          [fune^ (clos-conv-helper e)])
                                      (match-define `(has-type ,expr1 ,ht1) e)
                                      (match-define `(has-type ,funexpr2 ,funht2) fune^)
                                      ;check if funht2 is working
                                      (set! funht2 (lookup funexpr2 fun-env (if (eqv? (car funht2) 'Vector)
                                                                                funht2
                                                                                `(Vector ,funht2))))
                                      ;(display "expr: ") (displayln expr)
                                      ;(display "e: ") (displayln e)
                                      ;(display "fune^: ") (displayln fune^)
                                      `(has-type (let ([,newvar ,fune^])
                                                   (has-type (app (project (has-type (vector-ref
                                                                                      (has-type (project (has-type (inject (has-type ,newvar ,funht2) ,funht2) Any) (Vectorof Any)) (Vectorof Any))
                                                                                      (has-type (project (has-type (inject (has-type 0 Integer) Integer) Any) Integer) Integer))
                                                                                     Any) )
                                                                  ;;ht2 replaced with _
                                                                  (has-type (inject (has-type ,newvar (Vector _)) (Vector _)) Any)
                                                                  ,@(map clos-conv-helper es)) Any)) Any))]
    [`(has-type (lambda: ,vars : ,ret ,body) ,ht)
     (let* ([lamvar (gensym 'lam)]
            [closvar (gensym 'clos)]
            [fvs (set->list (get-free-vars body vars (set)))]
            [clos-var-types (map (lambda (x) 'Any) fvs)]
            [var-types (map last vars)]
            [def-stmt (fvs-let-builder body closvar clos-var-types fvs 1)])
       (match-define `(has-type ,b ,htb) def-stmt)
       (define ret-type `(Vector ,ht ,@clos-var-types))
       (define lam-types `(,ret-type  ,@var-types -> ,htb))
       (set! lambda-functions (append lambda-functions ;;,ret-type replaced with _
                                      `((define (,lamvar [,closvar : Any] . ,vars) : ,htb ,def-stmt))))
       (envend `(,lamvar . (Any ,@(map last vars) -> ,htb)))
       (let ([lkp (lookup lamvar fun-env ht)])
         `(has-type (vector (has-type (inject (has-type (function-ref ,lamvar) ,lkp) ,lkp) Any)
                          ,@(map (lambda (x)
                                   `(has-type (inject (has-type ,(car x) ,(cdr x)) ,(cdr x)) Any)) fvs))
                  (Vector Any ,@clos-var-types))))]
    [`(has-type (if ,con ,thn ,els) ,ht) `(has-type (if ,(clos-conv-helper con)
                                                        ,(clos-conv-helper thn)
                                                        ,(clos-conv-helper els)) ,ht)]
    ;
    [`(has-type (vector . ,e) ,t) (let ([vec-vals (map clos-conv-helper e)])
                                    `(has-type (vector . ,vec-vals) (Vector . ,(map last vec-vals))))]
    [`(has-type (vector-set! ,vect (has-type ,index Integer) ,value) ,ht)
     (match-let ([`(has-type ,vect-name ,vect-type) (clos-conv-helper vect)]
                 [`(has-type ,value-expr ,value-type) (clos-conv-helper value)])
       `(has-type (vector-set!
                   (has-type ,vect-name ,(list-set vect-type (add1 index) value-type))
                   (has-type ,index Integer)
                   (has-type ,value-expr ,value-type)) ,ht))]
    [`(has-type (,op ,e1 ,e2) ,ht) ;;(displayln op) (displayln e2)
     `(has-type (,op ,(clos-conv-helper e1) ,(clos-conv-helper e2)) ,ht)]
    [`(has-type (,op ,e1) ,ht) `(has-type (,op ,(clos-conv-helper e1)) ,ht)]
    [`(has-type ,e ,t) `(has-type ,e ,(lookup e fun-env t))]
    [`(has-type (inject ,e ,t) Any) (match-define `(has-type ,e1 ,t1) (clos-conv-helper e))
                     `(has-type (inject (has-type ,e1 ,t1) ,t1) Any)]
    [`(has-type (project ,e ,t) ,t) (match-define `(has-type ,e1 Any) (clos-conv-helper e))
                     `(has-type (project (has-type ,e1 Any) ,t) ,t)]
    [else expr]))

(define (convert-to-closures expr)
  (set! lambda-functions '())
  (match-define `(program ,ret . ,e) expr)
  (let ([clos (map clos-conv-helper e)])
    `(program ,ret ,@lambda-functions ,@clos)))
