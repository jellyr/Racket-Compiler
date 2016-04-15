#lang racket
(require "../utilities.rkt")

(provide typecheck-R2)

(define type-predicates
  (set 'boolean? 'integer? 'vector? 'procedure?))

;; input a list of define intrs
(define (defines-env instrs)
  (map (lambda (e)
         (match e
           [`(define (,funame . ,var-defs) : ,ret-type ,body)
            (let* ([new-env (map (lambda (def^)
                                   (match def^
                                     [`(,var : ,var-type) `(,var . ,var-type)]
                                     [else (error "in define[new-env]")])) var-defs)])
              `(,funame . (,@(map cdr new-env) -> ,ret-type)))]))
       instrs))

(define (typechecker-define-helper env e)
  (match e
    [`(define (,funame . ,var-defs) : ,ret-type ,body)
     (let* ([new-env (map (lambda (def^)
                            (match def^
                              [`(,var : ,var-type) `(,var . ,var-type)]
                              [else (error "in define[new-env]")])) var-defs)])
       (match-define `(,ret-type^ ,ret-e^) (typecheck-R2 (append env new-env) body))
       (if (equal? ret-type^ ret-type)
           (list ret-type^ `(define (,funame . ,var-defs) : ,ret-type ,ret-e^))
           (error "in define")))]))

(define (hast type^ expr^)
  (list type^ `(has-type ,expr^ ,type^)))

(define (typecheck-R2 env e)
  ;(display "e: ") (displayln e)
  (define recur (curry typecheck-R2 env))
  (match e
    [`(inject (function-ref ,funame) ,t)
     (hast 'Any `(inject (has-type (function-ref ,funame) ,t) ,t))]
    [`(inject ,e^ ,ty)
     (match-define `(,t1^ ,e1^) (recur e^))
     ;(displayln e1^)
     (cond
       [(equal? ty t1^) (hast 'Any `(inject ,e1^ ,ty))]
       [else (error "In inject")])]
    [`(project ,e^ ,ty)
     (match-define `(,t1^ ,e1^) (recur e^))
     (cond
       [(equal? t1^ 'Any) (hast ty `(project ,e1^ ,ty))]
       [else (error "In project")])]
    [`(,pred ,e^) #:when (set-member? type-predicates pred)
        (match-define `(,e-ty ,e1^) (recur e^))
        (cond
         [(equal? e-ty 'Any)
          (hast 'Boolean `(,pred e1^))]
         [else
          (error "predicate expected arg of type Any, not" e-ty)])]
    [`(vector-ref ,e^ ,ie)
     (match-define `(,t ,e1^) (recur e^))
     (match-define `(,t-i ,e-i) (recur ie))
     (match t
       ; change me
       ; [`(Vector . vec-args) ]
       [`(Vectorof ,t)
        (unless (equal? t-i 'Integer)
          (error 'type-check "invalid index ~a" ie))
        (list t `(has-type (vector-ref ,e1^ ,e-i) ,t))]
       [else (error "expected a vector in vector-ref, not" t)])]
    [`(vector-set! ,vec ,ie ,vec-arg)
     (match-define `(,t-vec ,e-vec^) (recur vec))
     (match-define `(,t-arg ,e-arg^) (recur vec-arg))
     (match-define `(,t-i ,e-i) (recur ie))
     (match t-vec
       ; change me
       ; [`(Vector ,ts ...) ...]
       [`(Vectorof ,t)
        (unless (equal? t-i 'Integer)
          (error 'type-check "invalid index ~a" ie))
        (unless (equal? t t-arg)
          (error 'type-check "type mismatch in vector-set! ~a ~a" 
                 t t-arg))
        (list 'Void `(has-type (vector-set! ,e-vec^
                                            (has-type ,e-i Integer)
                                            ,e-arg^) Void))]
       [else (error 'type-check
                    "expected a vector in vector-set!, not ~a"
                    t-vec)])]
    [(? fixnum?) (hast 'Integer e)]
    [(? boolean?) (hast 'Boolean e)]
    [(? symbol?) (define t^ (lookup e env)) (hast t^ e)]
    [`(read) (hast 'Integer e)]
    [`(void) (hast 'Void e)]
    [`(+ ,e1 ,e2)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match-define `(,t2^ ,e2^) (typecheck-R2 env e2))
     (match `(,t1^ ,t2^)
       ['(Integer Integer) (hast 'Integer `(+ ,e1^ ,e2^))]
       [else (error "In +:")])]
    [`(- ,e1)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match t1^
       ['Integer (hast 'Integer `(- ,e1^))]
       [else (error "in -")])]
    [`(if ,econd ,ethen ,eelse)
     (match-define `(,cond-t^ ,cond-e^) (typecheck-R2 env econd))
     (match cond-t^
       ['Boolean (match-let ([`(,then-t^ ,then-e^) (typecheck-R2 env ethen)]
                             [`(,else-t^ ,else-e^) (typecheck-R2 env eelse)])
                   (if (equal? then-t^ else-t^)
                       (hast then-t^ `(if ,cond-e^ ,then-e^ ,else-e^))
                       (error "in if")))]
       [else (error "in if")])]
    
    [`(let ([,x ,e]) ,body)
     (match-define `(,para-t ,para-e) (typecheck-R2 env e))
     (define new-env (cons (cons x para-t) env))
     (match-define `(,body-t ,body-e) (typecheck-R2 new-env body))
     (hast body-t `(let ([,x ,para-e]) ,body-e))
     ]
    ;; maybe just need this one
    [`(let ,var-defines ,body)
     (define new-env env)
     (let ([vars^ (map (lambda (vd)
                         (match-define `(,v^ ,vare^) vd)
                         (match-define `(,para-t ,para-e) (typecheck-R2 env vare^))
                         (set! new-env (cons (cons v^ para-t) new-env))
                         `(,v^ ,para-e)) var-defines)])
       (match-define `(,body-t ,body-e) (typecheck-R2 new-env body))
       (hast body-t `(let ,vars^ ,body-e))
       )]
    
    [`(not ,e1)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match t1^
       ['Boolean (hast 'Boolean `(not ,e1^))]
       [else (error "in not")])]
    
    [`(eq? ,e1 ,e2)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match-define `(,t2^ ,e2^) (typecheck-R2 env e2))
     (match `(,t1^ ,t2^)
       ['(Boolean Boolean) (hast 'Boolean `(eq? ,e1^ ,e2^))]
       ['(Integer Integer) (hast 'Boolean `(eq? ,e1^ ,e2^))]
       ['(Any Any) (hast 'Boolean `(eq? ,e1^ ,e2^))]
       [else (error "In eq?")])]
    
    [`(and ,e1 ,e2)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match-define `(,t2^ ,e2^) (typecheck-R2 env e2))
     (match `(,t1^ ,t2^)
       ['(Boolean Boolean) (hast 'Boolean `(and ,e1^ ,e2^))]
       [else (error "In and")])]

    ;; need to consider vector
    
    [`(vector . ,expr)
     (define value-list (map (curry typecheck-R2 env) expr))
     (hast `(Vector ,@(map car value-list)) `(vector ,@(map last value-list)))]
    
    [`(vector-ref ,expr ,number)
     (match-define `(,vector-t^ ,vector-e^) (typecheck-R2 env expr))
     (define erroref (curry error "in vector ref"))
     (match vector-t^
       [`(Vector) (error "empty vector")]
       [`(Vector . ,type-expr) ;; see if we can check index?
        (match-define `(,number-t^ ,number-e^) (typecheck-R2 env number))
        (match-define `(,vector-t^ ,vector-e^) (typecheck-R2 env expr))
        (if (eqv? number-t^ 'Integer)
            (hast (list-ref vector-t^ (add1 number)) `(vector-ref ,vector-e^ ,number-e^))
            (erroref))]
       [else (erroref)])]
    
    [`(vector-set! ,expr1 ,number ,expr2)
     (define errorset (curry error "in vector set"))
     (match-let ([`(,vector-t^ ,vector-e^) (typecheck-R2 env expr1)]
                 [`(,expr-t^ ,expr-e^) (typecheck-R2 env expr2)]
                 [`(,number-t^ ,number-e^) (typecheck-R2 env number)])
       (if (eq? number-t^ 'Integer)
         (if (equal? (list-ref vector-t^ (add1 number)) expr-t^)
             (hast 'Void `(vector-set! ,vector-e^ ,number-e^ ,expr-e^))
             (errorset))
         (errorset)))]
    [`(lambda: ,paras : ,ret-type ,body)
     ;; (displayln paras)
     (define lambda-env (map (lambda (para-e)
                               (match-define `(,var : ,t) para-e)
                               `(,var . ,t)) paras))
     (match-define `(,body-type ,body-e) (typecheck-R2 (append lambda-env env) body))
     (if (equal? body-type ret-type)
         (hast `(,@(map cdr lambda-env) -> ,body-type) `(lambda: ,paras : ,ret-type ,body-e))
         (error "in lambda"))]
    
    [`(program . ,expr)
     ;; 
     (define defs (drop-right expr 1))
     (define body (last expr))
     (define new-env (defines-env defs))
     (define def-values^ (map (curry typechecker-define-helper new-env) defs))
     ;(displayln new-env^)
     (match-define `(,type^ ,expr^) (typecheck-R2 new-env body))
     `(program (type ,type^) ,@(map last def-values^) ,expr^)]
    
    [`(app ,fun-call . ,paras)
     (match-define `(,func-type ,func-e) (typecheck-R2 env fun-call))
     (match func-type
       [`(,paras-types ... -> ,ret-type)
        (define value-list (if (eq? (length paras) (length paras-types))
             (map (lambda (v t)
                    (define tuple (typecheck-R2 env v))
                    (if (equal? t (car tuple))
                        tuple
                        (error "in func-call"))) paras paras-types)
             (error "in func-call")))
        (hast ret-type `(app ,func-e ,@(map last value-list)))])]

    
    
    ))

(define test (curry typecheck-R2 '()))
