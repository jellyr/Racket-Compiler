#lang racket
(require "../utilities.rkt")

(provide typecheck-R2)

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
       (define ret-type^ (typecheck-R2 (append env new-env) body))
       (if (equal? ret-type^ ret-type)
           ret-type^
           (error "in define")))]))

(define (typecheck-R2 env e)
  (match e
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol?) (lookup e env)]
    [`(read) 'Integer]
    [`(+ ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Integer Integer) 'Integer]
       [else (error "In +")])]
    [`(- ,e1)
     (match (typecheck-R2 env e1)
       ['Integer 'Integer]
       [else (error "in -")])]
    (`(if ,econd ,ethen ,eelse)
     (match (typecheck-R2 env econd)
       ['Boolean (let ([tthen (typecheck-R2 env ethen)]
                       [telse (typecheck-R2 env eelse)])
                   (if (eqv? tthen telse)
                       tthen
                       (error "in if")))]
       [else (error "in if")]))
    [`(let ([,x ,e]) ,body)

     (match-define `(,para-t ,para-e) (typecheck-R2 env e))
     (define new-env (cons (cons x para-t) env))
     (match-define `(,body-t ,body-e) (typecheck-R2 new-env body))
     (hast body-t `(let ([,x ,para-e]) ,body-e))
     ]
    
    [`(not ,e1)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match t1^
       ['Boolean (hast 'Boolean `(not ,e1^))]
       [else (error "in not")])]
    [`(eq? ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Boolean Boolean) 'Boolean]
       ['(Integer Integer) 'Boolean]
       [else (error "In eq?")])]
    [`(and ,e1 ,e2)
     (match `(,(typecheck-R2 env e1) ,(typecheck-R2 env e2))
       ['(Boolean Boolean) 'Boolean]
       [else (error "In and")])]
    [`(vector . ,expr)
     `(Vector ,@(map (curry typecheck-R2 env) expr))]
    [`(vector-ref ,expr ,number)
     (define vector_t (typecheck-R2 env expr))
     (define erroref (curry error "in vector ref"))
     (match vector_t
       [`(Vector) (error "empty vector")]
       [`(Vector . ,expr) ;; see if we can check index?
        (if (eqv? (typecheck-R2 env number) 'Integer)
            (list-ref vector_t (add1 number))
            (erroref))]
       [else (erroref)])]
    [`(vector-set! ,expr1 ,number ,expr2)
     (define vector_t (typecheck-R2 env expr1))
     (define errorset (curry error "in vector set"))
     (if (eq? (typecheck-R2 env number) 'Integer)
         (if (equal? (list-ref vector_t (add1 number)) (typecheck-R2 env expr2))
             'Void
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
     (map (curry typechecker-define-helper new-env) defs)
     (define _type (typecheck-R2 new-env body))
     `(program (type ,_type) ,@expr)]
    
    [`(,fun-call . ,paras)
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
        (hast ret-type `(,func-e ,@(map last value-list)))])]
    ))

(define test (curry typecheck-R2 '()))

