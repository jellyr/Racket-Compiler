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

(define (hast type^ expr^)
  (list type^ `(has-type ,expr^ ,type^)))

(define (typecheck-R2 env e)
  (match e
    [(? fixnum?) (hast 'Integer e)]
    [(? boolean?) (hast 'Boolean e)]
    [(? symbol?) (define t^ (lookup e env)) (hast t^ e)]
    [`(read) (hast 'Integer e)]
    [`(+ ,e1 ,e2)
     (match-define `(,t1^ ,e1^) (typecheck-R2 env e1))
     (match-define `(,t2^ ,e2^) (typecheck-R2 env e2))
     (match `(,t1^ ,t2^)
       ['(Integer Integer) (hast 'Integer `(+ ,e1^ ,e2^))]
       [else (error "In +")])]
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
                   (if (eqv? then-t^ else-t^)
                       (hast then-t^ `(if ,cond-e^ ,then-e^ ,else-e^))
                       (error "in if")))]
       [else (error "in if")])]
    
    [`(let ([,x ,e]) ,body)
     (match-define `(,para-t ,para-e) (typecheck-R2 env e))
     (define new-env (cons (cons x para-t) env))
     (match-define `(,body-t ,body-e) (typecheck-R2 new-env body))
     (hast body-t `(let ([,x ,para-e] ,body-e)))
     ]
    
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
    
    ;; [`(vector-ref ,expr ,number)
    ;;  (define vector_t (typecheck-R2 env expr))
    ;;  (define erroref (curry error "in vector ref"))
    ;;  (match vector_t
    ;;    [`(Vector) (error "empty vector")]
    ;;    [`(Vector . ,expr) ;; see if we can check index?
    ;;     (if (eqv? (typecheck-R2 env number) 'Integer)
    ;;         (list-ref vector_t (add1 number))
    ;;         (erroref))]
    ;;    [else (erroref)])]
    ;; [`(vector-set! ,expr1 ,number ,expr2)
    ;;  (define vector_t (typecheck-R2 env expr1))
    ;;  (define errorset (curry error "in vector set"))
    ;;  (if (eq? (typecheck-R2 env number) 'Integer)
    ;;      (if (equal? (list-ref vector_t (add1 number)) (typecheck-R2 env expr2))
    ;;          'Void
    ;;          (errorset))
    ;;      (errorset))]
    
    [`(program . ,expr)
     ;; 
     (define defs (drop-right expr 1))
     (define body (last expr))
     (define new-env (defines-env defs))
     (define new-env^ (map (curry typechecker-define-helper new-env) defs))
     ;(displayln new-env^)
     (match-define `(,type^ ,expr^) (typecheck-R2 new-env body))
     `(program (type ,type^) ,expr^)]
    
    ;; [`(,fun-call . ,paras)
    ;;  (let ([func-type (lookup fun-call env (typecheck-R2 env fun-call))])
    ;;    (match func-type
    ;;      [`(,paras-types ... -> ,ret-type)
    ;;       ;; need to check
    ;;       (map (lambda (v t)
    ;;              (if (equal? t (typecheck-R2 env v))
    ;;                  #t
    ;;                  (error "in func-call"))) paras paras-types)
    ;;       ret-type]))]

    ))

(define test (curry typecheck-R2 '()))



;; (has-type (let ([(has-type x Int) (has-type 41 Int)])
;;             (has-type (+ (has-type x Int) (has-type 1 Int)) Int)) Int)
