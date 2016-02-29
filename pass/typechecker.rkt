#lang racket
(require "../utilities.rkt")

(provide typecheck-R2)


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
     (define T (typecheck-R2 env e))
     (define new-env (cons (cons x T) env))
     (typecheck-R2 new-env body)]
    [`(not ,e)
     (match (typecheck-R2 env e)
       ['Boolean 'Boolean]
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
         (errorset))]
    [`(program ,body)
     (define _type (typecheck-R2 '() body))
     `(program (type ,_type) ,body)]))