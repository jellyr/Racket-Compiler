#lang racket
(require "../utilities.rkt")

(provide untyped-typed)

(define env '())

(define (envend key val)
  (set! env (cons `(,key . ,val) env)))

(define (conv-helper e)
  (match e
    [(? boolean?) `(inject ,e Boolean)]
    [(? fixnum?) `(inject ,e Integer)]
    [(? symbol?) e]
    [`(read) `(inject (read) Integer)]
    [`(void) `(inject (void) Void)]
    [`(+ ,e1 ,e2) `(inject (+ (project ,(conv-helper e1) Integer)
                              (project ,(conv-helper e2) Integer)) Integer)]
    [`(- ,e1) `(inject (- (project ,(conv-helper e1) Integer)) Integer)]
    [`(vector . ,e1) (let ([val (map conv-helper e1)])
                       `(inject (vector . ,val) (Vector . ,(map (lambda x: 'Any) val))))]
    [`(vector-ref ,e1 ,e2) #:when (fixnum? e2) (let ([tmp1 (gensym 'tmp)]
                                                     [tmp2 (gensym 'tmp)])
                                                 `(let ([,tmp1 (project ,(conv-helper e1) (Vectorof Any))])
                                                    (let ([,tmp2 (project ,(conv-helper e2) Integer)])
                                                      (vector-ref ,tmp1 ,tmp2))))]
    [`(vector-set! ,e1 ,e2 ,e3) #:when (fixnum? e2) (let ([tmp1 (gensym 'tmp)]
                                                          [tmp2 (gensym 'tmp)]
                                                          [tmp3 (gensym 'tmp)])
                                                      `(let ([,tmp1 (project ,(conv-helper e1) (Vectorof Any))])
                                                         (let([,tmp2 (project ,(conv-helper e2) Integer)])
                                                           (let ([,tmp3 ,(conv-helper e3)])
                                                             (inject (vector-set! ,tmp1 ,tmp2 ,tmp3) Void)))))]
    [`(eq? ,e1 ,e2) `(inject (eq? ,(conv-helper e1) ,(conv-helper e2)) Boolean)]
    [`(if ,con ,thn, els) `(if (eq? ,(conv-helper con) (inject #f Boolean))
                               ,(conv-helper els)
                               ,(conv-helper thn))]
    [`(and ,e1 ,e2) (let ([tmp (gensym 'tmp)])
                      `(let ([,tmp ,(conv-helper e1)])
                         (if (eq? ,tmp (inject #f Boolean))
                             ,tmp
                             ,(conv-helper e2))))]
    [`(app ,e0 . ,e1) `(app (project ,(conv-helper e0) (,@(map (lambda x: 'Any) e1) -> Any))
                            ,@(map conv-helper e1))]
    [`(lambda ,x ,b) `(inject (lambda: ,(map (lambda (var)
                                                `[,var : Any]) x) : Any ,(conv-helper b))
                              (,@(map (lambda x: 'Any) x) -> Any))]
    [`(let ,x ,b) (let ([xvals (map (lambda (val)
                                      (match-define `(,var ,expr) val)
                                      `(,var ,(conv-helper expr))) x)])
                    `(let ,xvals ,(conv-helper b)))]
    [`(define (,fname . ,x) ,b) (envend fname `(,@(map (lambda x: 'Any) x) -> Any))
                               `(define (,fname . ,(map (lambda (var)
                                                          `[,var : Any]) x)) : Any ,(conv-helper b))]
    [`(function-ref ,f) `(inject (function-ref ,f) ,(lookup f env))]))

(define (untyped-typed e)
  (match-define `(program . ,expr) e)
  `(program . ,(map conv-helper expr)))
