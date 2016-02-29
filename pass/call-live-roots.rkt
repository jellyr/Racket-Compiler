#lang racket
(require "../common.rkt")

(provide call-live-roots)

(define (live-roots-vector? var type-env)
  (let ([lkp (lookup var type-env #f)])
    (match lkp
      [`(Vector . ,e1) #t]
      [else #f])))


(define (live-if-helper e type-env)
  (foldr (lambda (x r)
           (let* ([lives (if (null? r) (set) (car r))]
                  [newlives (live-analysis x lives type-env)])
             (cons newlives r)))
         '() e))

(define (live-analysis instr lak type-env)
  (define vector? (curryr live-roots-vector? type-env))
  (define (vector-unwrap var) (if (vector? var) (set var) (set)))
  (match instr
    [(? vector?) (set instr)]
    [`(allocate ,e) (set)]
    [`(assign ,var ,e) (let ([forsub (vector-unwrap var)]
                             [forunion (live-analysis e (set) type-env)])
                         (set-union forunion (set-subtract lak forsub)))]
    [`(vector-set! ,var ,index ,e) (let ([forunion (live-analysis e lak type-env)])
                                     (set-union lak forunion))]
    [`(vector-ref ,v ,index) (set-union lak (set v))]
    [`(if (eq? ,e1 ,e2) ,thn ,els)  (let ([e1set (live-analysis e1 lak type-env)]
                                          [e2set (live-analysis e2 lak type-env)]
                                          [thenset (live-if-helper thn type-env)]
                                          [elseset (live-if-helper els type-env)])
                                      (set-union e1set e2set (car thenset) (car elseset)))]
    [`(return ,var) (set)]
    [else lak]))


(define (call-live-roots e)
  
  (define (live-instr-helper instr livea)
    (match instr
      [`(if (collection-needed? ,e1)
            ((collect ,e2))
            ())
       `(if (collection-needed? ,e1)
            ((call-live-roots ,(set->list livea) (collect ,e2)))
            ())]
      [`(if (eq? ,e^1 ,e^2) ,thn ,els)
       `(if (eq? ,e^1 ,e^2) ,thn ,els)]
      [else instr]))
  
  (let* ([prog (car e)]
         [types (cadr e)]
         [ret-type (caddr e)]
         [instrs (cdddr e)]
         [livea (foldr (lambda (instr res)
                         (define value (live-analysis instr (car res) types))
                         (cons value res))
                       `(,(set))
                       instrs)])
    `(,prog ,(map car types) ,ret-type ,@(map live-instr-helper instrs (cdr livea)))))