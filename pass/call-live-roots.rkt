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
      [`(if (eq? ,e1 ,e2) ,thn ,els)  (let ([texpr (map (curryr live-instr-helper livea) thn)]
                                            [eexpr (map (curryr live-instr-helper livea) els)])
                                        `(if (eq? ,e1 ,e2) ,texpr ,eexpr))]
      [`(assign ,var (app . ,e1)) `(assign ,var (call-live-roots ,(set->list livea) (app . ,e1)))]
      [`(define (,fname . ,params) : ,ret ,types . ,body)
       `(define (,fname . ,params) : ,ret ,(map car types) . ,(map live-instr-helper body (cdr livea)))]
      [else instr]))
  (match-define `(program ,types ,ret-type (defines . ,defs) . ,body) e)
  (let* ([calc-live (lambda (types instr res)
                         (define value (live-analysis instr (car res) types))
                         (cons value res))]
         [live-main (foldr (curry calc-live types) `(,(set)) body)]
         [live-defs (map (lambda (def)
                           (match-define `(define (,fname . ,params) : ,ret ,def-types . ,def-body) def)
                           (foldr (curry calc-live def-types) `(,(set)) def-body)) defs)])
    `(program ,(map car types)
              ,ret-type
              (defines ,@(map live-instr-helper defs live-defs))
              ,@(map live-instr-helper body (cdr live-main)))))
