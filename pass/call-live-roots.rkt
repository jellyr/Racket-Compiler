#lang racket
(require "../common.rkt")

(provide call-live-roots)

(define (live-if-helper e)
  (foldr (lambda (x r)
           (let* ([lives (if (null? r) (set) (car r))]
                  [newlives (live-analysis x lives)])
             (cons newlives r)))
         '() e))

(define (live-analysis instr lak)
  (define (vector-unwrap var) (if (vector? var) (set var) (set)))
  (match instr
    [`(has-type ,x ,t) #:when (and (pair? t) (equal? (car t) 'Vector)) (set instr)]
    [`(has-type (allocate ,e) ,t) (set)]
    [`(has-type (assign ,var ,e) ,t) (let ([forsub (vector-unwrap var)]
                                           [forunion (live-analysis e (set))])
                           (set-union forunion (set-subtract lak forsub)))]
    [`(has-type (vector-set! ,var ,index ,e) ,t) (let ([forunion (live-analysis e lak)])
                                       (set-union lak forunion))]
    [`(has-type (vector-ref ,v ,index) ,t) (set-union lak (set v))]
    [`(has-type (if (eq? ,e1 ,e2) ,thn ,els) ,t)  (let ([e1set (live-analysis e1 lak)]
                                            [e2set (live-analysis e2 lak)]
                                            [thenset (live-if-helper thn)]
                                            [elseset (live-if-helper els)])
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
      [`(if (has-type (eq? ,e1 ,e2) ,t) ,thn ,els)  (let ([texpr (map (curryr live-instr-helper livea) thn)]
                                              [eexpr (map (curryr live-instr-helper livea) els)])
                                        `(if (eq? ,e1 ,e2) ,texpr ,eexpr))]
       ;;Need to change to make this work in select-instrs
      [`(assign ,var (has-type (app . ,e1) ,t)) `(assign ,var (call-live-roots () (app . ,e1)))]
      [`(define (,fname . ,params) : ,ret . ,body)
       `(define (,fname . ,params) : ,ret . ,(map live-instr-helper body (cdr livea)))]
      [else instr]))
  (match-define `(program ,tvars ,ret-type (defines . ,defs) . ,body) e)
  (let* ([calc-live (lambda (instr res)
                         (define value (live-analysis instr (car res)))
                         (cons value res))]
         [live-main (foldr calc-live `(,(set)) body)]
         [live-defs (map (lambda (def)
                           (match-define `(define (,fname . ,params) : ,ret . ,def-body) def)
                           (foldr calc-live `(,(set)) def-body)) defs)])
    `(program ,tvars ,ret-type
              (defines ,@(map live-instr-helper defs live-defs))
              ,@(map live-instr-helper body (cdr live-main)))))
