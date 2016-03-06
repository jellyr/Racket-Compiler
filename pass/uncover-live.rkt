#lang racket
(require "../common.rkt")

(provide uncover-live)

(define (uncover-live-unwrap e)
  (match e
    [`(var ,e1) (set e1)]
    [`(xorq (int 1) (var ,s)) (set s)]
    [`(offset ,e1 ,idx) (uncover-live-unwrap e1)]
    ;[`(function-ref ,e1) (set e1)]
    ;[`(reg ,r) (set r)]
    [else (set)]))

(define (uncover-live-helper e lak)
  (define lak^ (list->set lak))
  (match e
    ; [`(callq read_int) (set-add lak^^ 'rax)]
    ;[`(negq ,e1) (set-union lak^^ (uncover-live-unwrap e1))
    [`(if (eq? ,e1 ,e2) ,thn ,els) (let* ([thenexpr (instrs-live-helper thn lak^)]
                                          [elseexpr (instrs-live-helper els lak^)]
                                          [thenexpr (if (null? thenexpr) (list '(()) `(,(set))) thenexpr)]
                                          [elseexpr (if (null? elseexpr) (list '(()) `(,(set))) elseexpr)]
                                          [thenset (if (null? thenexpr) (set) (car (last thenexpr)))]
                                          [elseset (if (null? elseexpr) (set) (car (last elseexpr)))]
                                          [thenexpr (list (car thenexpr)
                                                          (if (null? (caar thenexpr)) `(,(set)) (cdr (last thenexpr))))]
                                          [elseexpr (list (car elseexpr)
                                                          (if (null? (caar elseexpr)) `(,(set)) (cdr (last elseexpr))))]
                                          
                                          
                                          )
                                     (list `(if (eq? ,e1 ,e2) ,@thenexpr ,@elseexpr)
                                           (set-union thenset elseset)))]
    [`(leaq ,e1 ,e2) #:when(eq? 'offset (car e2)) (list e (set-union lak^
                                                                     (uncover-live-unwrap e1)
                                                                     (uncover-live-unwrap e2)))]
    [`(leaq ,e1 ,e2) (list e (set-union (set-subtract lak^ (uncover-live-unwrap e2)) (uncover-live-unwrap e1)))]
    [`(movq ,e1 ,e2) #:when(eq? 'offset (car e2)) (list e (set-union lak^
                                                                     (uncover-live-unwrap e1)
                                                                     (uncover-live-unwrap e2)))]
    [`(movq ,e1 ,e2) (list e (set-union (set-subtract lak^ (uncover-live-unwrap e2)) (uncover-live-unwrap e1)))]
    [`(cmpq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(movzbq ,e1 ,e2) (list e (set-subtract lak^ (uncover-live-unwrap e2)))]
    [`(addq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    [`(subq ,e1 ,e2) (list e (set-union lak^ (uncover-live-unwrap e1) (uncover-live-unwrap e2)))]
    ;; consider callq?
    [`(indirect-callq ,e1) (list e (set-union lak^ (uncover-live-unwrap e1)))]
    [else (list e lak^)]))


;; lak is set
(define (instrs-live-helper e lak)
  (define lak-list (set->list lak))
  (foldr (lambda (x r)
           ;(println (if (null? r) `(,(list->set lak-list)) (cadr r)))
           (let* ([expr (if (null? r) `() (car r))]
                  [lives (if (null? r) `(,(list->set lak-list)) (cadr r))]
                  [helpexpr (uncover-live-helper x (if (null? (cdr lives)) (set->list lak) (car lives)))])
             (list (cons (car helpexpr) expr)
                   (cons (cadr helpexpr) lives))))
         '() e))


(define (foldr-helper instrs)
  (foldr (lambda (x r)
           (let* ([expr (if (null? r) '() (car r))]
                  [lives (if (null? r) `(,(set)) (cadr r))]
                  [helpexpr (uncover-live-helper x (if (null? lives) (set) (car lives)))])
             (list (cons (car helpexpr) expr)
                   (cons (cadr helpexpr) lives))))
         '() instrs))

(define (def-helper instr)
  (match-define `(define ,funame ,index (,paras ,maxstack) . ,body) instr)
  (define defexpr (foldr-helper body))
  `(define ,funame ,index (,paras ,maxstack ,(cdadr defexpr)) ,@(car defexpr)))

(define (uncover-live e)
  (match-define `(program (,paras ,maxstack) ,ret-type (defines . ,defs) . ,body) e)
  (let ([mainexpr (foldr-helper body)])
    `(program ,(list paras maxstack (cdadr mainexpr)) ,ret-type (defines ,(map def-helper defs)) ,@(car mainexpr))))

