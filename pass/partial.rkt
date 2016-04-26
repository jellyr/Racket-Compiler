#lang racket
(require "../common.rkt")

(provide partial)

;;; in this file we need to eval the s-expression


;;; define some simple example


;;; let para estimate : the var is not used in let expression

;;; let eval

;; if statement eval: the statement is true or false

;; + -, eval, some case read textbook

;;

;;

(define maxdepth 10)

(define (var-occurs? var instr)
  (define recur (lambda (x) (var-occurs? var x)))
  (match instr
    [`(has-type ,e1 ,t) #:when (equal? e1 var) (call/cc (lambda (k) (k #t)))]
    [`(has-type (let ((,var (has-type ,para ,var-t))) ,b) ,ht) (or (recur para) (recur b))]
    [`(has-type (vector-set! ,e1 ,e2 ,e3) ,ht) (or (recur e1) (recur e2) (recur e3))]
    [`(has-type (lambda: ,vars : ,ret ,b) ,ht) (recur b)]
    [`(has-type (,op ,e1 ,e2) ,ht) (or (recur e1) (recur e2))]
    [`(has-type (,op ,e1) ,ht) (recur e1)]
    [`(has-type (if ,con ,thn ,els) ,ht) (or (recur con) (recur thn) (recur els))]
    [else #f]))

;; if this need?
(define (decidable? instr)
  (match instr
    [`(has-type ,e^ ,t) (decidable? e^)]
    [`(read) #f]
    [else #t]))

(define (partial-helper instr env curdepth)
  (display "curdepth: ") (displayln curdepth)
  (display "instr: ") (displayln instr)
  (define recur (lambda (x) (partial-helper x env (add1 curdepth))))
  (if (> curdepth maxdepth)
      (list instr #f)
      (match instr
        [(? fixnum?) (list instr #f)]
        [(? symbol?) (list (lookup instr env instr) #f)]
        [`(has-type ,e^ ,t)
         (match-define `(,eval-e ,changed) (recur e^))
         (list `(has-type ,eval-e ,t) changed)]

        ;; define
        [`(define (,funame . ,var-defs) : ,ret-type ,body)
         (match-define `(,eval-e ,changed) (recur body))
         (list `(define (,funame . ,var-defs) : ,ret-type ,eval-e) changed)]
        
        ;; simple add
        [`(+ (has-type ,e1^ Integer) (has-type ,e2^ Integer))
         (list `(has-type ,(+ e1^ e2^) Integer) #t)]
        [`(+ ,e1^ ,e2^)
         (match-define `(,e1 ,changed1) (recur e1^))
         (match-define `(,e2 ,changed2) (recur e2^))
         (list `(+ ,e1 ,e2) (or changed1 changed2))]

    ;;; if condition
        [`(if #f ,thn ,els) (list els #t)]
        [`(if #t ,thn ,els) (list thn #t)]
        
    ;;; let parameter estimate

        [`(let ([,var ,para]) ,body)
         (if (var-occurs? var body)
             (let* ([eval-var (recur para)]
                    [new-env (cons `(var . eval-var) env)])
               (partial-helper body new-env (add1 curdepth)))
             (let ([result (recur body)])
               (list (car result) #t)))]
        )))


(define (partial-with-level prog level)
  (match-define `(program ,ret-type . ,exprs) prog)
  ;; (define defs (drop-right exprs 1))
  (define partial-result (map (lambda (x)
                                (partial-helper x '() 1)) exprs))
  (display "partial result: ") (displayln partial-result)
  `(program ,ret-type ,@(map car partial-result)))


(define (partial prog)
  (partial-with-level prog 5))
