#lang racket
(require "../common.rkt")

(provide build-interference)

(define (build-interference-unwrap e)
  (match e
    [`(var ,e1) e1]
    [`(reg ,r) r] ;; removed rax from interference graph
    ['(byte-reg al) 'rax]
    [`(offset ,e1 ,idx) (build-interference-unwrap e1)]
    [`(xorq (int 1) (var ,e1)) e1]
    [else e]))

(define (build-interference-helper graph e lak)
  (let ([lak (set->list lak)])
    (match e
      [`(movq (offset ,var1 ,index) ,var2)
       (let ([s (build-interference-unwrap var1)]
             [d (build-interference-unwrap var2)])
         (map (lambda (v) (add-edge graph d v)) lak))]
      [`(,op ,e1 ,e2)#:when (and (or (var? e2) (reg? e2)) (or (eq? op 'movq) (eq? op 'movzbq)))
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         ;(display "e:")(displayln e)
         ;(display "s:")(displayln s)
         ;(display "d:")(displayln d)
         ;(display "lak:")(displayln lak)
         (map (lambda (v) (cond
                            [(not (or (eqv? s v) (eqv? d v))) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(,op ,e1 ,e2)#:when (and (or (var? e2) (reg? e2)) (or (eq? op 'addq)
                                                              (eq? op 'cmpq)
                                                              (eq? op 'sarq)
                                                              (eq? op 'salq)
                                                              (eq? op 'orq)))
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (eqv? d v)) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(negq ,e2)#:when (or (var? e2) (reg? e2))
       (let ([d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (eqv? d v)) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(,op ,label)  #:when (or (eqv? 'callq op) (eqv? 'indirect-callq op))
       (map (lambda (v1)
              (map (lambda (v2)
                     (hash-set! graph v1 (set-add (hash-ref graph v1 (set)) v2)))
                   (set->list (set-remove caller-save 'r11)))) lak)]
      [`(leaq (function-ref ,e1) ,e2)
       (let ([d (build-interference-unwrap e2)])
         (map (lambda (v) (cond
                            [(not (eqv? d v)) (add-edge graph d v)]
                            [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [`(if (eq? ,e1 ,e2) ,thn ,thnlive ,els ,elslive)
       (let ([s (build-interference-unwrap e1)]
             [d (build-interference-unwrap e2)])
         (map (lambda (v1 v2)
                (build-interference-helper graph v1 v2)) thn thnlive)
         (map (lambda (v1 v2)
                (build-interference-helper graph v1 v2)) els elslive)
         (map (lambda (v)
                (cond
                  [(and (symbol? d) (not (eqv? d v))) (add-edge graph d v)]
                  ;[(and (symbol? s) (not (eqv? s v))) (add-edge graph s v)]
                  [else (hash-set! graph v (hash-ref graph v (set)))])) lak))]
      [else '()])))

(define (define-helper defs)
  (map (lambda (def)
         (match-define `(define (,fname) ,pcnt (,vars ,max-stack ,lak) . ,instrs) def)
         (let ([graph (make-graph '())])
           (map (curry build-interference-helper graph) instrs lak)
           `(define (,fname) ,pcnt (,vars ,max-stack ,graph) . ,instrs))) defs))

(define (build-interference e)
  (match-define `(program (,vars ,mstack ,lak) ,ret (defines . ,defs) . ,instr) e)
  (let ([graph (make-graph '())])
    (map (curry build-interference-helper graph) instr lak)
    `(program (,vars ,mstack ,graph) ,ret (defines . ,(define-helper defs))  . ,instr)))
