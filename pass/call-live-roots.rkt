#lang racket
(require "../common.rkt")

(provide call-live-roots)

(define (live-if-helper e)
  ;(display "in iffffffff:") (displayln e)
  (foldr (lambda (x r)
           (let* ([lives (if (null? r) (set) (car r))]
                  [newlives (live-analysis x lives)])
             (cons newlives r)))
         '() e))

(define (live-analysis instr lak)
  (define (vector-unwrap var) (set var))
  ;(display "lak: ") (displayln lak)
  ;(display "instr: ")(displayln instr)
  (match instr
    
    [`(has-type (allocate ,e ,t1) ,t) (set)]
    [`(assign ,var ,e) (let ([forsub (vector-unwrap var)]
                             [forunion (live-analysis e (set))])
                         ;(display "forunion:")(displayln forunion)
                         ;(display "forsub")(displayln forsub)
                         (set-subtract (set-union forunion lak) forsub))]
    [`(has-type (vector-set! ,var ,index ,e) ,t)
     ;(display "instr: ") (displayln instr)
     (let ([forunione (live-analysis e lak)]
           [forunionvar (live-analysis var lak)])
       (set-union lak forunione forunionvar))]
    [`(has-type (vector-ref (has-type ,v ,vt^) ,index) ,t) (set-union lak (set v))]
    [`(has-type (if (has-type (eq? ,e1 ,e2) Boolean) ,thn ,els) ,t)  (let ([e1set (live-analysis e1 lak)]
                                                        [e2set (live-analysis e2 lak)]
                                                        [thenset (live-if-helper thn)]
                                                        [elseset (live-if-helper els)])
                                                    ; (display "thenset: ") (displayln thenset)
                                                                       (set-union e1set e2set (car thenset) (car elseset)))]
    [`(has-type (app . ,e1) ,t)  (foldr set-union (set) (map (lambda (x^) (live-analysis x^ lak)) e1)) ]
    [`(has-type ,x ,t) #:when (and (and (pair? t) (equal? (car t) 'Vector)) (not (equal? (cadr t) '_))) (set x)]
    [`(return ,var) (set)]
    [else lak]))


(define (call-live-roots e)
  (define (live-instr-helper instr livea)
    ; (display "instr: ") (displayln instr)
      ;(display "livea: ") (displayln livea)
    (match instr
      [`(has-type (if (collection-needed? ,e1)
             ((collect ,e2))
             ()) ,t)
       ;(display "e2: ") (displayln e2)
        ;(display "livea: ") (displayln (set->list livea))
       `(has-type (if (collection-needed? ,e1)
                      ((call-live-roots ,(set->list livea) (has-type (collect ,e2) Void)))
                      ()) ,t)]
      [`(has-type (if (has-type (eq? ,e1 ,e2) ,t1) ,thn ,els) ,t)  (let ([texpr (map (curryr live-instr-helper livea) thn)]
                                                                        [eexpr (map (curryr live-instr-helper livea) els)])
                                                                    `(has-type (if (eq? ,e1 ,e2) ,texpr ,eexpr) ,t))]
       ;;Need to change to make this work in select-instrs
      [`(assign ,var (has-type (app . ,e1) ,t))
       ;; (display "livea: ")(displayln (set->list livea))
       `(assign ,var (call-live-roots ,(set->list (set-remove livea var)) (has-type (app . ,e1) ,t)))]
      [`(define (,fname . ,params) : ,ret ,lvars . ,body)
       `(define (,fname . ,params) : ,ret ,lvars . ,(map live-instr-helper body (cdr livea)))]
      [else instr]))
  (match-define `(program ,tvars ,ret-type (defines . ,defs) . ,body) e)
  (let* ([calc-live (lambda (instr res)
                         (define value (live-analysis instr (car res)))
                         (cons value res))]
         [live-main (foldr calc-live `(,(set)) body)]
         [live-defs (map (lambda (def)
                           (match-define `(define (,fname . ,params) : ,ret ,lvars . ,def-body) def)
                           (foldr calc-live `(,(set)) def-body)) defs)])
    ;(display "live-main:")(displayln live-main)
    
    `(program ,tvars ,ret-type
              (defines ,@(map live-instr-helper defs live-defs))
              ,@(map live-instr-helper body (cdr live-main)))))
