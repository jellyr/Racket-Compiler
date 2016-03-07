#lang racket
(require "../common.rkt")

(provide allocate-registers)

(define (highest-saturation graph ol)
  (foldr (lambda (v r)
           (if (and (> (set-count (cdr v)) (set-count (cdr r)))
                    (not (findf (lambda (val) (eq? (car v) val)) ol)))
               v
               r)) (cons 'none (set)) (hash->list graph)))

(define (assign-minicolor node graph assign-list constrain-graph phash)
  (define colorvals (range 100))
  (define contrains (hash-ref! constrain-graph (car node) (set)))
  (if (set-member? (hash-ref graph (car node) (set)) 'rcx)
      (set! contrains (set-union contrains (list->set (range 1 9))))
      '()) 
  (define preferlist (dropf (map (lambda (v) (lookup v assign-list #f))
                                 (set->list (set-subtract (hash-ref! phash (car node) (set))
                                                          (hash-ref graph (car node) (set)))))
                            false?))
  (if (null? preferlist)
      (car (dropf colorvals (curry set-member? contrains)))
      (car preferlist)))

(define (allocate-registers-helper graph assign-list constrain-graph phash)
  (let* ([node (highest-saturation graph (map car assign-list))]
         [minvalue (assign-minicolor node graph assign-list constrain-graph phash)])
    (cond
      ((eq? 'none (car node)) assign-list)
      (else (allocate-registers-helper graph
                                       `((,(car node) . ,minvalue) . ,assign-list)
                                       (foldl (lambda (gr res)
                                                (hash-set! res gr (set-add (hash-ref! res gr (set)) minvalue))
                                                res) constrain-graph (set->list (cdr node)))
                                       phash)))))

(define (allocate-reg-stack assign-list)
  (define general-registers (vector 'rbx 'rcx 'rdx 'rsi 'rdi
                      'r8 'r9 'r10 'r12 
                  'r13 'r14 'r15))
  (define k (vector-length general-registers)) ;; (vector-length general-registers)
  (let ([reglist (filter (lambda (v) (< (cdr v) k)) assign-list)]
        [stacklist (filter (lambda (v) (>= (cdr v) k)) assign-list)])
    (cons `(_stacklength . ,(set-count (list->set (map cdr stacklist)))) ;; a hack way
          (append (map (lambda (v) `(,(car v) . (stack ,(* -8 (add1 (- (cdr v) k)))))) stacklist)
                  (map (lambda (v) `(,(car v) . (reg ,(vector-ref general-registers (cdr v))))) reglist)))))

(define (allocate-var e env)
  (match e
    ;;; consider this situation again, testcase: (let ([x 41]) (+ x 1)) 
    [`(var ,e1) (lookup e1 env '(reg rax))]
    [`(,op ,e1 ,e2) `(,op ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [`(,op ,e1) `(,op ,(allocate-var e1 env))]
    [`(if (eq? ,e1 ,e2) ,thn ,thnlive ,els ,elslive) `(if (eq? ,(allocate-var e1 env) ,(allocate-var e2 env))
                                           ,(map (lambda (v)
                                                   (allocate-var v  env)) thn)
                                           ,(map (lambda (v)
                                                   (allocate-var v  env)) els))]
    ;[`(addq ,e1 ,e2) `(addq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    ;[`(subq ,e1 ,e2) `(subq ,(allocate-var e1 env) ,(allocate-var e2 env))]
    [else e]))

(define (allocate-prefer insts)
  (define phash (make-hash))
  (map (lambda (inst)
         (match inst
           [`(movq (var ,e1) (var ,e2))
            (hash-set! phash e1 (set-add (hash-ref phash e1 (set)) e2))]
           ;; consider if condition
           [else '()])) insts) phash)

(define (allocate-func-registers def)
  (match-define `(define (,fname) ,pcnt (,vars ,max-stack ,graph) . ,instrs) def)
  (let* ([phash (allocate-prefer instrs)]
         ;;; (hash-remove (cadadr e) 'rax)
         [assign-list (allocate-registers-helper graph '() (make-graph '()) phash)]
         [env (allocate-reg-stack assign-list)])
    ;(print assign-list)
    `(define (,fname) ,(lookup '_stacklength env) . ,(map (curryr allocate-var env) instrs))))

(define (allocate-registers e)
  (match-define `(program (,vars ,mstack ,graph) ,ret (defines . ,defs) . ,instrs) e)
  (let* ([phash (allocate-prefer instrs)]
         ;;; (hash-remove (cadadr e) 'rax)
         [assign-list (allocate-registers-helper graph '() (make-graph '()) (make-hash) ;phash
                                                 )]
         [env (allocate-reg-stack assign-list)])
    ;(print assign-list)
    `(program ,(lookup '_stacklength env) ,ret (defines . ,(map allocate-func-registers defs)) . ,(map (curryr allocate-var env) instrs))))









