#lang racket
(require "../common.rkt")

(provide select-instructions)


(define arg-regs (vector->list arg-registers))
(define SI-VARS '()) ;;Global variable to hold all new variables in select instructions pass
(define si-rootstack '())
(define mstack 0)


(define (calc-pointer-mask v idx res)
  (match v
    [`(Vector ,e1) (+ res (* idx 1))]
    [else (+ res (* idx 0))]))

(define (si-func-helper defs)
  (map (lambda (def)
         (set! SI-VARS '())
         (match-define `(define (,fname . ,params) : ,ret ,vars . ,body) def)
         (let* ([pcnt (add1 (length params))]
                [max-stack (if (< pcnt 6) 0 (- pcnt 6))]
                [in-params (map car params)]
                [rs (gensym 'rootstack)]
		[void-set (set! SI-VARS `(,rs))]
                [void-set1 (set! si-rootstack (cons `(,fname . ,rs) si-rootstack))]
                [void-set2 (set! mstack (if (> max-stack mstack) max-stack mstack))]
                [func-si (append-map (curry select-instructions-assign fname) body)]
                [tvars (append (remove fname vars) SI-VARS)])
           `(define (,fname) ,pcnt (,tvars ,max-stack) ,@(map (lambda (var param)
                                                                  `(movq ,(if (member param arg-regs)                                                                            
                                                                              `(reg ,param)
                                                                              param)
                                                                         ,(select-instructions-assign fname var)))
                                                              `(,rs . ,in-params) (append (take arg-regs (if (< pcnt 6) pcnt 6))
                                                                                          (map (lambda (v) `(stack-arg ,(* 8 v))) (range max-stack)))) ,@func-si))) defs))

(define (select-instructions-assign func-ref e)
  (match e
    [(? fixnum?) `(int ,e)]
    ;[(? symbol?) #:when (eq? e ret-v) '(reg rax)]
    [(? boolean?) (if e '(int 1) '(int 0))]
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(not ,e1) `(xorq (int 1) ,(select-instructions-assign e1))]
    [`(assign ,var (function-ref ,e1)) `((leaq (function-ref ,e1) ,(select-instructions-assign func-ref var)))]
    [`(assign ,var (call-live-roots ,la (app ,fun . ,args))) (let ([len (length args)])
                                                               ;(print si-rootstack) (newline)
                                                               `((movq (var ,(lookup func-ref si-rootstack #f)) (reg rdi))
                                                                 ,@(if (< len 6)
                                                                      (map (lambda (arg reg)
                                                                             `(movq ,(select-instructions-assign func-ref arg) (reg ,reg)))
                                                                           args (take (cdr arg-regs) len))
                                                                      (append (map (lambda (arg reg)
                                                                                     `(movq ,(select-instructions-assign func-ref arg) (reg ,reg)))
                                                                                   (take args 5) (cdr arg-regs))
                                                                              (map (lambda (arg st)
                                                                                     `(movq ,(select-instructions-assign func-ref arg) (stack-arg ,(* 8 st))))
                                                                                   (drop args 5) (range (- len 5)))))
                                                                 (indirect-callq ,(select-instructions-assign func-ref fun))
                                                                 (movq (reg rax) ,(select-instructions-assign func-ref var))))]
    [`(initialize ,rootlen ,heaplen) (let ([newvar1 (gensym 'rootstack)])
                                       (set! SI-VARS `(,newvar1))
                                       (set! si-rootstack (cons `(_main . ,newvar1) si-rootstack))
                                       `((movq (int ,rootlen) (reg rdi))
                                         (movq (int ,heaplen) (reg rsi))
                                         (callq initialize)
                                         (movq (global-value rootstack_begin)
                                               ,(select-instructions-assign func-ref newvar1))))]
    [`(assign ,var (vector-ref ,v1 ,idx)) (let ([v1^ (select-instructions-assign func-ref v1)]
                                                [var^ (select-instructions-assign func-ref var)])
                                            `((movq (offset ,v1^ ,(* 8 (add1 idx))) ,var^)))]
    [`(assign ,var (vector-set! ,v1 ,idx ,arg)) (let ([v1^ (select-instructions-assign func-ref v1)]
                                                      [arg^ (select-instructions-assign func-ref arg)])                                                 
                                                  `((movq ,arg^ (offset ,v1^ ,(* 8 (add1 idx))))
                                                    (movq (int 46) ,(select-instructions-assign func-ref var))))]
    [`(assign ,var (allocate ,len (Vector . ,type))) (let* ([var^ (select-instructions-assign func-ref var)]
                                                            [ptrmask (foldr calc-pointer-mask
                                                                            0
                                                                            type
                                                                            (build-list len (curry expt 2)))]
                                                            [tag (bitwise-ior (arithmetic-shift ptrmask 7)
                                                                              (bitwise-ior (arithmetic-shift len 1) 1))])
                                                       `((movq (global-value free_ptr) ,var^)
                                                         (addq (int ,(* 8 (add1 len))) (global-value free_ptr))
                                                         (movq (int ,tag) (offset ,var^ 0))))]
    [`(call-live-roots ,la (collect ,bytes^)) (let* ([n (length la)]
                                                     [nvals (build-list n values)]
                                                     [newvar1 (last SI-VARS)]
                                                     [newvar2 (gensym 'rootstack)]
                                                     [nv1 (select-instructions-assign func-ref newvar1)]
                                                     [nv2 (select-instructions-assign func-ref newvar2)])
                                                (set! SI-VARS (append `(,newvar2) SI-VARS))
                                                `(,@(map (lambda (v idx)
                                                           `(movq (var ,v) (offset ,nv1 ,(* 8 idx))))
                                                         la
                                                         nvals)
                                                  (movq ,nv1 ,nv2)
                                                  (addq ,(select-instructions-assign func-ref n) ,nv2)
                                                  (movq ,nv2 (reg rdi))
                                                  (movq (int ,bytes^) (reg rsi))
                                                  (callq collect)
                                                  ,@(map (lambda (v idx)
                                                           `(movq (offset ,nv1 ,(* 8 idx)) (var ,v)))
                                                         la
                                                         nvals)))]
    [`(if (collection-needed? ,bytes^) ,thn ,els) (let* ([thn^ (map (curry select-instructions-assign func-ref) thn)]
                                                         [els^ (map (curry select-instructions-assign func-ref) els)]
                                                         [newvar1 (gensym 'end-data)]
                                                         [newvar2 (gensym 'lt)]
                                                         [nv1 (select-instructions-assign func-ref newvar1)]
                                                         [nv2 (select-instructions-assign func-ref newvar2)])
                                                    (set! SI-VARS (append `(,newvar1 ,newvar2) SI-VARS))
                                                    `((movq (global-value free_ptr) ,nv1)
                                                      (addq (int ,bytes^) ,nv1)
                                                      (cmpq ,nv1 (global-value fromspace_end))
                                                      (setl (byte-reg al))
                                                      (movzbq (byte-reg al) ,nv2)
                                                      (if (eq? (int 0) ,nv2)
                                                          ,els^
                                                          ,(car thn^))))]
    [`(assign ,var (eq? ,e1 ,e2)) `((cmpq ,(select-instructions-assign func-ref e1)
                                          ,(select-instructions-assign func-ref e2))
                                    (sete (byte-reg al))
                                    (movzbq (byte-reg al) (var ,var)))]
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((if (eq? ,(select-instructions-assign func-ref e1)
                                              ,(select-instructions-assign func-ref e2))
                                         ,(append-map (curry select-instructions-assign func-ref) thn)
                                         ,(append-map (curry select-instructions-assign func-ref) els)))]
    [`(assign ,var (read)) `((callq read_int) (movq (reg rax) (var ,var)))]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign func-ref e1) (var ,var)) (negq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) `((addq ,(select-instructions-assign func-ref e2) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) `((addq ,(select-instructions-assign func-ref e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) #:when (fixnum? e1)`((movq ,(select-instructions-assign func-ref e2) (var ,var))
                                                     (addq ,(select-instructions-assign func-ref e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign func-ref e1) (var ,var))
                                  (addq ,(select-instructions-assign func-ref e2) (var ,var)))]
    [`(return ,e1) `((movq ,(select-instructions-assign func-ref e1) (reg rax)))]
    [`(assign ,var ,e1) `((movq ,(select-instructions-assign func-ref e1) (var ,var)))]
    ;[(? list?) (list (remove ret-v e))]
    [else `(,e)]))

(define (clean-has-type e)
  (match e
    [`(has-type ,e^ ,t) (clean-has-type e^)]
    [(? pair?) (map clean-has-type e)]
    [else e]))

(define (select-instructions e)
  (define clean-e (clean-has-type e))
  (match-define `(program ,args ,ret-type (defines . ,defs) . ,instrs) clean-e)
  (let ([si (append-map (curry select-instructions-assign '_main) instrs)]
        [tvars SI-VARS]
        [si-defs (si-func-helper defs)])
    `(program (,(append args tvars) ,mstack) ,ret-type (defines ,@si-defs) ,@si)))
