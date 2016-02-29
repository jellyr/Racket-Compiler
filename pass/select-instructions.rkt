#lang racket
(require "../common.rkt")

(provide select-instructions)



(define SI-VARS '()) ;;Global variable to hold all new variables in select instructions pass


(define (calc-pointer-mask v idx res)
  (match v
    [`(Vector ,e1) (+ res (* idx 1))]
    [else (+ res (* idx 0))]))

(define (select-instructions-assign ret-v e)
  (match e
    [(? fixnum?) `(int ,e)]
    ;[(? symbol?) #:when (eq? e ret-v) '(reg rax)]
    [(? boolean?) (if e '(int 1) '(int 0))]
    [(? symbol?) #:when (not (eq? e 'program)) `(var ,e)]
    [`(not ,e1) `(xorq (int 1) ,(select-instructions-assign ret-v e1))]
    [`(initialize ,rootlen ,heaplen) (let ([newvar1 (gensym 'rootstack)])
                                       (set! SI-VARS `(,newvar1))
                                       `((movq (int ,rootlen) (reg rdi))
                                         (movq (int ,heaplen) (reg rsi))
                                         (callq initialize)
                                         (movq (global-value rootstack_begin)
                                               ,(select-instructions-assign ret-v newvar1))))]
    [`(assign ,var (vector-ref ,v1 ,idx)) (let ([v1^ (select-instructions-assign ret-v v1)]
                                                [var^ (select-instructions-assign ret-v var)])
                                            `((movq (offset ,v1^ ,(* 8 (add1 idx))) ,var^)))]
    [`(assign ,var (vector-set! ,v1 ,idx ,arg)) (let ([v1^ (select-instructions-assign ret-v v1)]
                                                      [arg^ (select-instructions-assign ret-v arg)])
                                                  
                                                  `(
                                                    (movq ,arg^ (offset ,v1^ ,(* 8 (add1 idx))))
                                                    (movq (int 46) ,(select-instructions-assign ret-v var))
                                                    ))]
    
    [`(assign ,var (allocate ,len (Vector . ,type))) (let* ([var^ (select-instructions-assign ret-v var)]
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
                                                     [nv1 (select-instructions-assign ret-v newvar1)]
                                                     [nv2 (select-instructions-assign ret-v newvar2)])
                                                (set! SI-VARS (append `(,newvar2) SI-VARS))
                                                `(,@(map (lambda (v idx)
                                                           `(movq (var ,v) (offset ,nv1 ,(* 8 idx))))
                                                         la
                                                         nvals)
                                                  (movq ,nv1 ,nv2)
                                                  (addq ,(select-instructions-assign ret-v n) ,nv2)
                                                  (movq ,nv2 (reg rdi))
                                                  (movq (int ,bytes^) (reg rsi))
                                                  (callq collect)
                                                  ,@(map (lambda (v idx)
                                                           `(movq (offset ,nv1 ,(* 8 idx)) (var ,v)))
                                                         la
                                                         nvals)))]
    [`(if (collection-needed? ,bytes^) ,thn ,els) (let* ([thn^ (map (curry select-instructions-assign ret-v) thn)]
                                                         [els^ (map (curry select-instructions-assign ret-v) els)]
                                                         [newvar1 (gensym 'end-data)]
                                                         [newvar2 (gensym 'lt)]
                                                         [nv1 (select-instructions-assign ret-v newvar1)]
                                                         [nv2 (select-instructions-assign ret-v newvar2)])
                                                    (set! SI-VARS (append `(,newvar1 ,newvar2) SI-VARS))
                                                    `((movq (global-value free_ptr) ,nv1)
                                                      (addq (int ,bytes^) ,nv1)
                                                      (cmpq ,nv1 (global-value fromspace_end))
                                                      (setl (byte-reg al))
                                                      (movzbq (byte-reg al) ,nv2)
                                                      (if (eq? (int 0) ,nv2)
                                                          ,els^
                                                          ,(car thn^))))]
    [`(assign ,var (eq? ,e1 ,e2)) `((cmpq ,(select-instructions-assign ret-v e1)
                                          ,(select-instructions-assign ret-v e2))
                                    (sete (byte-reg al))
                                    (movzbq (byte-reg al) (var ,var)))]
    [`(if (eq? ,e1 ,e2) ,thn ,els) `((if (eq? ,(select-instructions-assign ret-v e1)
                                              ,(select-instructions-assign ret-v e2))
                                         ,(append-map (curry select-instructions-assign ret-v) thn)
                                         ,(append-map (curry select-instructions-assign ret-v) els)))]
    [`(assign ,var (read)) `((callq read_int) (movq (reg rax) (var ,var)))]
    [`(assign ,var (- ,e1)) `((movq ,(select-instructions-assign ret-v e1) (var ,var)) (negq (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e1) `((addq ,(select-instructions-assign ret-v e2) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2))#:when (eq? var e2) `((addq ,(select-instructions-assign ret-v e1) (var ,var)))]
    [`(assign ,var (+ ,e1 ,e2)) `((movq ,(select-instructions-assign ret-v e1) (var ,var))
                                  (addq ,(select-instructions-assign ret-v e2) (var ,var)))]
    [`(return ,e1) `((movq ,(select-instructions-assign ret-v e1) (reg rax)))]
    [`(assign ,var ,e1) `((movq ,(select-instructions-assign ret-v e1) (var ,var)))]
    ;[(? list?) (list (remove ret-v e))]
    [else `(,e)]))

(define (select-instructions e)
  (let* ([ret-var (last (last e))]
         [prog (car e)]
         [args (cadr e)]
         [ret-type (caddr e)]
         [instrs (cdddr e)]
         [si (append-map (curry select-instructions-assign ret-var) instrs)])
    `(,prog ,(append args SI-VARS) ,ret-type ,@si)))
