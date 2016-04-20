#lang racket
(require "../common.rkt")
(provide flattens)

(define env '())

(define (envend v)
  (set! env (cons v env)))

(define (applyenv e)
  (lookup e env #f))

(define (hast-env e)
  ; (displayln env)
  (let ([t (applyenv e)])
    (if t
        `(has-type ,e ,t)
        e)))

(define (if-flatten cnd thn els)
  (match cnd
    [`(has-type (not ,cexp) ,t) (let-values (((cn tn el op) (if-flatten cexp els thn)))
                      (values cn tn el #f))]
    [`(has-type (eq? ,e1 ,e2) ,t) #:when (and (scalar? e1) (scalar? e2)) (values cnd thn els #t)]
    [else (values cnd thn els #f)]))

(define (flatten-vec-app expr)
  (let [(flat-vec-app   (foldr (lambda (exp res)
                                (let-values ([(e^ stmt^ alist^) (flattens exp)])
                                  `(,(append `(,e^) (car res))
                                    ,(append stmt^ (cadr res))
                                    ,(append alist^ (caddr res))))) '(() () ())  expr))]
      (values (car flat-vec-app)
              (cadr flat-vec-app)
              (caddr flat-vec-app))))

(define (flatten-func expr)
  `(defines ,@(map flattens expr)))

(define (flattens e)
  ;(display "e: ")(displayln e)
  ;(display "env: ")(displayln env)
  (match e
    [`(has-type ,expr ,ty)#:when (or (scalar? expr) (vector? expr))
     (envend `(,expr . ,ty))
     (values expr `() '())]
    [(or (? scalar?) (? vector?)) (values e '() '())]
    
    [`(has-type (inject ,expr ,ty) Any)
     (match-define `(has-type ,has-expr ,has-T) expr)
     (define newvar (gensym))
     (envend `(,newvar . ,ty))
     
     ;(display "e: ")(displayln e)       
     (if (or (eq? has-T 'Integer) (eq? has-T 'Boolean))
         (values newvar
                 `((assign ,newvar (has-type (inject ,expr ,ty) Any)))
                 `(,newvar))         
         (let-values ([(e^ stmt^ alist^) (flattens expr)])
         
           (envend `(,e^ . ,ty))
           (values newvar
                   (append stmt^ `((assign ,newvar (has-type (inject ,e^ ,ty) Any))))
                   (cons newvar alist^))))]
    [`(has-type (project ,expr ,ty) ,ty)
     (match-define `(has-type ,has-expr ,has-T) expr)
     (define newvar (gensym))
     ;(set! ty (hast-env expr))
     
     ;(display "e: ")(displayln e)
     (envend `(,newvar . ,ty))
     (if (or (eq? has-T 'Integer) (eq? has-T 'Boolean))
         (values newvar
                 `((assign ,newvar (has-type (project ,expr ,ty) ,ty)))
                 `(,newvar))         
         (let-values ([(e^ stmt^ alist^) (flattens expr)])
           ;; (display "e^: ")(displayln e^)
           ;; (display "ty: ")(displayln ty)
           ;; (display "env: ")(displayln env)
           
           (values newvar
                   (append stmt^ `((assign ,newvar (has-type (project ,e^ ,(lookup e^ (cdr env) ty)) ,ty))))
                   (cons newvar alist^))))
     ]
    
    [`(has-type ) (void)]
    [`(has-type (read) ,t) (let ([newvar (gensym)])
                             (envend `(,newvar . ,t))
                             (values newvar  `((assign ,newvar ,e)) `(,newvar)))]
    ;; be careful here 
    [`(program ,type . ,e) (let-values ([(e^ stmt^ alist^) (flattens (last e))])
                             `(program ,alist^ ,type ,(flatten-func (drop-right e 1))  ,@stmt^ (return (has-type (inject ,e^ ,(lookup e^ env)) Any))))]
    [`(define (,fname . ,params) : ,type ,body) (let-values ([(e^ stmt^ alist^) (flattens body)])
                                                  `(define (,fname . ,params) : ,type ,alist^ ,@(append stmt^ `((return ,e^)))))]
    [`(has-type (app . ,e1) ,t)
     (let-values ([(e1^ stmt1^ alist1^) (flatten-vec-app e1)])
       ;(display "app -> e1^: ") (displayln e1^)
       (let ([newvar (gensym)])
         (envend `(,newvar . ,t))
         (values newvar
                 (append stmt1^ `((assign ,newvar (has-type (app . ,(map hast-env e1^)) ,t))))
                 (cons newvar alist1^))))]
    [`(has-type (function-ref ,e1) ,t) (let-values ([(e^ stmt^ alist^) (flattens e1)])
                                         (let ([newvar (gensym)])
                                          
                                           (envend `(,e^ . ,t))
                                           (envend `(,newvar . ,t))
                                           (values newvar
                                                   (append stmt^ `((assign ,newvar (has-type (function-ref ,e^) (Vector _)))))
                                                   (cons newvar alist^))))]
    [`(has-type (vector . ,e1) ,vt)
     (let-values ([(e^ stmt^ alist^) (flatten-vec-app e1)])
       (let [(newvar (gensym))]
         (envend `(,newvar . ,vt))
         (values newvar
                 (append stmt^ `((assign ,newvar (has-type (vector . ,(map hast-env e^)) ,vt))))
                 (cons newvar alist^))))]
    [`(has-type (vector-ref ,e1 (has-type ,e2 Integer)) ,t)
     (let-values ([(e1^ stmt1^ alist1^) (flattens e1)]
                  [(newvar) (gensym)])      
       (envend `(,newvar . ,t))
       (values newvar
               (append stmt1^
                       `((assign ,newvar (has-type (vector-ref ,(hast-env e1^) (has-type ,e2 Integer)) ,t))))
               (cons newvar alist1^)))]
    [`(has-type (vector-ref ,e1 ,e2) ,t)
     (let-values ([(e1^ stmt1^ alist1^) (flattens e1)]
                  [(e2^ stmt2^ alist2^) (flattens e2)]
                  [(newvar) (gensym)])

       (envend `(,newvar . ,t))
       (values newvar
               (append stmt1^
                       `((assign ,newvar (has-type (vector-ref ,(hast-env e1^) ,(hast-env e2^) ) ,t))))
               (cons newvar (append alist1^ alist2^))))]
    [`(has-type (vector-set! ,e1 ,e2 ,e3) ,t) (let-values ([(e1^ stmt1^ alist1^) (flattens e1)]
                                               [(e2^ stmt2^ alist2^) (flattens e2)]
                                               [(e3^ stmt3^ alist3^) (flattens e3)]
                                               [(newvar) (gensym)])
                                                (envend `(,newvar . ,t))
                                                (values newvar
                                                        (append stmt1^
                                                                stmt2^
                                                                stmt3^
                                                                `((assign ,newvar (has-type (vector-set! ,(hast-env e1^) ,(hast-env e2^) ,(hast-env e3^)) ,t))))
                                                        (append (cons newvar alist1^) alist2^ alist3^)))]
    [`(has-type (if ,cn ,tn ,en) ,t) (let-values (((cnd thn els op) (if-flatten cn tn en)))
                                       (let-values (((ec stmtc alistc) (flattens cnd))
                                                    ((et stmtt alistt) (flattens thn))
                                                    ((ee stmte aliste) (flattens els)))
                                         (let ([newvar (gensym)])                                    
                                           (values newvar
                                                   (if op
                                                       (append `((has-type (if ,cnd
                                                                      ,(append stmtt `((assign ,newvar ,(hast-env et))))
                                                                      ,(append stmte `((assign ,newvar ,(hast-env ee)))))
                                                                ,(last tn))))
                                                       (append stmtc `((has-type
                                                                        (if (has-type (eq? (has-type #t Boolean) ,(hast-env ec)) Boolean)
                                                                            ,(append stmtt `((assign ,newvar ,(hast-env et))))
                                                                            ,(append stmte `((assign ,newvar ,(hast-env ee)))))
                                                                        ,(last tn)))))
                                                   (if op
                                                       (append (cons newvar alistt) aliste)
                                                       (append (cons newvar alistc) alistt aliste))))))]
    [`(has-type (let ([,x ,e]) ,body) ,t)
     ;(envend `(,x . ,vartype))
     (let-values
         ([(xe^ stmtx^ alistx^) (flattens e)]
          [(be^ stmtb^ alistb^) (flattens body)])
       (match e
         [`(has-type (if ,cnd ,thn ,els) ,t1) (values be^
                                                      (append stmtx^
                                                              `((assign ,x ,(hast-env xe^)))
                                                              stmtb^)
                                                      (append alistx^ alistb^))]
         [else (let* [(alistx^ (cons x (if (null? alistx^) alistx^ (remq xe^ alistx^))))
                      (xe^ (if (null? stmtx^) xe^ (last (last stmtx^))))
                      (stmtx^ (if (null? stmtx^) '() (take stmtx^ (sub1 (length stmtx^)))))]
                 ;; (display "env: ")(displayln env)
                 ;; (display "x: ")(displayln x)
                 ;; (display "xe^: ")(displayln xe^)
                 (values be^
                         (append stmtx^ `((assign ,x ,(hast-env xe^))) stmtb^)
                         (append alistx^ alistb^)))]))]
    [`(has-type (and ,e1 ,e2) ,t) (flattens `(has-type (if (has-type (eq? ,e1 #t) Boolean) ,e2 (has-type #f Boolean)) ,t))]
    [`(has-type (,op ,e1 ,e2) ,t) (let-values
                                      (((e1^ stmt1^ alist1^) (flattens e1))
                                       ((e2^ stmt2^ alist2^) (flattens e2)))
                                    (let ([newvar (gensym)])
                                      (envend `(,newvar . ,t))
                                      (values newvar
                                              (append stmt1^
                                                      (append stmt2^
                                                              `((assign ,newvar (has-type (,op ,(hast-env e1^) ,(hast-env e2^)) ,t)))))
                                              (append (cons newvar alist1^) alist2^))))]
    [`(has-type (,op ,e1) ,t) (let-values ([(e^ statements^ alist) (flattens e1)])
                                (let [(newvar (gensym))]
                                  (envend `(,newvar . ,t))
                                  (values newvar
                                          (append statements^ `((assign ,newvar (has-type (,op ,(hast-env e^)) ,t))))
                                          (cons newvar alist))))]))
