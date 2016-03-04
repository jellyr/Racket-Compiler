#lang racket
(require "../common.rkt")
(provide flattens)

(define (if-flatten cnd thn els)
  (match cnd
    [`(not ,cexp) (let-values (((cn tn el op) (if-flatten cexp els thn)))
                    (values cn tn el #f))]
    [`(eq? ,e1 ,e2) #:when (and (scalar? e1) (scalar? e2)) (values cnd thn els #t)]
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
  (match e
    [(or (? scalar?) (? vector?)) (values e '() '())]
    [`(read) (let [(newvar (gensym))]
               (values newvar  `((assign ,newvar (read))) `(,newvar)))]
    ;; be careful here 
    [`(program ,type . ,e) (let-values ([(e^ stmt^ alist^) (flattens (last e))])
                             `(program ,alist^ ,type ,(flatten-func (drop-right e 1))  ,@stmt^ (return ,e^)))]
    [`(define (,fname . ,params) : ,type ,body) (let-values ([(e^ stmt^ alist^) (flattens body)])
                                                  `(define (,fname . ,params) : ,type ,alist^ ,@(append stmt^ `((return ,e^)))))]
    [`(app . ,e1) (let-values ([(e1^ stmt1^ alist1^) (flatten-vec-app e1)])
                      (let ([newvar (gensym)])
                        (values newvar
                                (append stmt1^ `((assign ,newvar (app . ,e1^))))
                                (cons newvar alist1^))))]
    [`(function-ref ,e1) (let-values ([(e^ stmt^ alist^) (flattens e1)])
                          (let ([newvar (gensym)])
                            (values newvar
                                    (append stmt^ `((assign ,newvar (function-ref ,e^))))
                                    (cons newvar alist^))))]
    [`(vector . ,e1) (let-values ([(e^ stmt^ alist^) (flatten-vec-app e1)])
                       (let [(newvar (gensym))]
                         (values newvar
                                 (append stmt^ `((assign ,newvar (vector . ,e^))))
                                 (cons newvar alist^))))]
    [`(vector-set! ,e1 ,e2 ,e3) (let-values ([(e1^ stmt1^ alist1^) (flattens e1)]
                                             [(e2^ stmt2^ alist2^) (flattens e2)]
                                             [(e3^ stmt3^ alist3^) (flattens e3)]
                                             [(newvar) (gensym)])
                                  (values newvar
                                          (append stmt1^
                                                  stmt2^
                                                  stmt3^
                                                  `((assign ,newvar (vector-set! ,e1^ ,e2^ ,e3^))))
                                          (append (cons newvar alist1^) alist2^ alist3^)))]
    [`(if ,cn ,tn ,en) (let-values (((cnd thn els op) (if-flatten cn tn en)))
                         (let-values (((ec stmtc alistc) (flattens cnd))
                                      ((et stmtt alistt) (flattens thn))
                                      ((ee stmte aliste) (flattens els)))
                           (let ([newvar (gensym)])
                             (values newvar
                                     (if op
                                         (append `((if ,cnd
                                                       ,(append stmtt `((assign ,newvar ,et)))
                                                       ,(append stmte `((assign ,newvar ,ee))))))
                                         (append stmtc `((if (eq? #t ,ec)
                                                             ,(append stmtt `((assign ,newvar ,et)))
                                                             ,(append stmte `((assign ,newvar ,ee)))))))
                                     (if op
                                         (append (cons newvar alistt) aliste)
                                         (append (cons newvar alistc) alistt aliste))))))]
    [`(let ([,x ,e]) ,body) (let-values
                                ([(xe^ stmtx^ alistx^) (flattens e)]
                                 [(be^ stmtb^ alistb^) (flattens body)])
                              (match e
                                [`(if ,cnd ,thn ,els) (values be^
                                                              (append stmtx^ `((assign ,x ,xe^)) stmtb^)
                                                              (append alistx^ alistb^))]
                                [else (let* [(alistx^ (cons x (if (null? alistx^) alistx^ (remq xe^ alistx^))))
                                             (xe^ (if (null? stmtx^) xe^ (last (last stmtx^))))
                                             (stmtx^ (if (null? stmtx^) '() (take stmtx^ (sub1 (length stmtx^)))))]
                                        (values be^
                                                (append stmtx^ `((assign ,x ,xe^)) stmtb^)
                                                (append alistx^ alistb^)))]))]
    [`(and ,e1 ,e2) (flattens `(if (eq? ,e1 #t) ,e2 #f))]
    [`(,op ,e1 ,e2) (let-values (((e1^ stmt1^ alist1^) (flattens e1))
                               ((e2^ stmt2^ alist2^) (flattens e2)))
                    (let ([newvar (gensym)])
                      (values newvar
                              (append stmt1^ (append stmt2^ `((assign ,newvar (,op ,e1^ ,e2^)))))
                              (append (cons newvar alist1^) alist2^))))]
    [`(,op ,e1) (let-values ([(e^ statements^ alist) (flattens e1)])
                (let [(newvar (gensym))]
                  (values newvar
                          (append statements^ `((assign ,newvar (,op ,e^))))
                          (cons newvar alist))))]))
