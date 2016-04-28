
(define-inline (app f x)
    (f x))

(app (lambda (x) x) 42)
