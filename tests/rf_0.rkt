(define-inline (id x) x)
(let ([fun id])
  (fun 42))
