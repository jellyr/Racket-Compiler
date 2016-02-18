#lang racket
(require "interp.rkt")
(require "compiler.rkt")

(provide test)

;;;; some helpful dev code
;;;; see: https://piazza.com/class/ij8uxz86huctk?cid=46

(define (test passes exp)
  (display "--------------------------") (newline)
  (pretty-print exp)
  (display "--------------------------") (newline)
  (let loop ([ls passes] [prog `(program ,exp)])
    (if (null? ls) (begin (display "done") (newline))
        (match (car ls)
          (`(,name ,func ,_)
           (display (string-append name ":")) (newline)
           (define new-prog (func prog))
           (if (string=? name "print-x86")
               (begin (display new-prog) (newline))
               (begin (pretty-print new-prog) (newline))) 
           (loop (cdr ls) new-prog))))))

(define expr '(let ([a 1])
  (let ([b 1])
    (let ([c 1])
      (let ([d 1])
        (let ([e 1])
          (let ([f 1])
            (let ([g 1])
              (let ([h 1])
                (let ([i 1])
                  (let ([j 1])
                    (let ([k 1])
                      (let ([l 1])
                        (let ([m 1])
                          (let ([n 1])
                            (let ([o 1])
                              (let ([p 1])
                                (let ([q 1])
                                  (let ([r 1])
                                    (let ([s 1])
                                      (let ([t 1])
                                        (let ([u 1])
                                          (let ([z
                                                 (let ([x 1])
                                                   (let ([y 9])
                                                     (let ([z (+ y y)])
                                                       (let ([w (+ 2 z)])
                                                         (+ w x)))))])
                                            
                                          (+ a 
                                          (+ b 
                                          (+ c 
                                          (+ d 
                                          (+ e 
                                          (+ f 
                                          (+ g 
                                          (+ h 
                                          (+ i 
                                          (+ j 
                                          (+ k 
                                          (+ l 
                                          (+ m 
                                          (+ n 
                                          (+ o
                                          (+ p 
                                          (+ q 
                                          (+ r 
                                          (+ s 
                                          (+ t 
                                          (+ u z)))))))))))))))))))))))))))))))))))))))))))
      

)

(test `(("typecher1" ,typechecker ,interp-scheme) ,@r3-passes) expr)
