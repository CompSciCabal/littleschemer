#lang racket

(define cc
  (lambda (n)
    (call-with-current-continuation
     (lambda (jump)
       jump))))

(define counter
    (let ((n 0))
      (lambda (m)
        (set! n (+ n m))
        n)))

; (define e (call-with-current-continuation
;      (lambda (jump)
;        jump)))

; (define x (cc 5))
; (x e)
; (e x)
; (x 7)

; (define q 1)
; (counter (let () (set! q (cc 1)) 5))