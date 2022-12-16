#lang racket

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat)) (member? a (cdr lat)))])))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [(null? (cdr lat)) #f]
      [else (or (eq? (car lat) (cadr lat)) (two-in-a-row? (cdr lat)))])))

(define two-in-a-row-b?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (is-first-b? (car lat) (cdr lat))])))

(define is-first-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a) (two-in-a-row-b? lat))])))

(define two-in-a-row-c?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a) (two-in-a-row-c? (car lat) (cdr lat)))])))

(define two-in-a-row-d?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-c? (car lat) (cdr lat))])))

(define sum-of-prefixes
  (lambda (tup)
    (cond
      [(null? tup) '()]
      [else (cons (car tup) (sop-b (car tup) (cdr tup)))])))

;(define sop (lambda (tup) (sop-b 0 tup)))
;
;(define sop-b
;  (lambda (sum tup)
;    (cond
;      [(null? tup) '()]
;      [else (cons (+ sum (car tup)) (sop-b (+ sum (car tup)) (cdr tup)))])))


(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      [(null? tup) (quote ())]
      [else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup) (cons (car tup) rev-pre)))])))

; (define scramble (lambda (tup) (scramble-b tup '())))

(define multirember
  (lambda (a lat)
    (letrec ([multirember-inner
              (lambda (lat)
                (cond
                  [(null? lat) (quote ())]
                  [(eq? a (car lat)) (multirember-inner (cdr lat))]
                  [else (cons (car lat) (multirember-inner (cdr lat)))]))])
      (multirember-inner lat))))

; made it to 12th commandment, page 22


(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) empty]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

; (define rember-eq? (rember-f test?))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (letrec ([multirember-inner
                (lambda (lat)
                  (cond
                    [(null? lat) (quote ())]
                    [(test? a (car lat)) (multirember-inner (cdr lat))]
                    [else (cons (car lat) (multirember-inner (cdr lat)))]))])
        (multirember-inner lat)))))


; ((multirember-f eq?) 5 '(1 5 3 4 5 6 8 7)) ; '(1 3 4 6 8 7)

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))])))

(define sop
  (letrec ([sop-b (lambda (sum tup)
                    (cond
                      [(null? tup) '()]
                      [else
                       (cons (+ sum (car tup))
                             (sop-b (+ sum (car tup)) (cdr tup)))]))])
    (lambda (tup) (sop-b 0 tup))))

; (sop '(1 2 3 4)) ; '(1 3 6 10)

(define sop-b
  (lambda (sum tup)
    (cond
      [(null? tup) '()]
      [else (cons (+ sum (car tup)) (sop-b (+ sum (car tup)) (cdr tup)))])))


(define scramble
  (lambda (tup)
    (letrec ([scramble-b
              (lambda (tup rev-pre)
                (cond
                  [(null? tup) (quote ())]
                  [else
                   (cons (pick (car tup) (cons (car tup) rev-pre))
                         (scramble-b (cdr tup) (cons (car tup) rev-pre)))]))])
      (scramble-b tup '()))))

; chapter 13

(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) empty]
      [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(define intersectall
  (lambda (lset)
    (call-with-current-continuation
     (lambda (hop)
       (letrec ([A (lambda (lset)
                     (cond
                       [(null? (car lset)) (hop empty)]
                       [(null? (cdr lset)) (car lset)]
                       [else (intersect (car lset) (A (cdr lset)))]))])
         (cond
           [(null? lset) (quote ())]
           [else (A lset)])))))) ; ; Empty result.

; (intersectall '((1 2 3) () (2 4 5) (4 2 7))) ; '()

; 14th commandment

(define rember
  (lambda (a lat)
    (letrec ([R (lambda (lat)
                  (cond
                    [(null? lat) '()]
                    [(eq? a (car lat)) (cdr lat)]
                    [else (cons (car lat) (R (cdr lat)))]))])
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (letrec ([R (lambda (lat tail)
                  (cond
                    [(null? lat) tail]
                    [(eq? a (car lat)) (R (cdr lat) (cdr lat))]
                    [else (R (cdr lat) tail)]))])
      (R lat lat))))

(define rember-upto-last-cc
  (lambda (a lat)
    (call-with-current-continuation
     (lambda (skip)
       (letrec ([R (lambda (lat)
                     (cond
                       [(null? lat) '()]
                       [(eq? a (car lat)) (skip (R (cdr lat)))]
                       [else (cons (car lat) (R (cdr lat)))]))])
         (R lat))))))

(define leftmost
  (lambda (l)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (car l)]
      [else
       (let ([carl (leftmost (car l))])
         (cond
           [(atom? carl) carl]
           [else (leftmost (cdr l))]))])))

(define depth*
  (lambda (l)
    (cond
      [(null? l) 1]
      [(atom? (car l)) (depth* (cdr l))]
      [else
       (let ([a (add1 (depth* (car l)))] [d (depth* (cdr l))])
         (if (> d a) d a))])))

; BLT comment on page 76

(define leftmost2
  (lambda (l) (call-with-current-continuation (lambda (skip) (lm l skip)))))

(define lm
  (lambda (l out)
    (cond
      [(null? l) '()]
      [(atom? (car l)) (out (car l))]
      [else
       (let ()
         (lm (car l) out)
         (lm (cdr l) out))])))

(define walk
  (lambda (l out)
    (or (null? l)
        (let ()
          (cond
            [(atom? (car l)) (out (car l))]
            [else (walk (car l) out)])
          (walk (cdr l) out)))))

(define rm
  (lambda (a l oh)
    (cond
      [(null? l) (oh (quote no))]
      [(atom? (car l))
       (if (eq? (car l) a) (cdr l) (cons (car l) (rm a (cdr l) oh)))]
      [else
       (let ([new-car (call-with-current-continuation
                       (lambda (oh) (rm a (car l) oh)))])
         (if (atom? new-car)
             (cons (car l) (rm a (cdr l) oh))
             (cons new-car (cdr l))))])))


(call-with-current-continuation
 (lambda (Say) (rm 'noodles '((food) more food) Say))) ; 'no

;;;
(define x 'skins)
(define gourmet (lambda (food) (cons food (cons x '()))))

(gourmet 'onion)
(set! x 'rings)
(gourmet 'onion)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x '()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake (cons food '()))))

(define omnivore
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define gobbler
  (let ([x 'minestrone])
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define counter
  (let ([x 0])
    (lambda ()
      (set! x (+ 1 x))
      x)))

(define notcounter
  (lambda ()
    (let ([x 0])
      (set! x (+ 1 x))
      x)))

(define chez-nous
  (lambda (food)
    (let ([z food])
      (set! food x)
      (set! x z))))

; Skordalia!

(define qq 0)
(define cntr
  (lambda ()
    (set! qq (+ 1 qq))
    qq))

(define argtest
  (lambda (x)
    (set! x 5)
    x))

(define xy ; (x + yi) * (a + bi)
  (let ([x 1] [y 0])
    (lambda (a b)
      (set! x (- (* x a) (* y b)))
      (set! y (+ (* y a) (* x b)))
      `(,x ,y))))

(define iterator
  (lambda (f init)
    (let ([x init])
      (lambda ()
        (set! x (f x))
        x))))

(define it
  (lambda (f x n)
    (cond
      [(= n 0) '()]
      [else (cons (f x) (it f (f x) (- n 1)))])))

(define unpair (lambda (f) (lambda (z) (f (car z) (cadr z)))))

; z -> z^2 + c

(define c* ; (x + yi) * (a + bi)
  (lambda (z1 z2)
    (let ([x (car z1)] [y (cadr z1)] [a (car z2)] [b (cadr z2)])
      (list (- (* x a) (* y b)) (+ (* y a) (* x b))))))

(define c+ ; (x + yi) + (a + bi)
  (lambda (z1 z2)
    (let ([x (car z1)] [y (cadr z1)] [a (car z2)] [b (cadr z2)])
      (list (+ x a) (+ y b)))))

(define range
  (lambda (start end step)
    (letrec ([R (lambda (x)
                  (cond
                    [(> x end) '()]
                    [else (cons x (R (+ x step)))]))])
      (R start))))

(define mandelbrot ; does it get larger than 2?
  (lambda (c)
    (letrec
        ([R (lambda (z n)
              (letrec ([z2 (c* z z)] [zc (c+ z2 c)] [x (car zc)] [y (cadr zc)])
                (cond
                  [(> (abs x) 2) 7]
                  [(> (abs y) 2) 1]
                  [(eq? n 0) 0]
                  [else (R zc (- n 1))])))])
      (R '(0 0) 100))))


(define plot
  (lambda (f xs ys)
    (map (lambda (y) (map f (map (lambda (x) (list x y)) xs))) ys)))

; (plot mandelbrot (range -1 1 0.2) (range -1 1 0.2))
; (plot mandelbrot (range -1.5 0.5 0.05) (range -1 1 0.05))

(define newtonsmethod 1)

;; Chapter 16

(define sweet-tooth (lambda (food) (cons food (cons 'cake '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))

(define deep
  (lambda (n)
    (cond
      [(zero? n) 'pizza]
      [else (cons (deep (sub1 n)) '())])))

(define deepRRRR
  (let ([Ns '()] [Rs '()])
    (lambda (n)
      (set! Ns (cons n Ns))
      (set! Rs
            (cons (cond
                    [(zero? n) 'pizza]
                    [else (cons (deepR (sub1 n)) '())])
                  Rs))
      Rs)))

(define deepR
  (let ([Ns '()] [Rs '()])
    (lambda (n)
      (let ([result (deep n)])
        (set! Ns (cons n Ns))
        (set! Rs (cons result Rs))
        result))))

(define find
  (lambda (n Ns Rs)
    (cond
      [(null? Ns) #f]
      [(eq? n (car Ns)) (car Rs)]
      [else (find n (cdr Ns) (cdr Rs))])))

; (define deepM
;   (let ([Ns '()] [Rs '()])
;     (lambda (n)
;       (let ([exists (find n Ns Rs)])
;         (if (false? exists)
;             (let ([result (deep n)])
;               (set! Ns (cons n Ns))
;               (set! Rs (cons result Rs))
;               result)
;             exists)))))

; (define length (lambda (lat) (if (null? lat) 0 (add1 (length (cdr lat))))))


(define L
  (lambda (length)
    (lambda (l)
      (cond
        [(null? l) 0]
        [else (add1 (length (cdr l)))]))))

(define Y!
  (lambda (L)
    (let ([h (lambda (l) 'banana)])
      (set! h (L (lambda (arg) (h arg))))
      h)))

(define length (Y! L))

(length '(1 2 5 6 8 3)) ; 6

; (define collatz
;   (let ([x1 0] [x2 0])
;     (let ([y1 (lambda (n)
;                 (display n)
;                 (display " ")
;                 (cond
;                   [(equal? n 1) 1]
;                   [(zero? (modulo n 2)) (x1 (/ n 2))]
;                   [else (x2 n)]))]
;           [y2 (lambda (n)
;                 (cond
;                   [(equal? n 1) 1]
;                   [(zero? (modulo n 2)) (x1 n)]
;                   [else (x2 (+ 1 (* 3 n)))]))])
;       (set! x1 y1)
;       (set! x2 y2))
;     x1))

(define collatz
  (let ([x1 0] [x2 0])
    (set! x1
          (lambda (n)
            (display n)
            (display " ")
            (cond
              [(equal? n 1) 1]
              [(zero? (modulo n 2)) (x1 (/ n 2))]
              [else (x2 n)])))
    (set! x2
          (lambda (n)
            (cond
              [(equal? n 1) 1]
              [(zero? (modulo n 2)) (x1 n)]
              [else (x2 (+ 1 (* 3 n)))])))
    x1))

(collatz 14)

; end of book page 123 (not pdf)

(define depthstar
  (lambda (l)
    (cond
      [(null? l) 0]
      [(atom? (car l)) (max 1 (depthstar (cdr l)))]
      [else (max (depthstar (cdr l)) (+ 1 (depthstar (car l))))])))

(define D
  (lambda (depth*)
    (lambda (l)
      (cond
        [(null? l) 1]
        [(atom? (car l)) (depth* (cdr l))]
        [else (max (depth* (cdr l)) (+ 1 (depth* (car l))))]))))

(define Y-bang (lambda (L) (letrec ([h (L (lambda (arg) (h arg)))]) h)))

(define biz
  (let ([x 0])
    (lambda (f)
      (set! x (add1 x))
      (lambda (a) (if (= a x) 0 (f a))))))


;(define cc (lambda (n) (call-with-current-continuation (lambda (jump) jump))))
;(define x (cc 5))
;(x 7)
;(define y (cc 5))
;(y y)
;((y y) (y y))
;(y print)
;(y print)

; Chapter 17

(deep 99) ;

(define xcounter 0)
(define set-counter 0)

(define consC
  (let ([N 0])
    (set! xcounter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define supercounter
  (lambda (f)
    (letrec ([S (lambda (n)
                  (if (zero? n)
                      (f n)
                      (let ()
                        (f n)
                        (S (sub1 n)))))])
      (S 1000)
      (xcounter))))

(define deepM
  (let ([Rs (quote ())] [Ns (quote ())])
    (lambda (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
            (let ([result (if (zero? n)
                              (quote pizza)
                              (consC (deepM (sub1 n)) (quote ())))])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(supercounter deepM)


; page 139

(define rember1*C
  (lambda (a l)
    (letrec ([R
              (lambda (l oh)
                (cond
                  [(null? l) (oh (quote no))]
                  [(atom? (car l))
                   (if (eq? (car l) a) (cdr l) (consC (car l) (R (cdr l) oh)))]
                  [else
                   (let ([new-car (call-with-current-continuation
                                   (lambda (oh) (R (car l) oh)))])
                     (if (atom? new-car)
                         (consC (car l) (R (cdr l) oh))
                         (consC new-car (cdr l))))]))])
      (let ([new-l (call-with-current-continuation (lambda (oh) (R l oh)))])
        (if (atom? new-l) l new-l)))))

(define eqlist?
  (lambda (a b)
    (cond
      [(null? a) (null? b)]
      [(null? b) #f]
      [(atom? (car a)) (if (eq? (car a) (car b)) (eqlist? (cdr a) (cdr b)) #f)]
      [(atom? (car b)) #f]
      [else (and (eqlist? (car a) (car b)) (eqlist? (cdr a) (cdr b)))])))

(define rember1*C2
  (lambda (a l)
    (letrec ([R (lambda (l)
                  (cond
                    [(null? l) (quote ())]
                    [(atom? (car l))
                     (if (eq? (car l) a) (cdr l) (consC (car l) (R (cdr l))))]
                    [else
                     (let ([av (R (car l))])
                       (if (eqlist? (car l) av)
                           (consC (car l) (R (cdr l)))
                           (consC av (cdr l))))]))])
      (R l))))

; (consC (consC 'food '()) (consC 'more (consC (consC 'food '()) '())))

(define add-at-end
  (lambda (l)
    (cond
      [(null? l) '(egg)]
      [else (cons (car l) (add-at-end (cdr l)))])))

;(define add-at-end-too
;  (lambda (l)
;    (letrec
;        ((A (lambda (ls)
;              (cond
;                ((null? (kdr ls))
;                 (set-kdr ls '(egg))))
;                (else (A (kdr ls)))))))
;      (A l)
;      l)))

;(define kons
;  (lambda (kar kdr)
;    (lambda (selector)
;      (selector kar kdr))))

(define kar (lambda (c) (c (lambda (a d) a))))

(define kdr (lambda (c) (c (lambda (a d) d))))

(define set-kdr (lambda (c x) ((c (lambda (s a d) s)) x)))

(define bons
  (lambda (kar)
    (let ([kdr '()])
      (lambda (selector) (selector (lambda (x) (set! kdr x)) kar kdr)))))

(define kons
  (lambda (a d)
    (let ([c (bons a)])
      (set-kdr c d)
      c)))

; page 147


(define kar2 (lambda (c) (c (lambda (s a d) a))))

(define kdr2 (lambda (c) (c (lambda (s a d) d))))

(define lots
  (lambda (m)
    (cond
      [(zero? m) '()]
      [else (kons 'egg (lots (sub1 m)))])))

; (define x (lots 4))
; (define y (kons 'foo (kons 'bar '())))

(define to-list
  (lambda (k)
    (cond
      [(null? k) '()]
      [else (cons (kar2 k) (to-list (kdr2 k)))])))

(define koncat!
  (lambda [c1 c2]
    (cond
      [(null? (kdr2 c1)) (set-kdr c1 c2)]
      [else (koncat! (kdr2 c1) c2)])))

; Chapter 19

(define toppings 0)

(define deepB
  (lambda (m)
    (cond
      [(zero? m)
       (call-with-current-continuation (lambda (jump)
                                         (set! toppings jump)
                                         'pizza))]
      [else (cons (deepB (sub1 m)) '())])))
; page 160


(display (cons 'artichoke (cons 'pineapple (deepB 4))))
(display 'hi)

(cons 'garlic (toppings 'icecream))

(deepB 4)
(cons (toppings 'cake) (toppings 'cake))
(cons (toppings 'cake) (toppings 'moz))

(define deep&co
  (lambda (m k)
    (cond
      [(zero? m) (k 'pizza)]
      [else (deep&co (sub1 m) (lambda (x) (k (cons x empty))))])))

(deep&co 6 (lambda (x) x))

(define deep&coB
  (lambda (m k)
    (cond
      [(zero? m)
       (let ()
         (set! toppings k)
         (k 'pizza))]
      [else (deep&coB (sub1 m) (lambda (x) (k (cons x empty))))])))

(define fill 0)
(define leave 0)
(define waddle
  (lambda (l)
    (cond
      [(null? l) empty]
      [(atom? (car l))
       (let ()
         (call-with-current-continuation (lambda (rest)
                                           (set! fill rest)
                                           (leave (car l))))
         (waddle (cdr l)))]
      [else
       (let ()
         (waddle (car l))
         (waddle (cdr l)))])))

(define start-it2
  (lambda (l)
    (cons
     (call-with-current-continuation (lambda (here)
                                       (set! leave here)
                                       (waddle l))) '(1))))

;page 174

(define get-first
  (lambda (l)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (waddle l)
       (leave '())))))

(define get-next
  (lambda (x)
    (call-with-current-continuation
     (lambda (here)
       (set! leave here)
       (fill)))))

(define lookup
  (lambda (table name)
    (table name)))

(define the-empty-table
  (lambda (name)
    (abort (cons 'no-answer (cons name '())))))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        ((eq? name2 name1) value)
        (else (table name2))))))

(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) 'define))
      (else #f))))

(define global-table the-empty-table)

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox
     (lookup table (name-of e))
     (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend
               (formals-of e)
               (box-all args)
               table)))))

(define expression-to-actionn
  (lambda (e)
    (cond
      ((define? e) (lambda (e table) 1)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e)
     e table)))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define *define
  (lambda (e)
    (set! global-table
          (extend
           (name-of e)
           (box
            (the-meaning
             (right-side-of e)))
           global-table))))

(define value
  (lambda (e)
    (call-with-current-continuation
     (lambda (the-end)
       (set! abort the-end)
       (cond
         ((define? e) (*define e))
         (else (the-meaning e)))))))

(define abort 0)

; 185

(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es))
       (meaning (car es) table))
      (else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond
      ((null? vals) '())
      (else (cond (box (car vals)) (box-all (cdr vals)))))))

(define multi-extend
  (lambda (names vals table)
    (cond
      ((null? names) table)
      ((null? vals)  table)
      (else (extend (car names) (car vals)
                    (multi-extend (cdr names) (cdr vals) table))))))

(define odd?
  (lambda (n)
    (cond
      ((eq? n 0) #f)
      ((eq? n 1) #t)
      (else (odd? (- n 2))))))

(define even?
  (lambda (n)
    (cond
      ((eq? n 0) #t)
      ((eq? n 1) #f)
      (else (even? (- n 2))))))

(define odd??
  (lambda (n)
    (cond
      ((eq? n 1) #t)
      ((eq? n 0) #f)
      (else (not (even?? (- n 1)))))))

(define even??
  (lambda (n)
    (cond
      ((eq? n 0) #t)
      (else (not (odd?? (- n 1)))))))

(define odd???
  (lambda (n)
    (cond
      ((eq? n 0) #f)
      (else (even??? (- n 1))))))

(define even???
  (lambda (n)
    (cond
      ((eq? n 0) #t)
      (else (odd??? (- n 1))))))

(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
       ((lambda (val)
          (cons val
                (evlis (cdr args) table)))
        (meaning (car args) table))))))

(define :car
  (lambda (args)
    (car (car args))))

(define a-prim
  (lambda (f)
    (lambda (args)
      (f (car args)))))

(define b-prim
  (lambda (f)
    (lambda (args)
      (f (car args) (cadr args)))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      ((eq? e (quote cons)) (b-prim cons))
      ((eq? e (quote car)) (a-prim car))
      ((eq? e (quote cdr)) (a-prim cdr))
      ((eq? e (quote eq?)) (b-prim eq?))
      ((eq? e (quote atom?)) (a-prim atom?))
      ((eq? e (quote null?)) (a-prim null?))
      ((eq? e (quote zero?)) (a-prim zero?))
      ((eq? e (quote add1)) (a-prim add1))
      ((eq? e (quote sub1)) (a-prim sub1))
      ((eq? e (quote number?)) (a-prim number?)))))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define *letcc
  (lambda (e table)
    (call-with-current-continuation
     (lambda (skip)
       (beglis (ccbody-of e)
               (extend
                (name-of e)
                (box (a-prim skip))
                table))))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote)
          *quote)
         ((eq? (car e) 'lambda)
          *lambda)
         ((eq? (car e) 'letcc)
          *letcc)
         ((eq? (car e) 'set!)
          *set)
         ((eq? (car e) 'cond)
          *cond)
         (else *application)))
      (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      (else *identifier))))

;; text-of: list ->
;; Page 200
(define text-of
  (lambda (x)
    (car (cdr x))))

;; formals-of: list ->
;; Page 200
(define formals-of
  (lambda (x)
    (car (cdr x))))

;; body-of: list ->
;; Page 200
(define body-of
  (lambda (x)
    (cdr (cdr x))))

;; ccbody-of: list ->
;; Page 200
(define ccbody-of
  (lambda (x)
    (cdr (cdr x))))

;; name-of: list ->
;; Page 200
(define name-of
  (lambda (x)
    (car (cdr x))))

;; right-side-of: list ->
;; Page 200
(define right-side-of
  (lambda (x)
    (cond
     ((null? (cdr (cdr x))) 0)
     (else (car (cdr (cdr x)))))))

;; cond-lines-of: list ->
;; Page 200
(define cond-lines-of
  (lambda (x)
    (cdr x)))

;; else?: [s-exp] -> boolean
;; Page 200
(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x 'else))
     (else #f))))

;; question-of: list -> [s-exp]
;; Page 200
(define question-of
  (lambda (x)
    (car x)))

;; answer-of: list -> [s-exp]
;; Page 200
(define answer-of
  (lambda (x)
    (car (cdr x))))

;; function-of: list -> [s-exp]}
;; Page 200
(define function-of
  (lambda (x)
    (car x)))

;; arguments-of: list -> [s-exp]
(define arguments-of
  (lambda (x)
    (cdr x)))

;; TODO: Write more tests;
;(test "evlis" (evlis (cons 2 (quote (3))) global-table) '(2 3))
;(test "value" (value 1) 1)
;(test "value" (value (add1 2)) 3)
;(test "value" (value ((lambda (x) (add1 x)) 10)) 11)
;(test "value" (value ((lambda (x) (add1 x)) 10)) 11)
;(test "value" (value (car (quote (1 2)))) 1)
;(test "value" (value (eq? (quote a) (quote a))) #t)

; above copied from some random website, maybe fix later?

(value '(define value
(lambda (e) (letcc the-end
(set! abort the-end) (cond
((define? e) (^define e)) (else (the-meaning e)))))))
