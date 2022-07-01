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
      [else (or (eq? (car lat) (car (cdr lat))) (two-in-a-row? (cdr lat)))])))

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

(define sop (lambda (tup) (sop-b 0 tup)))

(define sop-b
  (lambda (sum tup)
    (cond
      [(null? tup) '()]
      [else (cons (+ sum (car tup)) (sop-b (+ sum (car tup)) (cdr tup)))])))


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

(define scramble (lambda (tup) (scramble-b tup '())))

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

(define rember-eq? (rember-f test?))

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


((multirember-f eq?) 5 '(1 5 3 4 5 6 8 7)) ; '(1 3 4 6 8 7)

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

(sop '(1 2 3 4)) ; '(1 3 6 10)

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

(intersectall '((1 2 3) () (2 4 5) (4 2 7))) ; '()

; 14th commandment
