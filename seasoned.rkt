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
