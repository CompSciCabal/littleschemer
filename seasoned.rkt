(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      ((null? (cdr lat)) #f)
      (else (or (eq? (car lat) (car (cdr lat)))
                (two-in-a-row? (cdr lat)))))))

(define two-in-a-row-b?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
             (two-in-a-row-b? lat))))))

(define two-in-a-row-c?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
             (two-in-a-row-c? (car lat) (cdr lat)))))))

(define two-in-a-row-d?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-c? (car lat) (cdr lat))))))

(define sum-of-prefixes
  (lambda (tup)
    (cond
     ((null? tup) '())
     (else (cons (car tup) (sop-b (car tup) (cdr tup)))))))

(define sop
  (lambda (tup)
    (sop-b 0 tup)))

(define sop-b
  (lambda (sum tup)
    (cond
      ((null? tup) '())
      (else (cons (+ sum (car tup)) (sop-b (+ sum (car tup)) (cdr tup)))))))
