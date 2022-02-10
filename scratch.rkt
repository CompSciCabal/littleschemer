#lang racket

; Chapter 1

(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

; Chapter 2

(define lat? (lambda (l) (or (null? l) (and (atom? (car l)) (lat? (cdr l))))))

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a) (member? a (cdr lat)))])))

; Chapter 3

(define firsts
  (lambda (l)
    (cond
      [(null? l) empty]
      [else (cons (caar l) (firsts (cdr l)))])))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      [(null? lat) empty]
      [(or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat))]
      [else (cons (car lat) (subst2 new o1 o2 (cdr lat)))])))

(define multirember
  (lambda (a lat)
    (cond
      [(null? lat) empty]
      [(eq? (car lat) a) (multirember a (cdr lat))]
      [else (cons (car lat) (multirember a (cdr lat)))])))

(define multiinsertR
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [(eq? (car lat) old)
       (cons (car lat) (cons new (multiinsertR new old (cdr lat))))]
      [else (cons (car lat) (multiinsertR new old (cdr lat)))])))

(define multiinsertL
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [(eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr lat))))]
      [else (cons (car lat) (multiinsertL new old (cdr lat)))])))

(define multisubst
  (lambda (new old lat)
    (cond
      [(null? lat) empty]
      [(eq? (car lat) old) (cons new (multisubst new old (cdr lat)))]
      [else (cons (car lat) (multisubst new old (cdr lat)))])))

; Chapter 4
(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

(define add (lambda (a b) (if (zero? b) a (add (add1 a) (sub1 b)))))

(define sub (lambda (a b) (if (zero? b) a (sub1 (sub a (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
      [(null? tup) 0]
      [else (add (car tup) (addtup (cdr tup)))])))

(define mult (lambda (a b) (if (zero? b) 0 (+ (mult a (sub1 b)) a))))

(define tup+
  (lambda (tup1 tup2)
    (if (and (null? tup1) (null? tup2))
        empty
        (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2))))))

(define >
  (lambda (a b)
    (cond
      [(zero? a) #f]
      [(zero? b) #t]
      [else (> (sub1 a) (sub1 b))])))

(define <
  (lambda (a b)
    (cond
      [(zero? b) #f]
      [(zero? a) #t]
      [else (< (sub1 a) (sub1 b))])))

(define =
  (lambda (a b)
    (cond
      [(zero? b) (zero? a)]
      [(zero? a) #f]
      [else (= (sub1 a) (sub1 b))])))

(define exp (lambda (a b) (if (zero? b) 1 (mult (exp a (sub1 b)) a))))

(define div
  (lambda (a b)
    (cond
      [(< a b) 0]
      [else (add1 (div (sub a b) b))])))

(define length (lambda (lat) (if (null? lat) 0 (add1 (length (cdr lat))))))

(define pick
  (lambda (n lat)
    (cond
      [(zero? (sub1 n)) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat) empty]
      [(number? (car lat)) (no-nums (cdr lat))]
      [else (cons (car lat) (no-nums (cdr lat)))])))

(define all-nums
  (lambda (lat)
    (cond
      [(null? lat) empty]
      [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
      [else (all-nums (cdr lat))])))

(define eqan?
  (lambda (a1 a2)
    (cond
      [(and (number? a1) (number? a2)) (= a1 a2)]
      [(or (number? a1) (number? a2)) #f]
      [else (eq? a1 a2)])))

(define occur
  (lambda (a lat)
    (cond
      [(null? lat) 0]
      [(eq? (car lat) a) (add1 (occur a (cdr lat)))]
      [else (occur a (cdr lat))])))

(define one? (lambda (n) (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
      [(one? n) (cdr lat)]
      [else (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

; Chapter 5

(define rember*
  (lambda (a l)
    (cond
      [(null? l) '()]
      [(eq? a (car l)) (rember* a (cdr l))]
      [(atom? (car l)) (cons (car l) (rember* a (cdr l)))]
      [else (cons (rember* a (car l)) (rember* a (cdr l)))])))

(define insertR*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons old (cons new (insertR* new old (cdr l))))]
         [else (cons (car l) (insertR* new old (cdr l)))])]
      [else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))])))

(define occur*
  (lambda (a l)
    (cond
      [(null? l) 0]
      [(eq? a (car l)) (add1 (occur* a (cdr l)))]
      [(atom? (car l)) (occur* a (cdr l))]
      [else (+ (occur* a (car l)) (occur* a (cdr l)))])))

(define subst*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons new (subst* new old (cdr l)))]
         [else (cons (car l) (subst* new old (cdr l)))])]
      [else (cons (subst* new old (car l)) (subst* new old (cdr l)))])))

(define insertL*
  (lambda (new old l)
    (cond
      [(null? l) '()]
      [(atom? (car l))
       (cond
         [(eq? (car l) old) (cons new (cons old (insertL* new old (cdr l))))]
         [else (cons (car l) (insertL* new old (cdr l)))])]
      [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))])))

(define member*
  (lambda (a l)
    (cond
      [(null? l) #f]
      [(atom? (car l))
       (cond
         [(eq? a (car l)) #t]
         [else (member* a (cdr l))])]
      [else (or (member* a (car l)) (member* a (cdr l)))])))

(define leftmost
  (lambda (l)
    (cond
      [(atom? (car l)) (car l)]
      [else (leftmost (car l))])))

(define equal?
  (lambda (s1 s2)
    (cond
      [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
      [(or (atom? s1) (atom? s2)) #f]
      [else (eqlist? s1 s2)])))

(define eqlist?
  (lambda (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(or (null? l1) (null? l2)) #f]
      [else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))])))

(define rember
  (lambda (s l)
    (cond
      [(null? l) empty]
      [(equal? (car l) s) (cdr l)]
      [else (cons (car l) (rember s (cdr l)))])))

; Chapter 6
(define numbered?
  (lambda (s)
    (cond
      [(atom? s) (number? s)]
      [(eq? (operator s) '+)
       (and (numbered? (1st-sub-exp s)) (numbered? (2nd-sub-exp s)))]
      [(eq? (operator s) 'x)
       (and (numbered? (1st-sub-exp s)) (numbered? (2nd-sub-exp s)))]
      [(eq? (operator s) 'exp)
       (and (numbered? (1st-sub-exp s)) (numbered? (2nd-sub-exp s)))])))

(define 1st-sub-exp (lambda (aexp) (car aexp)))

(define 2nd-sub-exp (lambda (aexp) (caddr aexp)))

(define operator (lambda (aexp) (cadr aexp)))

(define sero? (lambda (n) (null? n)))

(define edd1 (lambda (n) (cons empty n)))

(define zub1 (lambda (n) (cdr n)))


; Chapter 7

(define set?
  (lambda (lat)
    (cond
      [(null? lat) #t]
      [(member? (car lat) (cdr lat)) #f]
      [else (set? (cdr lat))])))

(define makeset
  (lambda (lat)
    (cond
      [(null? lat) empty]
      [else (cons (car lat) (makeset (multirember (car lat) (cdr lat))))])))

(define subset?
  (lambda (set1 set2)
    (cond
      [(null? set1) #t]
      [else (and (member? (car set1) set2) (subset? (cdr set1) set2))])))

(define eqset?
  (lambda (set1 set2) (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      [(null? set1) #f]
      [else (or (member? (car set1) set2) (intersect? (cdr set1) set2))])))

(define intersect
  (lambda (set1 set2)
    (cond
      [(null? set1) empty]
      [(member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2))]
      [else (intersect (cdr set1) set2)])))

(define union
  (lambda (set1 set2)
    (cond
      [(null? set1) set2]
      [(member? (car set1) set2) (union (cdr set1) set2)]
      [else (cons (car set1) (union (cdr set1) set2))])))

(define difference
  (lambda (set1 set2)
    (cond
      [(null? set1) empty]
      [(member? (car set1) set2) (difference (cdr set1) set2)]
      [else (cons (car set1) (difference (cdr set1) set2))])))

(define intersectall
  (lambda (l-set)
    (cond
      [(null? (cdr l-set)) (car l-set)]
      [else (intersect (car l-set) (intersectall (cdr l-set)))])))

(define a-pair?
  (lambda (x)
    (cond
      [(atom? x) #f]
      [(null? x) #f]
      [(null? (cdr x)) #f]
      [(null? (cdr (cdr x))) #t]
      [else #f])))

(define first car)

(define second cadr)

(define third caddr)

(define build
  (lambda (sl s2)
    (cond
      [else (cons sl (cons s2 empty))])))

(define fun? (lambda (rel) (set? (firsts rel))))

(define revpair (lambda (pair) (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      [(null? rel) empty]
      [else (cons (revpair (car rel)) (revrel (cdr rel)))])))

(define seconds
  (lambda (l)
    (cond
      [(null? l) empty]
      [else (cons (cadar l) (seconds (cdr l)))])))

(define fullfun? (lambda (fun) (set? (seconds fun))))

(define one-to-one? (lambda (fun) (fun? (revrel fun))))

; Chapter 8

(define eq?-c (lambda (a) (lambda (x) (eq? x a))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        [(null? l) empty]
        [(test? (car l) a) (cdr l)]
        [else (cons (car l) ((rember-f test?) a (cdr l)))]))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) empty]
        [(test? (car l) old) (cons new (cons old (cdr l)))]
        [else (cons (car l) ((insertL-f test?) new old (cdr l)))]))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        [(null? l) empty]
        [(test? (car l) old) (cons old (cons new (cdr l)))]
        [else (cons (car l) ((insertR-f test?) new old (cdr l)))]))))

(define seqL (lambda (new old l) (cons new (cons old l))))

(define seqR (lambda (new old l) (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        [(null? l) empty]
        [(eq? (car l) old) (seq new old (cdr l))]
        [else (cons (car l) ((insert-g seq) new old (cdr l)))]))))

(define insertL (insert-g seqL))

(define insertR (insert-g seqR))

(define seqS (lambda (new old l) (cons new l)))

(define subst (insert-g seqS))

(define atom-to-function
  (lambda (x)
    (cond
      [(eq? x '+) add]
      [(eq? x 'x) mult]
      [else exp])))

(define value
  (lambda (nexp)
    (cond
      [(atom? nexp) nexp]
      [else
       ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
                                           (value (2nd-sub-exp nexp)))])))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) empty]
        [(test? a (car lat)) ((multirember-f test?) a (cdr lat))]
        [else (cons (car lat) ((multirember-f test?) a (cdr lat)))]))))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c (quote tuna)))

; etc

(define filter
  (lambda (f lat)
    (cond
      [(null? lat) '()]
      [(not (f (car lat))) (filter f (cdr lat))]
      [(cons (car lat) (filter f (cdr lat)))])))

(define nnot (lambda (f) (lambda (x) (not (f x)))))

(define comp (lambda (f g) (lambda (x) (f (g x)))))
