;ex 1.3

(define (square x) (* x x))

;ex 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;ex 1.6

(define (new-if predicate if-clause else-clause)
  (cond (predicate if-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

;ex 1.7

(define (good-enough2? guess x)
  (< (abs (- (improve guess x) guess)) 0.001))

;ex 1.8

(define (cube-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-cube? guess x)
  (< (abs (- guess (improve-cube guess x))) 0.001))

;ex 1.9

; first process is recursive, second one is iterative

;ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(define (f n)
  (a 0 n))

(define (g n)
  (a 1 n))

(define (h n)
  (a 2 n))


;ex 1.11
(define (f n)
  (if (< n 3) n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(define (g n)
  (define (g-iter a b c count)
    (if (= count 0) a
	(g-iter b c
		(+ c (* 2 b) (* 3 a)) (- count 1))))
  (g-iter 0 1 2 n))

;ex1.12

(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
	 (pascal (- row 1) col))))

;ex1.16

(define (fast-exp b n)
  (define (fast-exp-iter a b n)
    (if (= n 0) a
	(if (= (remainder n 2) 1)
	    (fast-exp-iter (* a b) b (- n 1))
	    (fast-exp-iter a (* b b) (/ n 2)))))
  (fast-exp-iter 1 b n))

;ex1.17

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (if (= b 0) 0
      (if (= (remainder b 2) 0)
	  (double (fast-mult a (halve b)))
	  (+ a (fast-mult a (- b 1))))))

;ex1.18
(define (fast-mult-iter a b)
  (define (fast-mult-iter-helper x y z)
    (cond ((= z 0) x)
	  ((= (remainder z 2) 0)
	   (fast-mult-iter-helper x (double y) (halve z)))
	  ((fast-mult-iter-helper (+ x y) y (- z 1)))))
  (fast-mult-iter-helper 0 a b))


;fermat test

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))


;ex1.21

(define (smallest-divisor n)
  (define (divides? test-divisor n)
    (= (remainder n test-divisor) 0))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))


;ex1.22

(define (prime? n)
  (if (= n 1) false
      (= (smallest-divisor n) n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lower-bound)
  (define (check-is-prime value count)
    (define (display-prime value count)
      (newline)
      (display value)
      (check-is-prime (+ value 2) (+ count 1)))
    (if (> count 2) (display " *** ")
	(if (prime? value)
	    (display-prime value count)
	    (check-is-prime (+ value 2) count))))
  (if (even? lower-bound)
      (search-for-primes (+ lower-bound 1))
      (check-is-prime lower-bound 0)))


;ex1.29 simpson's rule

(define (sum term next a b)
  (if (> a b) 0
      (+ (term a) (sum term next (next a) b))))

(define (inc x) (+ x 1))
(define (identity x) x)

(define (sum-integers a b)
  (sum identity inc a b))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (define (add-two-h x) (+ x (* 2 h)))
  (define first-sum (sum f add-h a b))
  (define second-sum (sum f add-two-h (add-h a) b))
  (* (/ h 3.0) (+ (- (* 2 first-sum) a b)
		(* 2 second-sum))))

(define (cube x) (* x x x))

;ex1.30

(define (sum-iter term next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))


;ex1.31

(define (product term next a b)
  (if (> a b) 1
      (* (term a) (product term next (next a) b))))

(define (factorial n)
  (product identity inc 1 n))

(define (pi-term a)
  (/ (* a (+ a 2)) (square (+ a 1))))

(define (pi-next a) (+ a 2))

(define (pi-product a n)
  (product pi-term pi-next a n))

(define (accumulate combiner null-value term next a b)
  (if (> a b) null-value
      (combiner (term a)
		(accumulate combiner null-value term next (next a) b))))

(define (sum-accumulate term next a b)
  (accumulate + 0 term next a b))

;ex1.34

(define (f g)
  (g 2))

;ex 1.35 1.36

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (newline)
  (display guess)
  (if (close-enough? (f guess) guess)
      (f guess)
      (fixed-point f (f guess))))

;golden ratio approximation

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;ex 1.37

(define (cont-frac n d k)
  (define (cf-helper start)
    (if (= start k)
	(/ (n k) (d k))
	(/ (n start) (+ (d start) (cf-helper (+ start 1))))))
  (cf-helper 1))

(define (cont-frac-iter n d k)
  (define (cf-helper result count)
    (if (= count 0)
	result
	(cf-helper (/ (n count)
		      (+ (d count) result))
		   (- count 1))))
  (cf-helper 0 k))

(cont-frac-iter (lambda (i) 1.0)
		(lambda (i) 1.0)
		100)

;ex 1.38

(define (euler-denom i)
  (cond ((= (remainder i 3) 0) 1)
	((= (remainder i 3) 1) 1)
	(else (* 2 (/ (+ i 1) 3)))))

; ex 1.39

(define (tan-cf x k)
  (define (tan-num i)
    (if (= i 1) x
	(- (* x x))))
  (cont-frac-iter tan-num
		  (lambda (i) (- (* 2 i) 1))
		  k))

; ex 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a (* x x))
       (* b x)
       c)))

(define (deriv g)
  (define dx 0.0001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))


(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

; ex 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose (lambda (x) (* x x))
	  (lambda (x) (+ x 1))) 5)

; ex 1.41
(define (double f)
  (compose f f))

((double (lambda (x) (+ x 1))) 1)
(((double double) (lambda (x) (+ x 1))) 1)
(((double (double double)) (lambda (x) (+ x 1))) 1)

; ex 1.43
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) ((repeated f (- n 1)) (f x)))))

; ex 1.44
(define (smooth f)
  (define dx 0.0001)
  (lambda (x)
    (/ (+ (f (-x dx))
	  (f x)
	  (f (+ x dx)))
       3)))

(define (smooth-n-fold f n)
  (repeated (smooth f) n))

; ex 1.46
(define (iterative-improve close-enough? improve)
  (define (test guess)
    (if (close-enough? guess)
	guess
	(test (improve guess))))
  test)

(define (sqrt x)
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (close-enough? guess)
    (< (abs (- x (* guess guess))) 0.0001))
  ((iterative-improve close-enough? improve) 1.0))

; Chapter 2
;ex 2.1
(define (gcd a b)
  (if (= b 0)
      (abs a)
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (/ (- n) g) (/ (- d) g))
	(cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;ex 2.2
(define (make-point x y)
  (cons x y))

(define (make-segment start end)
  (cons start end))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (mid-point segment)
  (make-point
   (/ (+ (x-point (start-segment segment))
	 (x-point (end-segment segment)))
      2)
   (/ (+ (y-point (start-segment segment))
	 (y-point (end-segment segment)))
      2)))

;ex 2.3

(define (make-rect1 point dimension)
  (cons (point dimension)))

(define (width-rect1 rect)
  (car (cdr rect)))

(define (height-rect1 rect)
  (cdr (cdr rect)))

(define (make-rect2 point1 point2)
  (cons (point1 point2)))

(define (width-rect2 rect)
  (abs (- (x-point (car rect)) (x-point (cdr rect)))))

(define (height-rect2 rect)
  (abs (- (y-point (car rect)) (y-point (cdr rect)))))

(define (area-rect rect)
  (* (width-rect1 rect) (height-rect1 rect)))

; ex 2.4
(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (x y) x)))

(define (cdr1 z)
  (z (lambda (x y) y)))

;ex 2.5
(define (expt b n)
  (if (= 0 n) 1
      (* b (expt b (- n 1)))))


(define (cons1 a b) ; a and b are non negative integers
  (* (expt 2 a) (expt 3 b)))

(define (car1 z)
  (define (car-acc value count)
    (if (= (remainder value 2) 1) count
	(car-acc (/ value 2) (+ count 1))))
  (car-acc z 0))

(define (cdr1 z)
  (define (cdr-acc value count)
    (if (not (= (remainder value 3) 0)) count
	(cdr-acc (/ value 3) (+ count 1))))
  (cdr-acc z 0))

;ex 2.6

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

; ex 2.7

(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

; ex 2.8

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y))
				 (- (lower-bound y)))))

; ex 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

; ex 2.17

(define (last-pair a)
  (cond ((null? a) (error "empty list argument"))
	((= (length a) 1) a)
	(else (last-pair (cdr a)))))

; ex 2.19

(define (cc amount coins)
  (cond ((= amount 0) 1)
	((or (null? coins) (< amount 0)) 0)
	(else (+ (cc amount (cdr coins))
		 (cc (- amount (car coins)) coins)))))

; ex 2.18

(define (reverse a)
  (if (null? a) a (append (reverse (cdr a)) (list (car a)))))

; ex 2.20

(define (same-parity first-arg . args)
  (define (filter-args a)
    (define parity (remainder first-arg 2))
    (if (null? a) a
	(if (= (remainder (car a) 2) parity)
	    (cons (car a) (filter-args (cdr a)))
	    (filter-args (cdr a)))))
  (cons first-arg (filter-args args)))

; ex 2.24

(define a (list 1 (list 2 (list 3 4))))

;ex 2.27

(define (deep-reverse x)
  (if (or (null? x)
	  (not (pair? x)))
      x
      (append (deep-reverse (cdr x))
	      (list (deep-reverse (car x))))))

;ex 2.28

(define (fringe tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))


;ex 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr structure))

(define (total-weight-branch branch)
    (if (not (pair? (branch-structure branch)))
	(branch-structure branch)
	(total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

; ex 2.30

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree))) tree))

; ex 2.31

(define (tree-map tree f)
  (map (lambda (subtree)
	 (if (pair? subtree) (tree-map subtree f)
	     (f subtree))) tree))
	    

(define (square-tree2 tree)
  (tree-map tree square))

; ex 2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (cons (car s) subset))
			  rest)))))

; ex 2.33
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map1 f sequence)
  (accumulate (lambda (x y)
		(cons (f x) y)
		) () sequence))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length1 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

;ex 2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (node)
			 (if (not (pair? node))
			     1
			     (count-leaves node)))
		       tree)))

; ex 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))


; ex 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; ex 2.39

(define (reverse sequence)
  (accumulate (lambda (x y) (append y (list x))) () sequence))

; ex 2.40

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high) ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap 
   (lambda (i) (map (lambda (j)
		      (list i j))
		    (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; ex 2.41

(define (unique-triplets n)
  (flatmap (lambda (pair)
	     (map (lambda (k)
		    (list (cadr pair) (car pair) k))
		  (enumerate-interval (+ (car pair) 1) n)))
	   (unique-pairs n)))

(define (sum-triplets n s)
  (filter (lambda (triplet)
	    (= (+ (car triplet)
		  (cadr triplet)
		  (caddr triplet)) s))
	  (unique-triplets n)))

(define (remove item seq)
  (filter (lambda (x) (not (= item x))) seq))

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))

