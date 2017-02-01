(define (factorial n)
  (define (factorial-iter product count max-count)
    (if (> count max-count)
	product
	(factorial-iter (* product count)
			(+ count 1)
			max-count)))
  (factorial-iter 1 1 n))

(factorial 33)

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(g 6)
(define (count-change amount)
  (define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
	  ((or (< amount 0) (= kinds-of-coins 0)) 0)
	  (else (+ (cc amount (- kinds-of-coins 1))
		   (cc (- amount
			  (first-denomination kinds-of-coins))
		       kinds-of-coins)))))
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
	  ((= kinds-of-coins 2) 5)
	  ((= kinds-of-coins 3) 10)
	  ((= kinds-of-coins 4) 25)
	  ((= kinds-of-coins 5) 50)))
    (cc amount 5))
(count-change 100)

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

(g 25)

;ex1.12

(define (pascal row col)
  (if (or (= col 1) (= row 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
	 (pascal (- row 1) col))))

(pascal 5 3)

;ex1.16

(define (fast-exp b n)
  (define (fast-exp-iter a b n)
    (if (= n 0) a
	(if (= (remainder n 2) 1)
	    (fast-exp-iter (* a b) b (- n 1))
	    (fast-exp-iter a (* b b) (/ n 2)))))
  (fast-exp-iter 1 b n))

(fast-exp 2 10)

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

(fermat-test 19)

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))




(fast-prime? 47 5)

;ex1.21

(define (smallest-divisor n)
  (define (divides? test-divisor n)
    (= (remainder n test-divisor) 0))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(smallest-divisor 59)

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

(search-for-primes 1000)

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
(simpson cube 0 1 200)

;ex1.30

(define (sum-iter term next a b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(sum-iter identity inc 1 10)

;ex1.31

(define (product term next a b)
  (if (> a b) 1
      (* (term a) (product term next (next a) b))))

(define (factorial n)
  (product identity inc 1 n))

(factorial 4)

;ex 1.35

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.0001))
  (if (close-enough? (f guess) guess)
      (f guess)
      (fixed-point f (f guess))))

(fixed-point cos 1.0)
;golden ratio approximation

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 2)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; ex 1.36
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

;ex 1.37

(define (cont-frac n d k)
  (define (cf-helper start)
    (if (= start k)
	(/ (n k) (d k))
	(/ (n start) (+ (d start) (cf-helper (+ start 1))))))
  (cf-helper 1))

(define golden-inv (cont-frac
		    (lambda (i) 1.0)
		    (lambda (i) 1.0)
		    1000))
(/ 1 golden-inv)

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

(+ (cont-frac-iter (lambda (i) 1.0)
		euler-denom
		1000) 2)

; ex 1.39

(define (tan-cf x k)
  (define (tan-num i)
    (if (= i 1) x
	(- (* x x))))
  (cont-frac-iter tan-num
		  (lambda (i) (- (* 2 i) 1))
		  k))

(tan-cf 3.14 100000)

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

(newtons-method (cubic 3 3 1) 1)

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

((repeated (lambda (x) (* x x)) 2) 5)

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

; ex 1.45
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

(sqrt 2)
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

(print-rat (make-rat 15 0))

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

; ex 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))

(define z (cons 1 66))

(define (exp b n)
  (cond ((= n 0) 1)
	((= (remainder n 2) 0)
	 (* (exp b (/ n 2)) (exp b (/ n 2))))
	(else (* b (exp b (- n 1))))))


;ex 2.5
(define (cons a b) ; a and b are non negative integers
  (* (exp 2 a) (exp 3 b)))

(define (car z)
  (define (car-acc value count)
    (if (= (remainder value 2) 1) count
	(car-acc (/ value 2) (+ count 1))))
  (car-acc z 0))

(define (cdr z)
  (define (cdr-acc value count)
    (if (not (= (remainder value 3) 0)) count
	(cdr-acc (/ value 3) (+ count 1))))
  (cdr-acc z 0))

; ex 2.17

(define (last-pair a)
  (cond ((null? a) (error "empty list argument"))
	((= (length a) 1) a)
	(else (last-pair (cdr a)))))

(last-pair (list 4 66 22 99))

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

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; ex 2.24
(define a (list 1 (list 2 (list 3 4))))
(length a)
;ex 2.27

(define x (list (list 1 2) (list 3 4)))
(reverse x)
(define (deep-reverse x)
  (if (or (null? x)
	  (not (pair? x)))
      x
      (append (deep-reverse (cdr x))
	      (list (deep-reverse (car x))))))
(deep-reverse x)

;ex 2.28

(define (fringe tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (list tree))
	(else (append (fringe (car tree))
		      (fringe (cdr tree))))))

(fringe (list x x))

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

; ex 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (subset)
			    (cons (car s) subset))
			  rest)))))

(subsets (list 1 2 3))

; ex 2.33
(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map1 f sequence)
  (accumulate (lambda (x y)
		(cons (f x) y)
		) '() sequence))

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
(horner-eval 3 (list 1 0 1))

;ex 2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (node)
			 (if (not (pair? node))
			     1
			     (count-leaves node)))
		       tree)))

(define tree (list (list 1 2) 3 (list 4 (list 5 6))))
(count-leaves tree)
