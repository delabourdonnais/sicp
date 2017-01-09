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


		       


  