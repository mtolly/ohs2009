;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 2009comp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Oregon High School 7th Annual Programming Competition, May 2009
; Solutions by Mike Tolly
; PLT Scheme - Language: HtDP Intermediate Student w/ lambda

; [A]lgae Growth

; a-step: string -> string
; Performs one step in the algae simulation.
; Precondition: str is a string which consists of any number of 'A's and 'B's.
; Postcondition: The next step in the algae growth is returned.
(define (a-step str)
  (cond ((string=? str "") "")
        ((char=? (string-ref str 0) #\A) (string-append "AB" (a-step (substring str 1 (string-length str)))))
        (else (string-append "A" (a-step (substring str 1 (string-length str)))))))

; a: num -> string
; A simulation of algae growth patterns.
; Each step, a string of As and Bs undergoes the following transformation:
; Each A is replaced with AB, and each B is replaced with A.
; Starting with "A", the given number is the number of steps to be performed in the growth.
; Precondition: n is an integer, greater than or equal to 0.
; Postcondition: The algae simulation string, at the given number of growth steps, is returned.
(define (a n)
  (if (= n 0) "A"
      (a-step (a (- n 1)))))

(check-expect (a 0) "A")
(check-expect (a 1) "AB")
(check-expect (a 2) "ABA")
(check-expect (a 3) "ABAAB")
(check-expect (a 4) "ABAABABA")
(check-expect (a 5) "ABAABABAABAAB")
(check-expect (a 6) "ABAABABAABAABABAABABA")
(check-expect (a 7) "ABAABABAABAABABAABABAABAABABAABAAB")

; This is a much simpler implementation of A.
; The one above was the one that was submitted, but this version
; realizes that the algae growth series is in fact the Fibonacci sequence.
(define (a-alternate n)
  (cond ((= n -1) "B") ; this is actually the natural "first" element of the sequence.
        ((= n 0) "A")
        (else (string-append (a-alternate (- n 1)) (a-alternate (- n 2))))))

(check-expect (a-alternate 0) "A")
(check-expect (a-alternate 1) "AB")
(check-expect (a-alternate 2) "ABA")
(check-expect (a-alternate 3) "ABAAB")
(check-expect (a-alternate 4) "ABAABABA")
(check-expect (a-alternate 5) "ABAABABAABAAB")
(check-expect (a-alternate 6) "ABAABABAABAABABAABABA")
(check-expect (a-alternate 7) "ABAABABAABAABABAABABAABAABABAABAAB")

; [B]arnyard Headcount

; b: num num -> string
; A farm has a certain number of cows (1 head, 4 feet) and turkeys (1 head, 2 feet).
; Given the number of heads and feet among all the animals, this will return the number of cows.
; Precondition: heads and feet are both integers, greater than or equal to 0.
;               feet should be an even number, which is greater than or equal to heads.
; Postcondition: The number of cows is returned.
(define (b heads feet)
  (- (/ feet 2) heads))

(check-expect (b 0 0) 0)
(check-expect (b 10 24) 2)
(check-expect (b 30 120) 30)

; [C]hange

; The money structure is copied from the competition booklet.
(define-struct money (half-dollar quarter dime nickel penny))
;; half-dollar : number of coins
;; quarter : number of coins
;; dime : number of coins
;; nickel : number of coins
;; penny : number of coins

; c: num -> money
; Calculates the smallest assortment of coins that will produce the given value (in cents).
; Precondition: cents is an integer, greater than or equal to zero.
; Postcondition: The money structure is returned.
(define (c cents)
  (let* ((hdollar (floor (/ cents 50))) (cents1 (modulo cents 50))
         (quarter (floor (/ cents1 25))) (cents2 (modulo cents1 25))
         (dime (floor (/ cents2 10))) (cents3 (modulo cents2 10))
         (nickel (floor (/ cents3 5))) (penny (modulo cents3 5)))
    (make-money hdollar quarter dime nickel penny)))

(check-expect (c 0) (make-money 0 0 0 0 0))
(check-expect (c 41) (make-money 0 1 1 1 1))
(check-expect (c 127) (make-money 2 1 0 0 2))

; [F]ractions, continued.

; f: num num -> (num ...)
; Calculates the terms of a continued fraction for a given real number.
; The two arguments are the numerator and denominator.
; An empty list is returned if the denominator is 0.
; (If the given number is 0, an empty list will never be returned: it will return a list containing a 0.)
; Precondition: num and den are numbers, the numerator and denominator (respectively) of a fraction.
; Postcondition: A list of integers is returned, which represents the continued fraction.
(define (f num den)
  (cond ((= 0 den) empty)
        ((= 0 (modulo num den)) (cons (/ num den) empty))
        (else (cons (floor (/ num den)) (f den (modulo num den))))))

(check-expect (f 5 0) empty)
(check-expect (f 0 1) (list 0))
(check-expect (f 16 7) (list 2 3 2))
(check-expect (f 2 3) (list 0 1 2))

; [M]esopotamian Math

; to-base-helper: num num num -> (num ...)
; Helper for the to-base function.
(define (to-base-helper n base exp)
  (if (= exp 0) (cons n empty)
      (cons (floor (/ n (expt base exp))) (to-base-helper (modulo n (expt base exp)) base (- exp 1)))))

; to-base: num num -> (num ...)
; Finds the digits for a given number, when represented in a given base.
; The digits will be in order from highest to lowest.
; So in a list of 3 numbers which is in base 10, the first number is 100s,
; the second is 10s, and the third is 1s.
; Precondition: n is an integer, greater than or equal to 0.
;               base is an integer, greater than or equal to 2.
; Postcondition: The list of digits is returned.
(define (to-base n base)
  (if (= n 0) empty
      (to-base-helper n base (inexact->exact (floor (/ (log n) (log base)))))))

(check-expect (to-base 0 10) empty)
(check-expect (to-base 45791 60) (list 12 43 11))

; from-base: (num ...) num -> num
; Turns the output of the to-base function back into a normal number,
; given the list of digits and the base they are in.
; Precondition: digits is a list of non-negative integers.
;               base is an integer, greater than or equal to 2.
; Postcondition: The number is returned.
(define (from-base digits base)
  (if (empty? digits) 0
      (+ (* (first digits) (expt base (- (length digits) 1))) (from-base (rest digits) base))))

(check-expect (from-base empty 10) 0)
(check-expect (from-base (list 0) 10) 0)
(check-expect (from-base (list 12 43 11) 60) 45791)

; m: (num ...) string (num ...) -> (num ...)
; Performs arithmetic on numbers that are represented in sexagecimal (base 60).
; The two input lists are the sexagecimal numbers, and the string is an operator,
; one of "+", "-", "*", or "/".
; Precondition: n1 and n2 are both base 60 numbers, in the form used by
;               the to-base and from-base functions.
;               op is a string representing one of the four basic arithmetic operators.
; Postcondition: The result of the operation is returned, also in the form of a base 60 number.
(define (m n1 op n2)
  (to-base ((cond ((string=? op "+") +)
                  ((string=? op "-") -)
                  ((string=? op "*") *)
                  ((string=? op "/") /)) ; This whole cond expression could be written as (eval (string->symbol op)) in full Scheme.
            (from-base n1 60)
            (from-base n2 60))
           60))

(check-expect (m (list 1 2 47) "+" (list 4 22))
              (list 1 7 9))
(check-expect (m (list 15 4) "*" (list 3 41 2 17))
              (list 55 30 18 24 8))
(check-expect (m (list 1 2 47) "-" (list 4 22))
              (list 58 25))
(check-expect (m (list 2 5 34) "/" (list 1 2 47))
              (list 2))

; [N]on-Collinearity

; slope: posn posn -> num OR empty
; Finds the slope of the line that connects two points.
; Returns empty if the slope is undefined (the line is vertical), or if the two points are the same.
; Precondition: p1 and p2 are numbers.
; Postcondition: The slope is returned.
(define (slope p1 p2)
  (if (= (posn-x p1) (posn-x p2)) empty
      (/ (- (posn-y p1) (posn-y p2)) (- (posn-x p1) (posn-x p2)))))

(check-expect (slope (make-posn 0 0) (make-posn 0 0)) empty)
(check-expect (slope (make-posn 0 0) (make-posn 0 5)) empty)
(check-expect (slope (make-posn 0 0) (make-posn 5 0)) 0)
(check-expect (slope (make-posn 0 0) (make-posn 5 5)) 1)
(check-expect (slope (make-posn 0 0) (make-posn 5 -5)) -1)

; n: posn posn posn posn -> posn
; Given three points which are on the same line, and one which is not,
; returns the point which is not.
; Precondition: Three of the given points are collinear, and the other one is not collinear with them.
; Postcondition: The point, out of the four, which is not collinear with the other three is returned.
(define (n p1 p2 p3 p4)
  (let ((s12 (slope p1 p2))
        (s13 (slope p1 p3))
        (s14 (slope p1 p4)))
    (cond ((equal? s12 s13) p4)
          ((equal? s12 s14) p3)
          ((equal? s13 s14) p2)
          (else p1))))

(check-expect (n (make-posn 0 0)
                 (make-posn 1 15)
                 (make-posn 0 4)
                 (make-posn 0 -3))
              (make-posn 1 15))
(check-expect (n (make-posn 2 3)
                 (make-posn 3 5)
                 (make-posn 8 15)
                 (make-posn 7 12))
              (make-posn 7 12))
(check-expect (n (make-posn 5 4)
                 (make-posn 15 10)
                 (make-posn 20 12)
                 (make-posn 30 19))
              (make-posn 20 12))
(check-expect (n (make-posn 0 3)
                 (make-posn 1 7)
                 (make-posn -6 7)
                 (make-posn 15 7))
              (make-posn 0 3))

; [P]aired Primes

; lowest-factor-helper: num num -> num
; Helper for the lowest-factor function.
(define (lowest-factor-helper n test)
  (if (= (modulo n test) 0) test
      (lowest-factor-helper n (+ test 1))))

; lowest-factor: num -> num
; Returns the lowest factor, which is greater than 1, for a given number.
; Precondition: n is an integer, greater than or equal to 2.
; Postcondition: The factor is returned.
(define (lowest-factor n)
  (lowest-factor-helper n 2))

; prime?: num -> bool
; Returns true if the given number is prime (has no factors other than 1 and itself).
; Precondition: n is an integer.
; Postcondition: The boolean, representing whether or not n is prime, is returned
(define (prime? n)
  (if (< n 2) false
      (= n (lowest-factor n))))

(check-expect (prime? 0) false)
(check-expect (prime? 1) false)
(check-expect (prime? 2) true)
(check-expect (prime? 3) true)
(check-expect (prime? 4) false)
(check-expect (prime? 5) true)
(check-expect (prime? 6) false)
(check-expect (prime? 7) true)
(check-expect (prime? 8) false)
(check-expect (prime? 9) false)
(check-expect (prime? 10) false)

; next-pair-prime: num -> posn
; Returns the next prime number (greater than or equal to the given number)
; such that 2p+1 is also a prime number.
; Precondition: n is an integer.
; Postcondition: The first number of a prime pair is returned.
(define (next-pair-prime n)
  (let ((second (+ (* 2 n) 1)))
    (if (and (prime? n) (prime? second)) n
        (next-pair-prime (+ n 1)))))

; p-helper: num -> num
; Returns the nth prime number (where 2 is the 1st prime number)
; such that 2p+1 is also a prime number.
; Precondition: n is a positive integer.
; Postcondition: The first number of a prime pair is returned.
(define (p-helper n)
  (if (= n 1) 2
      (next-pair-prime (+ (p-helper (- n 1)) 1))))

; p: num -> num
; Returns a posn, where the x value is the nth prime number
; such that 2p+1 is also a prime number, and the y value is 2p+1.
; Precondition: n is a positive integer.
; Postcondition: A posn, where the x and y are the two primes in a prime pair, is returned.
(define (p n)
  (let ((x (p-helper n)))
    (make-posn x (+ (* 2 x) 1))))

(check-expect (p 1) (make-posn 2 5))
(check-expect (p 2) (make-posn 3 7))
(check-expect (p 3) (make-posn 5 11))
(check-expect (p 4) (make-posn 11 23))
(check-expect (p 5) (make-posn 23 47))

; [S]ettle the Differences

; s: num num num num -> num
; Finds the number of times it takes to settle the differences between four numbers to zero.
; Precondition: n1, n2, n3, and n4 are numbers.
; Postcondition: The number of times the differences are taken is returned.
(define (s n1 n2 n3 n4)
  (if (= 0 n1 n2 n3 n4) 0
      (+ 1 (s (abs (- n1 n2))
              (abs (- n2 n3))
              (abs (- n3 n4))
              (abs (- n4 n1))))))

(check-expect (s 0 0 0 0) 0)
(check-expect (s -3 -3 -3 -3) 1)
(check-expect (s 43 11 21 3) 4)
(check-expect (s 2 10 6 14) 3)

; Prime Factor [T]riples

; IMPORTANT! This solution depends on 2 functions defined above: the lowest-factor function,
; and its helper, lowest-factor-helper. If you copy the Prime Factor Triples functions
; into a separate file, make sure you also copy these two functions!

; prime-factors: num -> (num ...)
; Breaks down a number into its prime factors -- the prime numbers which, when
; multiplied together, equal the number.
; Precondition: n is an integer, greater than or equal to 2.
; Postcondition: A list of the prime factors is returned.
(define (prime-factors n)
  (let ((f (lowest-factor n)))
    (if (= n f) (cons n empty)
        (append (prime-factors f) (prime-factors (/ n f))))))

; count: any list -> num
; Counts how many times the given element appears in the given list.
; Precondition: l is a list.
; Postcondition: The number of times e appears in l is returned.
(define (count e l)
  (cond ((empty? l) 0)
        ((equal? e (first l)) (+ 1 (count e (rest l))))
        (else (count e (rest l)))))

(check-expect (count 5 empty) 0)
(check-expect (count 5 (list 3)) 0)
(check-expect (count 5 (list 5)) 1)
(check-expect (count 5 (list 1 2 5 3 5 5 8 5)) 4)

; remove-all: any list -> list
; Removes any instances of the given element from the list.
; Precondition: l is a list.
; Postcondition: A new list is returned, which is like l except that
;                all elements equal to e have been removed.
(define (remove-all e l)
  (cond ((empty? l) empty)
        ((equal? e (first l)) (remove-all e (rest l)))
        (else (cons (first l) (remove-all e (rest l))))))

(check-expect (remove-all 3 empty) empty)
(check-expect (remove-all 3 (list 5)) (list 5))
(check-expect (remove-all 3 (list 3)) empty)
(check-expect (remove-all 3 (list 3 5 2 3 7 9 3)) (list 5 2 7 9))

; tuples: list num -> num
; Counts how many elements in the list appear exactly n times.
; Precondition: l is a list, and n is a positive integer.
; Postcondition: The number of n-tuples that appear in the list is returned.
(define (tuples l n)
  (cond ((empty? l) 0)
        ((= (count (first l) l) n) (+ 1 (tuples (remove-all (first l) l) n)))
        (else (tuples (remove-all (first l) l) n))))

; t: num -> num
; Calculates how many numbers appear in the given number's prime factors exactly three times.
; Precondition: n is an integer, greater than or equal to 2.
; Postcondition: The number of prime factor triples is returned.
(define (t n)
  (tuples (prime-factors n) 3))

(check-expect (t 36) 0)
(check-expect (t 4779621) 2)

; [U]niform Random Distribution

; u: num num -> boolean
; Given a step value and a modulo value, this will determine whether
; they would be good values for a simple psuedo-random number function.
; Precondition: step and mod are positive integers.
; Postcondition: A boolean is returned.
(define (u step mod)
  (= (gcd step mod) 1))

(check-expect (u 3 5) true)
(check-expect (u 3 6) false)
(check-expect (u 3 7) true)
(check-expect (u 3 8) true)
(check-expect (u 3 9) false)
(check-expect (u 6 7) true)
(check-expect (u 6 8) false)
(check-expect (u 6 9) false)
(check-expect (u 15 20) false)
