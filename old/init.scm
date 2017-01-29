;; ex-1.1
10
12
8
3
6
a=3
b=4
19
false
4
16
6
16

;; ex-1.2
(/ (+
  5 4 (- 2 (- 3 (+ 6 (/ 4 5))))
) (*
  3 (- 6 2) (- 2 7)
))

;; ex-1.3
(define (square x) (* x x))

(define (sum-of-square x y) (+
  (square x)
  (square y)
))

(define (sum-of-square-of-two-largest x y z)
(if (> x y)
  (if (> y z) (sum-of-square x y) (sum-of-square x z))
  (if (> x z) (sum-of-square x y) (sum-of-square y z))
))

;; ex-1.4
If b is greater than 0 add arguments a and b.  If b is less than zero subtract arguments a and b.

;; ex-1.5
using applicative order the evaluation will never complete because (p) calls (p).  using normal order the expression evaluates step by step to 0.

;; ex-1.6
same as 1.5.  both expressions will evaluate causing a endless loop.

;; ex-1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
  )
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (sqrt x)
  (sqrt-iter 1.0 x)
)

(sqrt 9)

;; -- alternate solution

(define (sqrt-iter guess x prev-guess)
  (if (good-enough? guess prev-guess)
    guess
    (sqrt-iter (improve guess x) x guess)
  )
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.001)
)

(define (sqrt x)
  (sqrt-iter 1.0 x -1.0)
)

(sqrt 9)

;;Checking the difference between the previous guess and the current guess is much more accurate with very small
;;and very large numbers.

;;Checking diff:
(sqrt 1.0000000000000002e-10) = 9.765966330621755e-4

;;Checking tolerance:
(sqrt 1.0000000000000002e-10) = .03125000106562499

;;Actual answer:
(sqrt 1.0000000000000002e-10) = 1e-5

;; ex-1.8

(define (curt-iter y x prev-y)
  (if (good-enough? y prev-y)
    y (curt-iter (improve y x) x y)
  )
)

(define (improve y x)
  (average y
  (/ (+
    (/ x (square y))
    (* 2 y)
  ) 3)
))

(define (average x y)
  (/ (+ x y) 2)
)

(define (good-enough? y prev-y)
  (< (abs (- y prev-y)) 0.001)
)

(define (curt x)
  (curt-iter 1.0 x -1.0)
)

(curt 343)

;; ex-1.9

;;the first of the two is recursive.

(+ 4 5 (inc (+ (dec 4) 5))

;;the later of the two is iterative.

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)

;; ex-1.10

(A 0 n) = 2n
(A 1 n) = (2^n)
(A 2 n) = 2^2^2... n times

(1 3) = 8

;; ex-1.11
(define (f x) (cond
  ((< x 3) x)
  ((> x 2) (+ (f (- x 1)) (* 2 (f (- x 2))) (* 3 (f (- x 3))) ))
))

(f 10)

;; keeping track of state
;; f(3) = f(2) + 2f(1) + 3f(0)
;; f(4) = f(3) + 2f(2) + 3f(1)
;; f(5) = f(4) + 2f(3) + 3f(2)
;; f(count) = f(a) + 2f(b) + 3f(c)
;; the newest number is a + 2b + 3c
;; the and the rest are shifted -> a=new-above b=a c=b

(define (f2 x) (cond
  ((< x 3) x)
  ((> x 2) (f2-iter 2 1 0 x))
))

(define (f2-iter a b c count) (cond
  ((< count 3) a)
  ((> count 2) (f2-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))
))

(f2 10)

;; ex-1.12

     1
    1 1
   1 2 1
  1 3 3 1
 1 4 6 4 1
1 5 ! ! 5 1

(define (fib x) (cond
  ((< x 2) x)
  ((> x 2)
    (+ (fib (- x 1)) (fib (- x 2)))
  )
))

0 1 1 2 3 5 8

fib(6) = 5 + 3
fib(5) = 5
fib(4) = 3
fib(3) = 2
fib(2) = 1

pascal(0, 0) = 1
pascal(1, 0) = 1

;; end cases.
pascal(x, 0) = 1;
pascal(x, x) = 1;

;; Important statement ->
;; think of iterative as an imperative language while loop

;; draw this out...
                    pascal(4, 2) = 6
      pascal(3, 1)              &    pascal(3, 2)
pascal(2, 0) & pascal(2, 1)       pascal(2, 1) & pascal(2, 2)


;; for each level this is what we need to know.
pascal(x-1, y-1) & pascal(x-1, y);

(define (pascal row col)
  (cond
    ((= col 0) 1)
    ((= row col) 1)
    (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))
  )
)

;; ex-1.13

(define (fib x) (cond
  ((< x 2) x)
  (else (+ (fib (- x 1)) (fib (- x 2))))
))

(fib 7)

(define psi (/ (+ 1 (sqrt 5)) 2))
(define (fib2 x) (/ (expt psi x) (sqrt 5)))

(fib2 7)

;; ex-1.14

;; nice way to represent a tree
cc(11 5) = cc(11 4) + cc((11 - 50) 5)
  cc((11 - 50) 5) = 0
  cc(11 4) = cc(11 3) + cc((11 - 25) 4)
    cc((11 - 25) 4) = 0
    cc(11 3) = cc(11 2) + cc((11 - 10) 3) ;; break tree apart from these.
      cc((11 - 10) 3) = cc(1 2) + cc((1 - 10) 3)
        cc((1 - 10) 3) = 0
        cc(1 2) = cc(1 1) + cc((1 - 5) 2)
          cc((1 - 5) 2) = 0
          cc(1 1) = cc(1 0) + cc(0 1)
            cc(1 0) = 0
            cc(0 1) = 1
    cc(11 2) = cc(11 1) + cc((11 - 5) 2)
      cc((11 - 5) 2) cc(6 2) = cc(6 1) + cc(1 1)
        cc(6 1) = cc(6 0) + cc(5 1)
          cc(6 0) = 0
          cc(5 1) = cc(5 0) + cc(4 1)
          cc(4 1) = cc(4 0) + cc(3 1)
          cc(3 1) = cc(3 0) + cc(2 1)
          cc(2 1) = cc(2 0) + cc(1 1)
          cc(1 1) = cc(1 0) + cc(0 1)
             = 1
        cc(1 1) = cc(1 0) + cc(0 0)
          cc(1 0) = 0
          cc(0 0) = 1
      cc(11 1) = cc(11 0) + cc(10 1)
        cc(10 1) =
        cc(9 1) =
        cc(8 1) =
        cc(7 1) =
        cc(6 1) =
        cc(5 1) =
        cc(4 1) =
        cc(3 1) =
        cc(2 1) =
        cc(1 1) =
        cc(0 1) = 1

(count-change 11) = 4

The space required is O(n).  At any point in time the tallest the tree will be is
the number of arguments broken down into 1.  for 11 cents the tallest is 11.
Space required is the maximum depth of the tree.

We can see that each step generates two nodes until a terminating step is reached.

T(a, k) number of calls generated by cc calling cc(n k) n is amount and k is number of coins.
T(n, 1) = 2n + 1
t(n, 1) = O(n) // it scales linearly as opposed to n^2 n^n log(n)

T(n, 2) = (n/5) * 2n + 1
T(n, 2) = O(n^2);  terms are multiplied together.

T(n, 3) = O(n^3)
T(n, 5) = O(n^5) // max number of coins.

;; ex-1.15


;; only count calls to the actual function
;; in this case calls to 'p'
sine(12.5) = p(sine(4.166))
  sine(4.166) = p(sine(1.388))
    sine(1.388) = p(sine(.462))
      sine(.462) = p(sine(.154))
        sine(.154) = p(sine(0.0513))
          sine(0.0513) = 0.0513

The space required is O(log(n)) this problem scales linearly.
We can triple the input by only adding one additional step.
They are both O(log(n))

;; ex-1.16


;; basic definition for a recursive function.
b^n = b * b^(n-1)
b^0 = 1
O(n) steps and O(n) space

if we make this iterative by tracking state loops
o(n) steps and O(1) space

for evens we can use a quick rule of
b^2 = b * b
b^4 = b^2 * b^2
b^8 = b^3 * b^3

to allow this to work with odds we can use this quick rule
b^n = (b^(b/2))^2 ;; if n is even
b^n = b * b^(n-1) ;; if n is odd

;; even predicate
(define (even? n) (= (remainder n 2) 0))

if we use the fast rule above for even tracking it cuts down to
O(log(n)) steps and O(log(n)) space recursively and less iteratively.

;; create javascript application for keeping track of rules of algebra and quick math facts.
;; have it learn and test you to get better and better.

(define (exp x y) (cond
  ((= y 1) x)
  (else (* x (exp x y-1)))
))

(exp 2 4) = (* 2 (exp 2 3))
(exp 2 3) = (* 2 (exp 2 2))
(exp 2 2) = (* 2 (exp 2 1))
(exp 2 1) = 2

;; -- -- -- --

;; predicate is a boolean function
;; create predicate for even?
(define (even? x) (= (remainder x 2) 0))

(define (exp x y) (cond
  ((= y 1) x)
  ((= y 2) (* x x)) ;; need if you don't want to use 'square' ;;
                    ;; if this wasn't here it would cause an endless loop.
  ((even? y) (exp (exp x (/ y 2)) 2) ) ;; is this correct?
  (else (* x (exp x y-1)))
))

2^2 * 2^2


(exp 2 4) = (* 2 (exp 2 3))
(exp 2 3) = (* 2 (exp 2 2))
(exp 2 2) = (* 2 (exp 2 1))
(exp 2 1) = 2


;; 0
(exp 2 4) = (exp (exp 2 2) 2)
;; 1
(exp 2 2) = (exp (exp 2 1) 2)
;; 2
(exp 2 1) = 2
;; 3
(exp 2 2) = (exp 2 2) ;; this would cause an endless loop.

(define (even? x) (= (remainder x 2) 0))
(define (exp x y) (exp-iter x y 1))
(define (exp-iter x counter value) (cond
  ((= counter 0) value)
  ((even? counter) (exp-iter (* x x) (/ counter 2) value)) ;; x instead of value works for evens.
  (else (exp-iter x (- counter 1) (* value x)))
))
(exp 2 9)

;;


;; ex-1.17

(define (even? a) (= (remainder a 2) 0))
(define (halve a) (/ a 2))
(define (double a) (+ a a))
(define (* a b) (cond
  ((= b 0) 0)
  ((even? b) (* (double a) (halve b))) ;; no need to recursively add this is handled by else
  (else (+ a (* a (- b 1))))
))

;; ex-1.18


(define (even? a) (= (remainder a 2) 0))
(define (halve a) (/ a 2))
(define (double a) (+ a a))

(define (* a b) (*-iter a b 0))
(define (*-iter base counter value) (cond
  ((= counter 0) value)
  ((even? counter) (*-iter (double base) (halve counter) value))
  (else (*-iter base (- counter 1) (+ base value)))
))

;; ex-1.19

T(a, b) = (a + b, a)

starting with this pair.
T (1, 0)
T^nth is fib

(Fib(n + 1), Fib(n))


Tpq(a, b) = (bq + aq + ap, bp + aq)
if we apply the above twice it is equivalent to applying Tp'q'

compute p' and q' in terms of p and q

(1, 0) * (1, 1) * (2, 1) * (3, 2) * (5, 3)


;; read up more on logarithm.

p = 0
q = 1

b^8 = b^4 * b^4

Tp'q' = Tpq Tpq
                               (1, 0)
(0*1 + 1*1 + 1*0, 0*0 + 1*1) = (1, 1)
(1*1 + 1*1 + 1*0, 1*0 + 1*1) = (2, 1)
(1*1 + 2*1 + 2*0, 1*0 + 2*1) = (3, 2)



a = 1
b = 0
p = 0
q = 1

a1 = bq + aq + ap
b1 = bp + aq

a2 = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p;
b2 = (bp + aq)p + (bq + aq + ap)q;

a' = bq' + aq' + ap'
b' = bp' + aq'

a2 = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p;

b2 = bp' + aq'
b2 = b(pp + qq) + a(qq + 2pq)
p' = p^2 + q^2
q' = q^2 + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;


;; ex-1.20

(define (gcd a b) (cond
  ((= b 0) a)
  (else (gcd b (remainder a b)))
))


;; -- normal order
(gcd ((= b 0) a) (else (gcd b (remainder a b))))

(gcd 206 40) =

;; 1 = 1
(gcd ((= 40 0) 206) (else (gcd 40 (remainder 206 40))))

;; 2 = 4
(gcd ((= (remainder 206 40) 0) 40) (else (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))

;; 3 = 9
(gcd ((= (remainder 40 (remainder 206 40)) 0) (remainder 206 40)) (else (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

;; 4 = 17
(gcd ((= (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder 40 (remainder 206 40))) (else (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))))

;; 5 = 29
;; the last remainder will not execute 18....
(gcd ((= {7} 0) {4}) (else (gcd {7} (remainder {4} {7}))))
;; remainder is called 18 times.

;; -- application order
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
;; remainder is called 4 times


;;




;; ex-1.21

;;
