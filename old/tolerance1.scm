
(define tolerance 0.0001)

;; Should be when x = f(x) within tolerance.
;; y^2 = x => y = x/y
;; fix mlet to not have a last catch
;; Check this to see if it is correct.
(define (fixed-point f guess)
  (define (close-enough? x y) (< (abs (- x y)) tolerance))
  (let ((next (f guess)))
    (cond
      ((close-enough? next guess) guess)
      (else (fixed-point f next))
    )
  )
)

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x)))
)

;;((average-damp square) 10)

(define (sqrt x)
  ;;(fixed-point (lambda (y) (/ x y)) 1.0)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

;;(sqrt 100)

(define (curt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
)

;;(curt 1000)

(define dx 0.0001)
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))
)

;;(define (cube x) (* x x x))
;;((deriv cube) 5)

(define (newton-transform f)
  (lambda (x) (- x (/ (f x) ((deriv f) x))))
)
(define (newtons-method f guess)
  (fixed-point (newton-transform f) guess)
)

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0)
)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess)
)

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0)
)

;;(sqrt 100)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

(define (cubic-zeros a b c)
  (newtons-method (cubic a b c) 1.0)
)

;;(cubic-zeros 3 -2.4 6)

(define (double f)
  (lambda (x) (f (f x)))
)

(define (inc x) (+ x 1))

;;(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x)))
)

((compose square inc) 6)

(define (repeated f n)
  (lambda (x)
    (define (recur a)
      (cond
        ((= a n) (f x))
        (else (f (recur (+ 1 a))))
      )
    )
    (recur 1)
  )
)

(define (repeated f x)
  (cond
    ((= x 1) f)
    (else (compose f (repeated f (- x 1))))
  )
)

;;((repeated square 2) 5)

(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))
)
(define (repeated-smooth f n) (repeated (smooth f) n))


(define (pow x p)
  (cond
    ((= p 0) 1)
    (else (* x (pow x (- p 1))))
  )
)
(define (next-even x)
  (cond
    ((even? x) x)
    (else (+ x 1))
  )
)
(define (nth-root r x)
  (fixed-point ((repeated average-damp 4) (lambda (y) (/ x (pow y (- r 1))))) 1.0)
)

;;(nth-root 40 (pow 10 40))
;; 2=1 4=2 8=3 16=4

(define (iterative-improve good-enough? next-guess)
  (define (try-guess guess)
    (cond
      ((good-enough? guess) guess)
      (else (try-guess (next-guess guess)))
    )
  )
  try-guess
)

;;3 ^ 5 = 243
;;5th root of 243 = 3
;;log (243) / log (3) = 5
