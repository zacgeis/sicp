(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? a b) (> tolerance (abs (- a b))))
  (define (try-guess guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (cond
        ((close-enough? guess next) guess)
        ;;(else (try-guess next))
        (else (try-guess (* 0.5 (+ next guess))))
      )
    )
  )
  (try-guess first-guess)
)
;; x^2 = x + 1
;; x = (x + 1) / x
;; x = (1 + (1 / x))
;;(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; can't start with log(1) because zero division
;;(fixed-point (lambda (x) (/ (log 1000) (log x)) ) 1.1)

;; x = 1/x
(define (cont-frac n d k) (cond
  ((= k 0) (/(n 0) (n d)))
  (else (/ (n k) (+ (d k) (cont-frac n d (- k 1)))))
))

(define (cont-frac-iter n d k)
  (define (iter a result)
    (cond
      ((= a 0.0) result)
      (else (iter (- a 1.0) (/ (n a) (+ (d a) result))))
    )
  )
  (iter k 0.0)
)

;; write a scheme syntax highlighter for atom.io
;; (/ 1 (cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 100))

(define (seq i) (let ((a (- i 3)))
  (cond
    ((= (remainder a 3) 0) (+ 2 (* 2 (/ a 3))))
    (else 1)
  )
))
;;(cont-frac-iter (lambda (i) 1.0) seq 10000)


(define (tan-cf x k)
  (define (tan-cf-n i) (cond ((= i 1.0) x) (else (- (square x))) ))
  (define (tan-cf-d i) (- (* i 2.0) 1))
  (cont-frac-iter tan-cf-n tan-cf-d k)
)

(tan-cf 2 1000)
(tan-cf 1 1000)
