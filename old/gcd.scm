(define (gcd a b)
  (cond
    ((= b 0) (abs a))
    (else (gcd b (remainder a b)))
  )
)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond
      ((> d 0) (cons (/ n g) (/ d g)))
      (else (cons (/ (* -1 n) g) (/ (* -1 d) g)))
    )
  )
)

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

(define one-half (make-rat -5 -10))

(print-rat one-half)
