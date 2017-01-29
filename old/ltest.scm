(define (make-interval a b) (cons a b))
;;(define (lower-bound x) (- (car x) (* (cdr x) (car x))))
;;(define (upper-bound x) (+ (car x) (* (cdr x) (car x))))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval a b)
  (make-interval
    (+ (lower-bound a) (lower-bound b))
    (+ (upper-bound a) (upper-bound b))
  )
)

(define (sub-interval a b)
  (make-interval
    (- (lower-bound a) (upper-bound b))
    (- (upper-bound a) (lower-bound b))
  )
)

(define (mul-interval a b)
  (let
    (
      (p1 (* (lower-bound a) (lower-bound b)))
      (p2 (* (lower-bound a) (upper-bound b)))
      (p3 (* (lower-bound b) (upper-bound a)))
      (p4 (* (upper-bound b) (upper-bound a)))
    )
    (make-interval
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4)
    )
  )
)

(define (spans-zero? a)
  (and
    (<= (lower-bound a) 0)
    (>= (upper-bound a) 0)
  )
)

(define (div-interval a b)
  (if (spans-zero? b)
    (error "[Error] spans zero")
    (mul-interval a
      (make-interval
        (/ 1.0 (upper-bound b))
        (/ 1.0 (lower-bound b))
      )
    )
  )
)

(define (width-interval a)
  (/ (- (upper-bound a) (lower-bound a)) 2.0)
)

(define test (make-interval 1 4))
(define test2 (make-interval 5 6))

(+ (width-interval test) (width-interval test2))
(width-interval (add-interval test test2))

(* (width-interval test) (width-interval test2))
(width-interval (mul-interval test test2))

(define test3 (make-interval -5 5))
(div-interval test test3)

(define (find-center a)
  (let ((s (/ (+ (lower-bound a) (upper-bound a)) 2.0)))
    (make-interval s (- (upper-bound a) s))
  )
)

(find-center test3)
