(define (cube x) (* x x x))

(define (inc a) (+ a 1))

(define (sum term next a b) (cond
  ((> a b) 0)
  (else (+ (term a) (sum term next (next a) b)))
))

;;(sum cube inc 1 10)

(define (sum-of-cubes a b) (sum cube inc a b))

(sum-of-cubes 1 10)

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (* 8 (sum pi-term pi-next a b))
)

(pi-sum 1 1000)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b) dx)
)

(integral cube 0 1 0.001)

(define (simp-int f a b n)
  (define h (/ (- b a) n))
  (define (harmonic x) (cond
    ((= x 0) 1)
    ((= x n) 1)
    ((even? x) 2)
    (else 4)
  ))
  (define (y k) (* (harmonic k) (f (+ a (* k h)))))
  (* (sum y inc 0 n) (/ h 3))
)

(simp-int cube 0 1 1000)

(define (sum-iter term a next b)
  (define (iter a result) (cond
    ((> a b) result)
    (else (iter (next a) (+ result (term a))))
  ))
  (iter a 0)
)

(sum-iter cube 1 inc 10)

(define (sum-iter-product term a next b)
  (define (iter a result) (cond
    ((> a b) result)
    (else (iter (next a) (* result (term a))))
  ))
  (iter a 1)
)

(define (sum-product term a next b) (cond
  ((> a b) 1)
  (else (* (term a) (sum-product term (next a) next b)))
))

(define (same x) x)
(define (factorial x) (sum-iter-product same 1 inc x))
(factorial 5)

(define (calc-pi n)
  (define (pi-term x) (cond
    ((even? x) (/ (+ x 2) (+ x 1)))
    (else (/ (+ x 1) (+ x 2)))
  ))
  (* 4.0 (sum-product pi-term 1 inc n)) ;; 4 vs 4.0
)

(calc-pi 1000)


;;

(define (accumulate combiner null-value term a next b) (cond
  ((> a b) null-value)
  (else (combiner (term a) (accumulate combiner null-value term (next a) next b)))
))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result) (cond
    ((> a b) result)
    (else (iter (next a) (combiner (term a) result)))
  ))
  (iter a null-value)
)

(define (sum-accumulate term a next b)
  (accumulate-iter + 0 term a next b)
)

(sum-accumulate cube 1 inc 10)

;;



(define (filtered-accumulate combiner filter? null-value term a next b)
  (cond
    ((> a b) null-value)
    ((filter? a) (combiner (term a) (accumulate combiner null-value term (next a) next b)))
    (else (accumulate combiner null-value term (next a) next b))
))


;;
