(define (add a b) (+ a b))

(define (next-even a)
  (cond
    ((even? a) a)
    (else (+ a 1))
  )
)

(next-even 1)
