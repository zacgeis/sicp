; Streams are delayed lists

; the head of the stream is an actual value
; the tail of the stream is a delayed computation

; "demand driven programming"

; (define (delay1 expression) -- requires special form to avoid eager evaluation of params
;   (lambda () expression))
(define-syntax delay1
  (syntax-rules ()
    ((delay1 expression)
     (lambda () expression))))

(define (force1 expression)
  (expression))

; (define (cons-stream a b) (cons a (delay1 b))) -- requires special form
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
    (cons a (delay1 b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force1 (cdr stream)))

(define the-empty-stream '())
(define (stream-null? x) (null? x))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define one-to-ten-test (stream-enumerate-interval 1 10))

(display-stream one-to-ten-test)

; 3.50

(define (stream-map1 proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map1 (cons proc (map stream-cdr argstreams))))))

; 3.51

(define (show x)
  (display-line x) x)

(define (stream-ref s x)
  (if (= x 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- x 1))))

(define sample (stream-map show (stream-enumerate-interval 0 10)))

; 3.52

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; since sum is mutating state, if memo is used, the state will only update once per index
; if memo is not used, it will result it the state mutation multiple times per item

; map doesn't actually evaluate the rest of the stream. it adds an additional function before the delay step

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisble? a b) (= (modulo a b) 0))
; stream of primes. similar to signal processing
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve
      (stream-filter
        (lambda (value)
          (not (divisble? value (stream-car stream)))) (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(newline)
(display "prime: ")
(display (stream-ref primes 8))
(newline)

(define (add-streams s1 s2)
  (stream-map1 + s1 s2))

; 3.53

; 1 2 4 8 16
(define test (cons-stream 1 (add-streams test test)))

; 3.54

(define ones (cons-stream 1 ones))

(define (mul-streams s1 s2)
  (stream-map1 * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

; 3.55

(define (partial-sums stream)
  (cons-stream
    (stream-car stream)
    (add-streams (partial-sums stream) (stream-cdr stream))))

(define partial-sums-test (partial-sums integers))

; 3.56

(define S
  (cons-stream 1
    (merge (scale-stream S 2)
      (merge (scale-stream S 3) (scale-stream S 5)))))

; 3.57

; if memo-proc is not used, each stream-car will exponentially call itself down the stack

; 3.58

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

; 3.64

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (stream-limit stream tolerance)
  (let ((tail (stream-cdr stream)) (head (stream-car stream)))
    (let ((tail-head (stream-car tail)))
      (if (< (abs (- head tail-head)) tolerance) tail-head
        (stream-limit tail tolerance)))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


