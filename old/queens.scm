(define nil (list))

(define (enumerate-interval a b)
  (if (> a b) nil
    (cons a (enumerate-interval (+ a 1) b))))

(define (unique-pairs n)
  (flatmap (lambda (i) 
         (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
  (enumerate-interval 1 n)))

(define (remove item seq)
  (filter (lambda (i) (not (= i item))) seq))

(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define test (list 1 2 3 4 5))

;; works its way down by finding permutations of smaller sets
;; (1 2)
;; (1)
;; ()
;;
;; (1 2 3)
(define (permutations seq)
  (if (null? seq)
    (list nil)
    (flatmap 
      (lambda (i)
        (map 
          (lambda (p) (cons i p)) 
          (permutations (remove i seq))))
      seq)))

(permutations (list 1 2 3))
(permutations (list 1 2))

(define (sum-eq? seq v)
  (= (accumulate + 0 seq) v))

(define (unique-triples n)
  (flatmap (lambda (i) 
      (map (lambda (j) (append (list i) j))
           (unique-pairs (- i 1))))
    (enumerate-interval 1 n)))

(define (find-unique-triples n s)
  (filter (lambda (i) (sum-eq? i s)) (unique-triples n)))

(unique-triples 5)
(find-unique-triples 5 8)
(find-unique-triples 12 12)

;; queens


