(define test-vec-1 (list 1 2 3))
(define test-vec-2 (list 4 5 6))

(define test-mat-1 (list test-vec-1 test-vec-2))
(define test-mat-2 (list test-vec-2 test-vec-1))

(define test (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(define (accumulate op init sequence)
  (if (null? sequence) init
    (op (car sequence) (accumulate op init (cdr sequence)))))

(define (accumulate-n op init seqs)
  (cond
    ((null? seqs) seqs)
    ((null? (car seqs)) (car seqs))
    (else (cons
            (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs))))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;

(define (fold-right op init sequence)
  (if (null? sequence) init
    (op (car sequence) 
        (fold-right op init (cdr sequence)))))

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest) result
      (iter (op result (car rest)) (cdr rest))))
  (iter init sequence))

(fold-right / 1 (list 1 2 3)) ;; 1.5
(fold-left / 1 (list 1 2 3)) ;; .16667
(fold-right list nil (list 1 2 3)) ;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ;; (((() 1) 2) 3)

;; set op to + anything that order doesn't matter.

;; test be able to reproduce these
;; Study cons more understand how it compares to append
;; Does cons only work well for prepending values?  use recursive nature to create append?

(define (append list1 list2)
  (if (null? list1) list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x)) (list) sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))
