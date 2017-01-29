(define (square x) (* x x))

(define list1 (list 1 2 3 4 5 6 7 8 9))

(define (filter predicate sequence)
  (cond
    ((null? sequence) sequence)
    ((predicate (car sequence)) (cons (car sequence) (filter predicate (cdr sequence))))
    (else (filter predicate (cdr sequence)))))

(filter odd? list1)

(define (accumulate op initial sequence)
  (if (null? sequence) initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate + 0 list1)

;; define canocial and enumerate
;; could also be named 'range'
(define (enumerate-interval low high)
  (if (> low high) (list)
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 9)

(define (map-alt p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) (list) sequence))

(define (append-alt seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-alt sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length-alt list1)
(map-alt square list1)
(append-alt list1 (list 10 11 12))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; interesting definition
(define (count-leaves t)
  (accumulate + 0 (map (lambda (s) (if (pair? s) (count-leaves s) 1)) t)))

(count-leaves (list 1 2 (list 1 2 3) 4))

(define listn (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;; Email nicole back regarding course response story

(define (accumulate-n op init seqs)
  (cond
    ((null? seqs) seqs)
    ((null? (car seqs)) (car seqs))
    (else (cons (accumulate op init (map car seqs))
                (accumulate-n op init (map cdr seqs))))))

(accumulate-n + 0 listn)
