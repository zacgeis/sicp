(define nil (list))

(define (enumerate-interval a b)
  (if (> a b) nil
    (cons a (enumerate-interval (+ a 1) b))))

(define (accumulate op init seq)
  (if (null? seq) init
    (op (car seq) (accumulate op init (cdr seq)))))

(define (flatmap f seq)
  (accumulate append nil (map f seq)))

(define (unique-pairs n)
  (flatmap 
    (lambda (i) 
      (map 
        (lambda (j) (list i j)) 
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 10)

(define (make-position row col) (cons row col))
(define (position-row pos) (car pos))
(define (position-col pos) (cdr pos))

(define (attacks? q1 q2)
  (or (= (position-row q1) (position-row q2))
      (= (abs (- (position-row q1) (position-row q2)))
         (abs (- (position-col q1) (position-col q2))))))

(define (attacks-iter q board)
  (or (null? board)
      (and (not (attacks? q (car board)))
           (attacks-iter q (cdr board)))))

(define (safe? col positions)
  (let ((queen (list-ref positions (- col 1))) 
        (others (filter (lambda (q) 
                          (not (= col (position-col q)))) positions)))
    (attacks-iter queen others)))

(define (queens board-size)
  (define empty-board (list))
  (define (adjoin-position row col positions)
    (append positions (list (make-position row col))))
  (define (queens-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queens-cols (- k 1))))))
  (queens-cols board-size))

(queens 8)
