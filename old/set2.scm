(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-set (cdr set1)
                                     (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; Should be O(n)
(define (union-set set1 set2)
  (if (null? set1)
    set2
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (union-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (cons x1 
                   (union-set (cdr set1) set2)))
            ((< x2 x1)
             (cons x2 
                   (union-set set1 (cdr set2))))))))

(define test1 (list 1 3 4 5))
(define test2 (list 1 2 4 7))

(intersection-set test1 test2)

(adjoin-set 3 test2)
(adjoin-set 2 test2)
(adjoin-set 8 test2)

(union-set test1 test2)
