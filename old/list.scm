(define (list-ref l n)
  (cond
    ((= n 0) (car l))
    (else (list-ref (cdr l) (- n 1)))
  )
)

(define (list-length l)
  (if (null? l) 0
    (+ 1 (list-length (cdr l)))
  )
)

(define (list-length-alt items)
  (define (list-length-alt-iter l count)
    (if (null? l)
      count
      (list-length-alt-iter (cdr l) (+ count 1))
    )
  )
  (list-length-alt-iter items 0)
)

(define (list-append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (list-append (cdr list1) list2))
  )
)

(define (list-last-pair l)
  (if (null? (cdr l)) (car l) (list-last-pair (cdr l)))
)

;;(define (list-reverse l)
;;  (if (null? (cdr (car l)))
;;    l
;;    (cons)(car (car l))
;;  )
;;)

(define (list-reverse l)
  (if (null? l)
    l ;; (list)
    (list-append (list-reverse (cdr l)) (list (car l)))
  )
)
;; include emails in the files

;;(2 4 6 8 10)
;;((4 6 8 10) 2)
;;((6 8 10) 4 2)
;;((8 10) 6 4 2)
;;((10) 8 6 4 2)
;;(10 8 6 4 2)

(define test (list 2 4 6 8 10))
(define test2 (list 1 3 5 7 9))

(list-ref test 0)
(list-ref test 1)
(list-ref test 4)

(list-length test)
(list-length-alt test)

(list-append test test2)

(list-last-pair test)

(list-reverse test)

(define (cc amount coin-values)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (null? coin-values)) 0)
    (else
      (+
        (cc amount (cdr coin-values))
        (cc (- amount (car coin-values)) coin-values)
      )
    )
  )
)

(define us-coins (list 50 25 10 5 1))

(cc 100 us-coins)

(define (list-scale l factor)
  (if (null? l) l
    (cons
      (* factor (car l))
      (list-scale (cdr l) factor)
    )
  )
)

(list-scale us-coins 2)

(define (map l f)
  (if (null? l) l
    (cons
      (f (car l))
      (map (cdr l) f)
    )
  )
)

(define (list-scale-alt l factor)
  (map l (lambda (i) (* i factor)))
)

(list-scale-alt us-coins 2)

(define (for-each l f)
  (cond
    ((null? l) true)
    (else (f (car l)) (for-each (cdr l) f))
  )
)

(for-each (list 1 2 3 4) (lambda (i) (newline) (display i)))

(define tree (list (list 1 2) (list 3 (list 4 5))))

(define (tree-length t)
  (cond
    ((null? t) 0)
    ((not (pair? t)) 1)
    (else (+ (tree-length (car t)) (tree-length (cdr t))))
  )
)

(list-length tree)
(tree-length tree)

(define tree1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr tree1)))))

(define tree2 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr tree2))))))))))))

;; 1 2 3 4 5 6
;; (1 2 3) 4 5 6
;; (1 2 3) (4 5 6)
