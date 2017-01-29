(define list1 (list 1 2 3))
(define list2 (list 4 5 6))
(define tree1 (list 1 2 3 (list 4 5 6) 10 (list 7 8 9)))

(define (list-append l1 l2)
  (if (null? l1) l2
    (cons (car l1) (list-append (cdr l1) l2))
  )
)

(define (list-reverse l1)
  (if (null? l1) l1
    (list-append (list-reverse (cdr l1)) (list (car l1)))
  )
)

(define (tree-reverse l1)
  (cond
    ((null? l1) l1)
    ((pair? (car l1)) (list-append (tree-reverse (cdr l1)) (list (tree-reverse (car l1)))))
    (else (list-append (tree-reverse (cdr l1)) (list (car l1))))
  )
)

(tree-reverse tree1)

(define (fringe t1)
  (cond
    ((null? t1) t1)
    ((pair? (car t1)) (list-append (fringe (car t1)) (fringe (cdr t1))))
    (else (list-append (list (car t1)) (fringe (cdr t1))))
  )
)

(fringe tree1)
