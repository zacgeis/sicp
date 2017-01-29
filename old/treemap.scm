(define list1 (list 1 2 3 4 5))
(define tree1 (list 1 (list 2 3) (list 4 (list 5))))

(define (square x) (* x x))

;; Layout execution plan for this.
(define (map f l)
  (if (null? l) l
    (cons (f (car l)) (map f (cdr l)))
  )
)

(define (tree-square t)
  (cond
    ((null? t) t)
    ((not (pair? t)) (* t t))
    (else (cons (tree-square (car t)) (tree-square (cdr t))))
  )
)

(define (tree-square-with-map t)
  (map
    (lambda (x)
      (if (pair? x)
        (tree-square-with-map x)
        (square x)
      )
    ) t
  )
)

(define (map-tree f t)
  (map (lambda (x) (if (pair? x) (map-tree f x) (f x))) t)
)

(map square list1)
(tree-square tree1)
(tree-square-with-map tree1)
(map-tree square tree1)
