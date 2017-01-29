(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0) (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts) right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;;2.65
;;(define (union-set tree1 tree2))
;;(define (intersection-set tree1 tree2))

(define tree1 (list->tree (list 1 3 5 7 9 11)))
(define tree2 (list->tree (list 1 2 4 7 8 10)))

(define (union-set tree1 tree2)
  (cond ((null? tree1) tree2)
        ((null? tree2) tree1)
        ((= (entry tree1) (entry tree2))
         (make-tree 
           (entry tree1)
           (union-set (left-branch tree1) (left-branch tree2))
           (union-set (right-branch tree1) (right-branch tree2))))
        ((< (entry tree1) (entry tree2))
         (make-tree
           (entry tree2)
           (union-set (make-tree (entry tree1) (left-branch tree1) '()) (left-branch tree2))
           (union-set (right-branch tree1) (right-branch tree2))))
        ((> (entry tree1) (entry tree2))
         (make-tree
           (entry tree1)
           (union-set (make-tree (entry tree2) (left-branch tree2) '()) (left-branch tree1))
           (union-set (right-branch tree2) (right-branch tree1))))))
(union-set tree1 tree2)

(define (intersection-set-old tree1 tree2)
  (cond ((null? tree1) '())
        ((null? tree2) '())
        ((= (entry tree1) (entry tree2))
         (make-tree
           (entry tree1)
           (intersection-set (left-branch tree1) (left-branch tree2))
           (intersection-set (right-branch tree1) (right-branch tree2))))
        ((< (entry tree1) (entry tree2))
           (intersection-set
             (make-tree 
               (entry tree1) 
               (left-branch tree1)
               (intersection-set
                 (make-tree
                   (entry tree2)
                   '()
                   (right-branch tree2))
                 (right-branch tree1)))
             (left-branch tree2)))
        ((> (entry tree1) (entry tree2))
         (make-tree '()
           (intersection-set (right-branch tree2) tree1)
           (intersection-set
             (make-tree
               (entry tree2)
               (left-branch tree2)
               '())
             (left-branch tree1))))))

(define (intersection-set-old2 tree1 tree2)
  (cond ((null? tree1) '())
        ((null? tree2) '())
        ((= (entry tree1) (entry tree2))
         (make-tree
           (entry tree1)
           (intersection-set (left-branch tree1) (left-branch tree2))
           (intersection-set (right-branch tree1) (right-branch tree2))))
        ((< (entry tree1) (entry tree2))
         (intersection-set tree2 tree1))
        ((> (entry tree1) (entry tree2))
         (make-tree '()
           (intersection-set (right-branch tree2) tree1)
           (intersection-set
             (make-tree
               (entry tree2)
               (left-branch tree2)
               '())
             (left-branch tree1))))))

(define (intersection-set tree1 tree2)
  (define (merge-tree tree1 tree2 result-tree)
    (cond ((null? tree1) result-tree)
          ((null? tree2) result-tree)
          ((= (entry tree1) (entry tree2))
           (make-tree
             (entry tree1)
             (merge-tree (left-branch tree1) (left-branch tree2) result-tree)
             (merge-tree (right-branch tree1) (right-branch tree2) result-tree)))
          ((< (entry tree1) (entry tree2))
           (merge-tree tree2 tree1 result-tree))
          ((> (entry tree1) (entry tree2))
            (merge-tree 
              (right-branch tree2) 
              tree1
               (merge-tree
                 (make-tree
                   (entry tree2)
                   (left-branch tree2)
                   '())
                 (left-branch tree1) result-tree)))))
  (merge-tree tree1 tree2 '()))

(define tree3 (list->tree (list 1 3 5 7 9 11)))
(define tree4 (list->tree (list 10 11 12 13 14 15)))
(union-set tree1 tree2)
(union-set tree3 tree4)

;2.66
(define (lookup-set x set)
  (cond ((null? set) false)
        ((= x (entry set)) (entry set))
        ((< x (entry set))
         (lookup-set x (left-branch set)))
        ((> x (entry set))
         (lookup-set x (right-branch set)))))
