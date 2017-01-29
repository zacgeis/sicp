;; Every one buys an additional two. spaces...
;; How does the number length relate to this..
;; Worst case is single numbers...
;; If this was a larger 3 num we could center it to buy more space...

                            2
                           / \ 
                          /   \
                         /     \
                        /       \           
                       /         \
                      /           \
                     /             \
                    3               4
                   / \             / \
                  /   \           /   \
                 /     \         /     \
                5       6       7       8
               / \     / \     / \     / \
              /   \   /   \   /   \   /   \
             9    10 11   12 13   14 15   16



(define (number-length n)
  (if (< (/ n 10) 1) 1
    (+ (number-length (/ n 10)) 1)))
(define (spaces n)
  (list->string (spaces-list n)))
(define (spaces-list a)
  (if (= a 0) '()
    (cons #\space (spaces-list (- a 1)))))

;; possibly make this into iter
(define (tree-max-depth tree left right)
  (cond
    ((and (null? (right-branch tree)) (null? (left-branch tree))) 0)
    ((null? (right-branch tree)) (+ left (tree-max-depth (left-branch tree) left right)))
    ((null? (left-branch tree)) (+ right (tree-max-depth (right-branch tree) left right)))
    (else (max (+ left (tree-max-depth (left-branch tree) left right)) (+ right (tree-max-depth (right-branch tree) left right)) 0))))

(define (tree-max-depth-left tree)
  (tree-max-depth tree 1 -1))

(define (tree-max-depth-right tree)
  (tree-max-depth tree -1 1))

(define (tree-width tree)
  (+ (tree-max-depth-right tree) (tree-max-depth-left tree)))

;;(tree-max-depth-left '(2 (1 (2 () ()) ()) (2 (2 (2 (2 (2 (2 (2 (2 () ()) ()) ()) ()) ()) ()) ()) ())))
;;(tree-max-depth-right '(2 (1 (2 () ()) ()) (2 (2 (2 (2 (2 (2 (2 (2 () ()) ()) ()) ()) ()) ()) ()) (1 () ()))))

(tree-width '(2 (1 (2 () ()) ()) (2 (2 (2 (2 (2 (2 (2 (2 () ()) ()) ()) ()) ()) ()) ()) (1 () ()))))


