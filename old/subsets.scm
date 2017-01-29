(define list1 (list 1 2 3))

(define (append l1 l2)
  (if (null? l1) l2
    (cons (car l1) (append (cdr l1) l2))
  )
)

(define (square x) (list x))

;;(define (subsets s)
;;  (if (null? s) (list s)
;;    (let ((rest (subsets (cdr s))))
;;      (append rest
;;        (map
;;          (lambda (x)
;;           (list 'z s x))rest)))))

(define (subsets s)
 (define (step x) (append (list (car s)) x))
 (if (null? s) 
   (list s)
   (let ((rest (subsets (cdr s))))
     (append rest (map step rest)))))

(subsets list1)

;; Reference powersets http://en.wikipedia.org/wiki/Powerset#Algorithms
