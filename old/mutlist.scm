(define (count-pairs l)
  (if (not (pair? l))
    0
    (+ (count-pairs (car l))
       (count-pairs (cdr l))
       1)))

(define (contains-ref l ref)
  (cond ((null? l) #f)
        ((eq? (car l) ref) #t)
        (else (contains-ref (cdr l) ref))))
(define (unique-count l)
  (let ((refs '()))
    (define (count-pairs-ref l)
      (if (or (not (pair? l)) (contains-ref refs l))
        0
        (begin 
           (set! refs (cons l refs))
           (+ (count-pairs-ref (car l))
           (count-pairs-ref (cdr l))
           1))))
    (count-pairs-ref l)))

(define list1 (list 1))
(define list2 (list list1))
(define list3 (list list2))

(count-pairs (cons list1 list1))
(count-pairs (cons list1 list2))
(count-pairs (cons list3 list3))

(unique-count (cons list3 list3))

(define (loop-detector l)
  (let ((visited '()))
    (define (visit l)
      (cond ((null? l) #f)
            ((contains-ref visited (car l)) #t)
            (else
              (set! visited (cons (car l) visited))
              (visit (cdr l)))))
    (visit l)))

(define (fast-loop-detector l)
  (let ((turtle (cdr l)) (rabbit (cddr l)))
    (define (step)
      (cond 
        ((or (null? rabbit) (null? turtle)) #f)
        ((eq? (car rabbit) (car turtle)) #t)
        (else (set! turtle (cdr turtle))
              (set! rabbit (cddr rabbit))
              (step))))
    (step)))


(define list-loop (list 1 2 3 4 5 6))
(set-cdr! (cddddr list-loop) list-loop)
(define list-noloop (list 1 2 3 4 5 6))

(loop-detector list-loop)
(loop-detector list-noloop)

(fast-loop-detector list-noloop)
(fast-loop-detector list-loop)

(define three '(3))
(define nums (list 1 2 three 4 5))
(define two (cadr nums))
two
(set-car! (cdr nums) 3)
two
nums
(set-car! three '(4))
three
nums

(define str "Dog")
(define list-string (list "test" str "test"))
list-string
(string-set! str 0 #\L)
str     
list-string

(define x 4)
(define (add a) (+ a x))
(add 5)
(set! x 2)
(add 5)
