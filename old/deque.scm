;all operations should be accomplished in O(1) steps

;constructor
(define (make-deque)
  (cons '() '()))

;selectors
(define (empty-deque? deque)
  (null? (car deque)))
(define (front-deque deque)
  (caar deque))
(define (rear-deque deque)
  (cadr deque))

;create selectors for items
(define (make-item data) (list data '() '()))

(define (prev-item item)
  (car (cdr item)))
(define (next-item item)
  (car (cdr (cdr item))))
(define (data-item item)
  (car item))

(define (set-prev-item! item prev)
  (set-car! (cdr item) prev))
(define (set-next-item! item next)
  (set-car! (cdr (cdr item)) next))

;mutators
(define (front-insert-deque! deque data)
  (let ((new-item (make-item data)))
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           deque)
          (else
            (set-prev-item! (car deque) new-item)
            (set-next-item! new-item (car deque))
            (set-car! deque new-item)))))
(define (rear-insert-deque! deque data)
  (let ((new-item (make-item data)))
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           deque)
          (else
            (set-next-item! (cdr deque) new-item)
            (set-prev-item! new-item (cdr deque))
            (set-cdr! deque new-item)))))
(define (front-delete-deque! deque)
  (set-car! deque (next-item (car deque))))
(define (rear-delete-deque! deque)
  (set-cdr! deque (prev-item (cdr deque))))

(define q (make-deque))
(front-insert-deque! q 'b)

(front-deque q)

(front-insert-deque! q 'a)

(front-deque q)

(rear-deque q)

(rear-insert-deque! q 'c)

(rear-deque q)

(rear-delete-deque! q)

(rear-deque q)

(rear-delete-deque! q)

(rear-deque q)
(front-deque q)

