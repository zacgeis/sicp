(define (make-queue) (cons '() '()))

(define (make-queue-2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (front)
      (if (empty?)
        (error "FRONT called with an empty queue")
        (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue"))
            (else
              (set-front-ptr! (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)))
    dispatch))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else 
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define q (make-queue))

(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)

(delete-queue! q)

(define (print-queue queue)
  (front-ptr queue))

(print-queue q)

(define q2 (make-queue-2))
((q2 'insert!) 'a)
((q2 'insert!) 'b)
((q2 'insert!) 'c)

((q2 'front))
((q2 'delete!))
((q2 'front))

