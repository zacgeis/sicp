; Use a global 2d association table
(define put 2d-put!)

; 2d-get doesn't work because it uses eq? (Is this an issue with '(list))
 (define (get x-key y-key) 
   (let ((1d-table (2d-get-alist-x x-key)))
       (let ((type-f (assoc y-key 1d-table)))
             (if type-f (cdr type-f) false))))

(define (apply-type-tag type l)
  (cons type l))

(define (type-tag l)
  (car l))

(define (type-contents l)
  (cdr l))

;; add error handling to this
(define (apply-generic op . args)
  (let ((typed-args (map type-tag args)))
    (let ((proc (get op typed-args)))
      (apply proc (map type-contents args)))))

(put 'add '(number number) (lambda (a b) (+ a b)))

(apply-generic 'add (apply-type-tag 'number 1) (apply-type-tag 'number 2))

