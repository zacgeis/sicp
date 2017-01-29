; Use a global 2d association table
(define put 2d-put!)

; 2d-get doesn't work because it uses eq? (Is this an issue with '(list))
 (define (get x-key y-key) 
   (let ((1d-table (2d-get-alist-x x-key)))
       (let ((type-f (assoc y-key 1d-table)))
             (if type-f (cdr type-f) false))))


(define (install-polynomial-package)
  (define (eq-list? l1 l2)
    (cond ((and (null? l1) (null? l2)) true)
      (else (and (eq? (car l1) (car l2))
        (eq-list? (cdr l1) (cdr l2))))))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable poly)
    (car poly))
  (define (term-list poly)
    (cdr poly))
  (define (same-variable? p1 p2)
    (eq-list? p1 p2))
  (define (add-poly p1 p2)
    (if (eq? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
