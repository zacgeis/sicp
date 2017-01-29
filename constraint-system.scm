(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Uknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier a1 a2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? a1) (= (get-value a1) 0))
               (and (has-value? a2) (= (get-value a2) 0)))
           (set-value! product 0 me))
          ((and (has-value? a1) (has-value? a2))
           (set-value! product
                       (* (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? product))
           (set-value! a2
                       (/ (get-value product) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? product))
           (set-value! a1
                       (/ (get-value product) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Uknown request -- MULTIPLIER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Uknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
            (error "Uknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value #f) (informant #f) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? refactor informant)
          (begin (set! informant #f)
                 (for-each-except refactor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Uknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except source procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) source) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector refactor)
  ((connector 'forget) refactor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
(newline)
(newline)

;; 3.33

(define (averager a b c)
  (let ((two (make-connector))
        (a-plus-b (make-connector)))
    (constant 2 two)
    (adder a b a-plus-b)
    (multiplier two c a-plus-b)))

(define average-value-1 (make-connector))
(define average-value-2 (make-connector))
(define average-value-3 (make-connector))

(averager average-value-1 average-value-2 average-value-3)

;; 3.34

;; multiplier requires at least two inputs to propogate. just having a product is not enough.

;; 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
          (error "square less than 0 -- SQUARER" (get-value b))
          (set-value! a (sqrt (get-value b)) me))
      (set-value! b (* (get-value a) (get-value a)) me)))
    (define (process-forget-value)
      (forget-value! a me)
      (forget-value! b me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
              (error "Uknown request -- SQUARER" request))))
    (connect a me)
    (connect b me)
    me)

(define squarer-value-1 (make-connector))
(define squarer-value-2 (make-connector))

(squarer squarer-value-1 squarer-value-2)

;; 3.36

;; there will exist no constraints at time of execution 

;; 3.37

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z x y)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier x z y)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
