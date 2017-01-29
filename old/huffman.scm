(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;(decode sample-message sample-tree) ;; action

(define (filter func l)
  (cond ((null? l) '())
        ((func (car l)) (cons (car l) (filter func (cdr l))))
        (else (filter func (cdr l)))))

(define (encode-symbol symbol tree)
  (cdar (filter 
    (lambda (encoding) (eq? (car encoding) symbol)) 
    (encode-list tree))))

(define (encode-list tree)
  (define (encode-list1 value tree)
    (cond ((null? tree) '())
          ((leaf? tree) (list (cons (symbol-leaf tree) value)))
          (else (append
                  (encode-list1 (append value '(0)) (left-branch tree))
                  (encode-list1 (append value '(1)) (right-branch tree))))))
  (encode-list1 '() tree))

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

;;(encode '(a d a b b c a) sample-tree) ;; action

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ;symbol
                               (cadr pair)) ;frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (define (successive-merge-1 l t)
    (if (null? l) t (successive-merge-1 (cdr l) (make-code-tree (car l) t))))
  (successive-merge-1 (cdr leaves) (car leaves)))

(define test-list (list (list 'A 4) (list 'B 2) (list 'D 1) (list 'C 1)))

(make-leaf-set test-list)
(generate-huffman-tree test-list)
sample-tree

(define lyric-list '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define lyric-tree (generate-huffman-tree lyric-list))

(encode '(Get a job) lyric-tree)
(encode '(Sha na na na na na na na na) lyric-tree)

  



