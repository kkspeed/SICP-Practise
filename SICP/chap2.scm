(load "utils.scm")

(define (make-rat n d)
  (define n1 (abs n))
  (define d1 (abs d))
  (define g (gcd n1 d1))
  (define n2 (if (< (* n d) 0)
                 (neg n1)
                 n1))
  (cons (/ n2 g) (/ d1 g)))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (print-point p)
  (display '<)
  (display (x-point p))
  (display '-)
  (display (y-point p))
  (display '>)
  (newline))

(define (make-line p1 p2)
  (cons p1 p2))


(define line-start car)
(define line-end cdr)

(define (mid-point l)
  (define p1 (line-start l))
  (define p2 (line-end l))
  (make-point (average (x-point p1)
                       (x-point p2))
              (average (y-point p1)
                       (y-point p2))))

(define (my-cons x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y))))
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

(define (my-cons2 x y)
  (lambda (m) (m x y)))

(define (my-car2 z)
  (z (lambda (p q) p)))

(define (my-cdr2 z)
  (z (lambda (p q) q)))

(define (my-cons3 x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (my-pos p m)
  (if (/= (mod m p) 0)
      0
      (+ 1 (my-pos p (/ m p)))))

(define (my-car3 m)
  (my-pos 2 m))

(define (my-cdr3 m)
  (my-pos 3 m))

(define (same-parity . lst)
  (define (do-parity p lst)
    (if (null? lst)
        '()
        (if (= (mod (car lst) 2) p)
            (cons (car lst) (do-parity p (cdr lst)))
            (do-parity p (cdr lst)))))
  (do-parity (mod (car lst) 2) lst))

(define (count-leaves x)
  (cond
   ((null? x) 0)
   ((not (pair? x)) 1)
   (#t (+ (count-leaves (car x))
          (count-leaves (cdr x))))))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((not (pair? x)) x)
        (#t (append (deep-reverse (cdr x))
                    (list (deep-reverse (car x)))))))

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (#t (append (fringe (car x))
                    (fringe (cdr x))))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y)
                (inc y)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (my-reverse sequence)
  (foldr (lambda (x y)
           (append y (list x)))
         nil
         sequence))

(define (my-reverse-l sequence)
  (foldl (lambda (x y)
           (cons y x)) nil sequence))

(define (pairs n)
  (accumulate append
              nil
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (ordered-tuples n s)
  (filter (lambda (x)
            (= (accumulate + 0 x) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval (+ j 1) n)))
                              (enumerate-interval (+ i 1) (- n 1))))
                   (enumerate-interval 1 (- n 2)))))

(define (queens board-size)
  (define (safe? positions)
    (define cp (car positions))
    (define (conflict? cp p)
      (or (= (car cp) (car p))
          (= (abs (- (cdr cp) (cdr p)))
             (abs (- (car cp) (car p))))))
    (null? (filter (lambda (p)
                     (conflict? cp p))
                   (cdr positions))))
  (define (adjoin-position new-row k rest-of-queens)
    (cons (cons new-row k) rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter (lambda (positions)
                  (safe? positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position
                                   new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (#t (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (#t (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (make-exponentiation e1 e2)
  (cond ((=number? e1 0) 0)
        ((=number? e2 0) 1)
        ((and (number? e1) (number? e2)) (fast-expt e1 e2))
        (#t (list '^ e1 e2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (dec (exponent exp)))))
        (#t (display "Unknown expression type -- DERIV " exp))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (#t (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1 (intersection-set (cdr set1) set2))))
        (#t (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (#t (cdr set1) (cons (car set1) set2))))

(define (element-of-set-ordered? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (#t (element-of-set-ordered? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      nil
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-ordered (cdr set1)
                                                            (cdr set2))))
              ((< x1 x2) (intersection-set-ordered (cdr set1) set2))
              (#t (intersection-set-ordered set1 (cdr set2)))))))

(define (adjoin-set-ordered x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (#t (cons (car set) (adjoin-set-ordered x (cdr set))))))

(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (#t ((lambda (x1 x2)
               (cond ((< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2)))
                     ((< x2 x1) (cons x2 (union-set-ordered set1 (cdr set2))))
                     (#t (union-set-ordered (cdr set1) set2))))
             (car set1)
             (car set2)))))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry-set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry-set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))
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
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (#t (display "Bad bit: CHOOSE-BRANCH " bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (#t (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree
                                   (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (if (null? (memq s (symbols left)))
            (cons 1 (encode-symbol s right))
            (cons 0 (encode-symbol s left))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (let ((s1 (car set))
            (s2 (cadr set)))
        (successive-merge (adjoin-set (make-code-tree s1 s2)
                                      (cddr set))))))

(define symbol-pairs '((A 2)
                       (GET 2)
                       (SHA 3)
                       (WAH 1)
                       (BOOM 1)
                       (JOB 2)
                       (NA 16)
                       (YIP 9)))

(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA))
