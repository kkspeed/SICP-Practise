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
