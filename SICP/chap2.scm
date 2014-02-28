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
