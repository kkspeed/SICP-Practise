(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2.0))

(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (sum-square-larger x y z)
  (cond
   ((&& (>= x z) (>= y z)) (sum-squares x y))
   ((&& (>= x y) (>= z y)) (sum-squares x z))
   (#t  (sum-squares y z))))

(define (test x y)
  (if (= x 0) 0 y))

(define (fact x)
  (define b 5)
  (* b x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess) (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
