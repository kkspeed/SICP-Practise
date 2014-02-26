(load "utils.scm")

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

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter n)
  (define (factor-iter counter product)
    (if (> counter n)
      product
      (factor-iter (+ counter 1) (* counter product))))
  (factor-iter 1 1))
