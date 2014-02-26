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

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (#t (A (- x 1) (A x (- y 1))))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
   ((= amount 0) 1)
   ((or (< amount 0) (= kinds-of-coins 0)) 0)
   (#t (+ (cc amount
              (- kinds-of-coins 1))
          (cc (- amount (first-denomination kinds-of-coins))
              kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (ex-1-11-f n)
  (define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
  (f n))

(define (ex-1-11-f-iter n)
  (define (f a b c cnt)
    (if (>= cnt (- n 3))
        (+ a (* 2 b) (* 3 c))
        (f (+ a (* 2 b) (* 3 c))
           a
           b
           (+ cnt 1))))
  (f 2 1 0 0))

(define (ex-1-12-f row col)
  (cond
   ((= row 1) 1)
   ((= col 1) 1)
   ((= col row) 1)
   (#t (+ (ex-1-12-f (- row 1) (- col 1))
          (ex-1-12-f (- row 1) col)))))
