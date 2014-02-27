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

(define (fast-expt b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (fast-expt b (/ n 2))))
   (#t (* b (fast-expt b (- n 1))))))

(define (ex-1-16-expt-iter b n)
  (define (f b a n)
    (cond
     ((= n 0) a)
     ((even? n) (f (square b) a (/ n 2)))
     (#t (f b (* a b) (- n 1)))))
  (f b 1 n))

(define (double x)
  (+ x x))

(define (mul a b)
  (cond
   ((= b 1) a)
   ((even? b) (double (mul a (/ b 2))))
   (#t (+ a (mul a (- b 1))))))

(define (ex-1-18-mul-iter a b)
  (define (f a b c)
    (cond
     ((= b 0) c)
     ((even? b) (f (double a) (/ b 2) c))
     (#t (f a (- b 1) (+ c a)))))
  (f a b 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (#t #f)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (mod (square (expmod base (/ exp 2) m)) m))
        (#t (mod (* base (expmod base (- exp 1) m)) m))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (ex-1-30-sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter term a 1))

(define (pi-product a b)
  (define (pi-term x)
    (* (/ (- x 1.0) x)
       (/ (+ x 1.0) x)))
  (define (pi-next x)
    (+ x 2.0))
  (product pi-term a pi-next b))

(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate1 combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (filter-accumulate combiner pred null-value term a next b)
  (cond ((> a b) null-value)
        ((pred a) (combiner (term a)
                            (filter-accumulate combiner pred null-value term
                                               (next a) next b)))
        (#t (filter-accumulate combiner pred null-value term
                               (next a) next b))))

(define (prime-sum a b)
  (define (prime? x)
    (fast-prime? x 100))
  (filter-accumulate + prime? 0 identity a inc b))

(define (pi-product-2 a b)
  (define (pi-term x)
    (* (/ (- x 1.0) x)
       (/ (+ x 1.0) x)))
  (define (pi-next x)
    (+ x 2.0))
  (* 4.0 (accumulate * 1 pi-term a pi-next b)))

(define (fixed-point f x thresh)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) thresh))
  (define v (f x))
  (if (close-enough? v x)
      x
      (fixed-point f v thresh)))

(define  (my-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0
               0.01))
