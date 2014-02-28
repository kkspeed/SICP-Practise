(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2.0))

(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))

(define (inc c)
  (+ c 1))

(define (dec c)
  (- c 1))

(define (even? x)
  (= (mod x 2) 0))

(define (odd? x)
  (not (even? x)))

(define (fast-expt b n)
  (cond
   ((= n 0) 1)
   ((even? n) (square (fast-expt b (/ n 2))))
   (#t (* b (fast-expt b (- n 1))))))

(define (identity x) x)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(define (neg x)
  (- 0 x))
