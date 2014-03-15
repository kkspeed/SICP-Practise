(load "utils.scm")
(load "chap1.scm")

(define (make-accumulator init)
  (lambda (val)
    (set! init (+ init val))))

(define (make-monitor proc)
  (define times 0)
  (define (dispatch msg)
    (cond ((equal? msg 'how-many-times) times)
          (#t (begin
                (set! times (inc times))
                (proc msg)))))
  dispatch)

(define (estimate-pi trials)
  (sqrt (/ 6.0 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ (* 1.0 trials-passed) trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (#t (iter (- trials-remaining 1)
                    trials-passed))))
  (iter trials 0))

(define (random-range a b)
  (+ a (random (inc (- b a)))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (- x2 x1) (- y2 y1)
     (monte-carlo trials (lambda ()
                           (p (random-range x1 x2)
                              (random-range y1 y2))))))

(define (in-circle x y)
  (<= (+ (square x) (square y)) 1.0))

(define ex-3-8-f (let ((m nil))
                   (lambda (x)
                     (if (null? m)
                         (set! m x)
                         0))))
