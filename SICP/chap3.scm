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
