(define nil '())

(define (null? lst) (equal? lst nil))

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

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1)
            (append (cdr lst1) lst2))))

(define (last-pair lst)
  (cond
   ((null? lst) nil)
   ((null? (cdr lst)) lst)
   (#t (last-pair (cdr lst)))))

(define (reverse lst)
  (define (do-reverse lst acc)
    (if (null? lst)
        acc
        (do-reverse (cdr lst) (cons (car lst) acc))))
  (do-reverse lst nil))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc lst)
  (define (do-foreach acc l)
    (if (null? l)
        acc
        (do-foreach (proc (car l))
                  (cdr l))))
  (do-foreach nil lst))

(define (not x)
  (if (equal? x #t)
      #f
      #t))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
      (list nil)
      ((lambda (rest)
         (append rest (map (lambda (x)
                             (cons (car s) x)) rest)))
       (subsets (cdr s)))))

(define (filter predicate sequence)
  (cond ((null? sequence) sequence)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (#t (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (#t (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define foldr accumulate)

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (memq item x)
  (cond ((null? x) x)
        ((eq? item (car x)) x)
        (#t (memq item (cdr x)))))
