; 
; 1.2
;
(define ex-1-2
  (/ (+ 5
        4
        (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 
        (- 6 2)
        (- 2 7))))

;
; 1.3
;
(define (sqr x) (* x x))

(define (my-max x y) (if (> y x) y x))

(define (my-min x y) (if (> x y) y x))

(define (max-1st a b c)
  (my-max (my-max a b) c))

(define (max-2nd a b c)
  (if (> c (my-max a b))
    (my-max a b)
    (my-max (my-min a b) c)))

(define (ex-1-3 a b c) 
  (+ (sqr (max-1st a b c))
     (sqr (max-2nd a b c))))

;
; 1.8
;
(define (good-enough? guess x)
  (< (abs (- (sqr guess) x))
     0.001))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) 
  (/ (+ (/ x (sqr guess))
        (* 2 guess))
     3))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x)
                    x)))

(define (ex-1-8 x) (cube-root-iter 1.0 x))

;
; 1.11
;
(define (f-rec n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

(define (f-iter f-1 f-2 f-3 n-current n)
  (if (= n-current n)
    f-1
    (f-iter (+ f-1 (* 2 f-2) (* 3 f-3))
            f-1
            f-2
            (+ n-current 1)
            n)))

(define (f n)
  (if (< n 3) n (f-iter 2 1 0 2 n)))

;
; 1.12
;
(define (pascal row col)
  (if (or (= col 1) (= row col))
    1
    (+ (pascal (- row 1) col) (pascal (- row 1) (- col 1)))))

;
; 1.16
;
(define (fast-exp-iter result base power)
  (cond ((= power 0) result)
        ((even? power) (fast-exp-iter result (sqr base) (/ power 2)))
        (else (fast-exp-iter (* base result) base (- power 1)))))

(define (fast-exp a n) (fast-exp-iter 1 a n))

;
; 1.17, 1.18
;
(define (double x) (* 2 x))
(define (halve x) (/ 2 x))

(define (mul-iter a b r)
  (cond ((= b 1) (+ a r))
        ((even? b) (mul-iter (* a 2) (/ b 2) r))
        (else (mul-iter a (- b 1) (+ r a)))))

(define (mul a b) (mul-iter a b 0))

;
; 1.29
;

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (simpson-term k) 
    (* (if (even? k) 2 4)
       (f (+ a (* k h)))))
  (define (simpson-next k) (+ k 1))
  (define sumed (sum simpson-term 1 simpson-next (- n 1)))
  (* (/ h 3.0) 
     (+ (f a) sumed (f b))))

;
; 1.32
;
(define (id x) x)
(define (nxt x) (+ x 1))

(define (accumulate combiner null-value term a next b)
  (define (iter i result)
    (if (> i b) 
      result
      (iter (next i) (combiner result (term i)))))
  (iter a null-value))

(define (product term a next b)
  (define (combiner old new) (* old new))
  (accumulate combiner 1 term a next b))

;
; 1.41
;
(define (double f)
  (lambda (x) (f (f x))))

;
; 1.42
;
(define (compose f g) 
  (lambda (x) (f (g x))))

;
; 1.43
;
(define (repeated f n)
  (define (repeated-iter repeated-f i) 
    (if (equal? i n)
       repeated-f
       (repeated-iter (compose f repeated-f) (+ i 1))))
  (repeated-iter f 1))
