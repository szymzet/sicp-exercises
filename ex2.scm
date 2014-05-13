(define (gcd a b)
  (cond ((= a b) a)
        ((> a b) (gcd (- a b) b))
        (else (gcd a (- b a)))))

;
; rational numbers
;

; 2.1
(define (make-rat n d)
  (if (< d 0)
    (cons (* n -1) (abs d))
    (cons n (abs d))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; 2.2
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end) (cons start end))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (print-segment seg)
  (newline)
  (display "start: ")
  (print-point (start-segment seg))
  (newline)
  (display ", end: ")
  (print-point (end-segment seg)))

(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg))
        (avg (lambda (a b) (/ (+ a b) 2))))
    (make-point (avg (x-point start) (x-point end))
                (avg (y-point start) (y-point end)))))

(define p1 (make-point 10 20))
(define p2 (make-point 30 0))
(define s (make-segment p1 p2))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append (cdr list1) list2))))

;
; 2.17
;
(define (last-pair items)
  (define (last-pair-inner head tail)
    (if (null? tail)
      head
      (last-pair-inner (car tail) (cdr tail))))
  (last-pair-inner (car items) (cdr items)))

;
; 2.18
;
(define (reverse items)
  (define (reverse-aux items acc)
    (if (null? items)
        acc
        (reverse-aux (cdr items)
                     (cons (car items) acc))))
  (reverse-aux items (list)))

;
; 2.20
;
(define (same-parity . items)
  (define (filter-parity numbers is-odd)
    (cond ((null? numbers) numbers)
          ((equal? is-odd (odd? (car numbers)))
           (cons (car numbers) (filter-parity (cdr numbers) is-odd)))
          (else (filter-parity (cdr numbers) is-odd))))
  (filter-parity items (odd? (car items))))

;
; 2.23
;
(define (for-each proc items)
  (if (not (null? items))
      ((for-each proc (cdr items))
       (proc (car items)))))

;
; 2.27
;
(define (deep-reverse items)
  (define (deep-reverse-aux items acc)
    (if (null? items)
        acc
        (let ((head (car items))
              (tail (cdr items)))
              (if (pair? head)
                  (deep-reverse-aux (cdr items)
                                    (cons (deep-reverse head) acc))
                  (deep-reverse-aux (cdr items)
                                    (cons head acc))))))
  (deep-reverse-aux items '()))

;
; 2.28
;
(define (fringe items)
  (define (fringe-aux things acc)
    (cond ((pair? things) (fringe-aux (cdr things)
                                      (fringe-aux (car things) acc)))
          ((null? things) acc)
          (else (cons things acc))))
  (reverse (fringe-aux items '())))

;
; 2.31
;
(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((pair? tree) (cons (tree-map proc (car tree))
                            (tree-map proc (cdr tree))))
        (else (proc tree))))
