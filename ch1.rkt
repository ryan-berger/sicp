#lang sicp

; 1.1
(define a 3)
(define b (+ a 1))

(+ a b (* a b))
(= a b)

(if (and (> a b) (< b (* a b)))
    b
    a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1)))

; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; 1.3
(define (square x) (* x x))

(define (sum-biggest a b c)
  (if (>= a b)
      (+ (square a) (square (max b c)))
      (+ (square b) (square (max a c)))))

; 1.4
; if b > 0, then we add the operands of a, b together
; if b is <= 0, then we subtract which causes a negative
; b to become positive
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; 1.5
; using an applicative order evaluation will cause
; an infinite loop since p evaluates to p, and an applicative
; order eval will attempt to resolve p
;
; using a normal order evaluation would cause p not
; to be evaluated until it is needed, meaning that
; (test 0 (p)) will eval to 0, since the predicate (= x 0)
; is true, and the first branch of the if would be taken
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
; uncomment for infite loop
; (test 0 (p))

; 1.6
; new-if is problamatic because it requires that then-clause
; and else-clause are evaluated before they can be used within
; the cond. This is different from the special-form of if which
; lazily executes things
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

; 1.7
; good-enough? doesn't work well for large numbers because
; once you get above 2^54, many numbers become unrepresentable,
; so the epsilon of .001 you check against becomes impossible to
; 

; good guess! it is nearly exact for a small number
(good-enough? 2 4)

; should be a good guess since it is exact!
; Unsurprisingly good-enough? returns #f due to
; precision issues
(good-enough? 2e27 2e54)

; sqrt-iter-big checks the difference between the current guess
; and the last guess and only continues to approximate the square
; root if a difference exists
(define (sqrt-iter-big guess x)
  (if (< (abs (- (improve guess x) guess)) .001)
      guess
      (sqrt-iter-big (improve guess x) x)))

(define (sqrt-big x)
  (sqrt-iter-big 1.0 x))

(sqrt-big 2e32)

; 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* guess 2))
     3))

(define (cube-root-iter guess x)
  (if (< (abs (- (improve-cube guess x) guess)) .001)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 8)

; 1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
;
; (+ 4 5)
; = (inc (+ (dec 4) 5))
; = (inc (inc (+ (dec 3) 5)))
; = (inc (inc (inc (+ (dec 2) 5))))
; = (inc (inc (inc (inc (+ (dec 1) 5)))))
; = (inc (inc (inc (inc 5))))
; = (inc (inc (inc 6)))
; = (inc (inc 7))
; = (inc 8)
; = 9


;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
;
; (+ 4 5)
; = (+ (dec 4) (inc 5))
; = (+ (dec 3) (inc 6))
; = (+ (dec 2) (inc 7))
; = (+ (dec 1) (inc 8))
; = (inc 8)
; = 9

; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10)
(A 2 4)
(A 3 3)

; (define (f n) (* n 2))
(define (f n) (A 0 n))

; (define (g n) (pow 2 n))
(define (g n) (A 1 n))

; h(n) = 2^h(n-1)
(define (h n) (A 2 n))

; 1.11
(define (f-11 n)
  (if (< n 3)
      n
      (+ (f-11 (- n 1))
         (* 2 (f-11 (- n 2)))
         (* 3 (f-11 (- n 3))))))

(f-11 5)

(define (f-11-iter-help a b c count)
  (if (= count 0)
      c
      (+ (* 3 a)
         (* 2 b)
         (f-11-iter-help b c (- count 1)))))
(define (f-11-iter n)
  (f-11-iter-help 1 2 3 n))

(f-11 5)

; 1.12
;     1
;    1 1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;
;  1  
; 1 1 
;1 2 1
(define (pascal row col)
  (cond ((< col 0) 0)
        ((or (= row 0) (= row col)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))


(define (display-triangle n)
  (define col-count (+ n (- n 1)))  
  (define (get-col r c)
    (define space (- n r 1))
    (if (and (>= c space) (<= c (- col-count space)) (= (modulo (- c space) 2) 0))
        (display (pascal r (quotient (- c space) 2)))
        (display " ")))

  (define (iter from to callback)
    (if (< from to)
        (begin
          (callback to)
          (iter from (+ to 1)))))

  (define (iter-col r c)
    (if (< c col-count)
        (begin
           (get-col r c)
           (iter-col r (+ c 1)))))

  (define (iter-row r)
     (if (< r n)
         (begin
           (iter-col r 0)
           (display "\n")
           (iter-row (+ 1 r)))))
  
  (iter-row 0))

(display-triangle 7)
