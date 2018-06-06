#lang racket

; Part one utils

(define (find-vortex-num num)
  (define (find-vortex-num-rec num circle)
    (define rem-amount (if (zero? circle) 1 (* circle 4)))
    (if (positive-integer? num)
        (find-vortex-num-rec (- num rem-amount) (+ circle 2))
        (list num (/ (- circle 2) 2))))
  (find-vortex-num-rec num 0))

(define (generate-corner circle)
  (define (generate-corner-rec current-corner current-circle)
    (if (> current-circle circle)
        current-corner
        (generate-corner-rec (+ current-corner (* current-circle 2 4)) (add1 current-circle))))
  (generate-corner-rec 1 0))

; Part two
(define (gen-grid grid-size)
  (build-vector grid-size (lambda (x) (make-vector grid-size))))
(define (gen-grid-order order)
  (gen-grid (- (* order 2) 1)))

(define (grid-get grid row col)
  (vector-ref (vector-ref grid row) col))
(define (grid-set grid row col val)
  (vector-set! (vector-ref grid row) col val))

(define (start-grid grid)
  (define center (quotient (vector-length grid) 2))
  (grid-set grid center center 1))

(define (sum-around grid row col)
  (define grid-sz (vector-length grid))
  (define (gg r c)
    (if (or (negative? r) (negative? c) (>= r grid-sz) (>= c grid-sz))
        0
        (grid-get grid r c)))
  (+ (gg (sub1 row) (sub1 col)) (gg (sub1 row) col) (gg (sub1 row) (add1 col))
     (gg       row  (sub1 col)) 0                   (gg       row  (add1 col))
     (gg (add1 row) (sub1 col)) (gg (add1 row) col) (gg (add1 row) (add1 col))))

(define (fill-around grid row col)
  (define grid-sz (vector-length grid))
  (define (gg r c)
    (if (or (negative? r) (negative? c) (>= r grid-sz) (>= c grid-sz))
        0
        (grid-get grid r c)))
  (add1 (max (gg (sub1 row) (sub1 col)) (gg (sub1 row) col) (gg (sub1 row) (add1 col))
             (gg       row  (sub1 col)) 0                   (gg       row  (add1 col))
             (gg (add1 row) (sub1 col)) (gg (add1 row) col) (gg (add1 row) (add1 col)))))

(define (fill-next grid last-row last-col)
  (define center (quotient (vector-length grid) 2))
  (define start-row (add1 last-row))
  (define start-col (add1 last-col))
  (define steps (+ (- start-row center) (- start-col center)))
  ;(define sum-around fill-around)
  ; going up
  (for ([up-row (in-range (sub1 start-row) (- start-row steps 1) -1)])
    (grid-set grid up-row start-col (sum-around grid up-row start-col)))
  ; going left
  (for ([left-col (in-range (sub1 start-col) (- start-col steps 1) -1)])
    (define i-row (- start-row steps))
    (grid-set grid i-row left-col (sum-around grid i-row left-col)))
  ; going down
  (for ([down-row (in-range (add1 (- start-row steps)) (add1 start-row))])
    (define i-col (- start-col steps))
    (grid-set grid down-row i-col (sum-around grid down-row i-col)))
  ; gong right
  (for ([right-col (in-range (add1 (- start-col steps)) (add1 start-col))])
    (grid-set grid start-row right-col (sum-around grid start-row right-col)))
  )

(define (fill-grid grid)
  (start-grid grid)
  (define grid-mid (quotient (vector-length grid) 2))
  (for ([mid (in-range grid-mid)])
    (define loc (+ mid grid-mid))
    (fill-next grid loc loc))
  )

; Test

(define grid (gen-grid-order 5))
(fill-grid grid)
grid
; (vector-ref (vector-ref grid 0) 110)