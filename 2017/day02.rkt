#lang racket

; Part one

(define (find-boundaries lst)
  (define (choose-element fn new-val old-val)
    (if old-val
        (fn new-val old-val)
        new-val))
  (define (find-boundaries-rec lst bottom top)
    (if (null? lst)
        (list bottom top)
        (let* ([element (car lst)]
               [new-bottom (choose-element min element bottom)]
               [new-top (choose-element max element top)])
          (find-boundaries-rec (cdr lst) new-bottom new-top))))
  (find-boundaries-rec lst #f #f))

(define (find-difference lst)
  (define boundaries (find-boundaries lst))
  (- (second boundaries) (first boundaries)))

(define (find-checksum lst-of-rows)
  (foldl + 0 (map find-difference lst-of-rows)))

; Part two

(define (find-div-element lst el)
  (if (null? lst)
      0
      (let* ([next-el (first lst)]
             [top (max next-el el)]
             [bottom (min next-el el)]
             [rem (remainder top bottom)])
        (if (= rem 0)
            (quotient top bottom)
            (find-div-element (rest lst) el)))))

(define (find-div lst)
  (define (find-div-rec lst sum)
    (if (null? lst)
        sum
        (find-div-rec (rest lst)
                      (+ sum (find-div-element (rest lst) (first lst))))))
  (find-div-rec lst 0))

(define (find-divsum lst-of-rows)
  (foldl + 0 (map find-div lst-of-rows)))
