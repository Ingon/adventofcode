#lang racket

(define sample-input (list 0 2 7 0))

(define p1-input (list 5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6))

(define (find-max-index vec)
  (define (find-max-index-rec vec index max max-index)
    (cond
      [(>= index (vector-length vec)) max-index]
      [(> (vector-ref vec index) max) (find-max-index-rec vec (add1 index) (vector-ref vec index) index)]
      [else (find-max-index-rec vec (add1 index) max max-index)]))
  (find-max-index-rec vec 1 (vector-ref vec 0) 0))

(define (redist lst)
  (define vec (list->vector lst))
  (define max-index (find-max-index vec))
  (define max-value (vector-ref vec max-index))
  (define (inc index) (vector-set! vec index (add1 (vector-ref vec index))))
  (vector-set! vec max-index 0)
  (for ([x (in-range max-value)])
    (inc (remainder (+ max-index x 1) (vector-length vec))))
  (vector->list vec))

(define (count-cycles lst)
  (define (count-cycles-rec lst cnt seen)
    (cond
      [(member lst seen) cnt]
      [else (count-cycles-rec (redist lst) (add1 cnt) (cons lst seen))]
    ))
  (count-cycles-rec lst 0 null))

(define (count-cycles-loop lst)
  (define (count-cycles-rec lst cnt seen)
    (cond
      [(member lst seen) (- (length seen) (index-of (reverse seen) lst))]
      [else (count-cycles-rec (redist lst) (add1 cnt) (cons lst seen))]
    ))
  (count-cycles-rec lst 0 null))