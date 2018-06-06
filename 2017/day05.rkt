#lang racket

(define sample-input (list 0 3 0 1 -3))

(define input-port (open-input-file "day05-input.txt"))

(define (read-port port)
  (define next (read port))
  (if (eof-object? next)
      null
      (cons next (read-port port))))

(define target-input (read-port input-port))

(define (steps list fn)
  (define vec (list->vector list))
  (define (steps-rec pos acc)
    (if (or (negative? pos) (>= pos (vector-length vec)))
        acc
        (let ([jump (vector-ref vec pos)])
          (vector-set! vec pos (fn jump))
          (steps-rec (+ pos jump) (add1 acc))))
    )
  (steps-rec 0 0))