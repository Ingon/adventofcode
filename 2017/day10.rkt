#lang racket

(define sample-numbers (for/list ([i (in-range 5)]) i))
(define sample-lengths (list 3 4 1 5))

(define real-numbers (for/list ([i (in-range 256)]) i))
(define real-lengths (list 165 1 255 31 87 52 24 113 0 91 148 254 158 2 73 153))