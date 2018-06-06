#lang racket

(define zero-int (char->integer #\0))

(define (char->number ch) (- (char->integer ch) zero-int))

; Checkers
(define (base-checker str idx index-generator)
  (define this-char (string-ref str idx))
  (define other-index (index-generator str idx))
  (define other-index-capped
    (if (>= other-index (string-length str))
        (- other-index (string-length str))
        other-index))
  (define other-char (string-ref str other-index-capped))
  (if (char=? this-char other-char)
      (char->number this-char)
      0))

(define (next-index-checker str idx)
  (base-checker str idx (lambda (str idx) (+ idx 1))))

(define (halfway-index-checker str idx)
  (base-checker str idx (lambda (str idx) (+ idx (/ (string-length str) 2)))))
  
; Drivers
(define (check-string-from checker str idx)
  (if (< idx (string-length str))
      (+ (checker str idx) (check-string-from checker str (+ idx 1)))
      0))

(define (check-string checker str)
  (check-string-from checker str 0))

(define (check-p1-string str)
  (check-string next-index-checker str))

(define (check-p2-string str)
  (check-string halfway-index-checker str))