#lang racket

(define phrases-port (open-input-file "day04-input.txt"))

(define (process-phrase fn port)
  (define next-phrase (read-line port))
  (unless (eof-object? next-phrase)
    (fn next-phrase)
    (process-phrase fn port)))

(define (valid-unique? str)
  (define words (string-split str))
  (define words-set (list->set words))
  (= (length words) (set-count words-set)))

(define (valid-anagram? str)
  (define words (string-split str))
  (define (reword word) (list->string (sort (string->list word) char<?)))
  (define rewords (map reword words))
  (define words-set (list->set rewords))
  (= (length words) (set-count words-set)))

(define (count-valid valid? port)
  (define count 0)
  (define (counter str)
    (when (valid? str) (set! count (add1 count))))
  (process-phrase counter port)
  count)
