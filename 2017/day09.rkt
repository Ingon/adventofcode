#lang racket

(define garbage 0)
(define (parse-groups input)
  (define (parse-garbadge)
    (define ignore-next (box #f))
    (for ([next (in-input-port-chars input)])
      #:break (and (not (unbox ignore-next))
                   (char=? next #\>))
      (cond
        [(unbox ignore-next) (set-box! ignore-next #f)]
        [(char=? next #\!) (set-box! ignore-next #t)]
        [else (set! garbage (add1 garbage))])))
  (define (parse-group)
    (define groups null)
    (for ([next (in-input-port-chars input)])
      #:break (char=? next #\})
      (cond
        [(char=? next #\{) (set! groups (cons (parse-group) groups))]
        [(char=? next #\<) (parse-garbadge)]
        [(char=? next #\,) null]))
    (reverse groups))
  (define first-group (read-char input))
  (parse-group))

(define (count-groups grouping)
  (add1 (foldl + 0 (map count-groups grouping))))

(define (score grouping)
  (define (score-rec current-grouping current-score)
    (define score (add1 current-score))
    (foldl + score (map (lambda (g) (score-rec g score)) current-grouping)))
  (score-rec grouping 0))

(define real-input (open-input-file "day09-input.txt"))

(score (parse-groups real-input))
garbage