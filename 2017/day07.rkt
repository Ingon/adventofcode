#lang racket

(define (parse-assoc in)
  (for/list ([l (in-lines in)])
    (define spl (string-split l "->" #:trim? #t))
    (define header (string-split (first spl) #:trim? #t))
    (define name (first header))
    (define num (substring (second header) 1 (- (string-length (second header)) 1)))
    (define children-str (if (null? (rest spl))
                             ""
                             (second spl)))
    (define children (map string-trim (string-split children-str "," #:trim? #t)))
    (list name num children)))

(define sample-input (parse-assoc (open-input-file "day07-input-sample.txt")))
(define real-input (parse-assoc (open-input-file "day07-input.txt")))
;(define input sample-input)
(define input real-input)

(define keys (for/list ([e (in-list input)]) (first e)))
(define children (for/fold ([c null]) ([e (in-list input)]) (append c (third e))))
(define root (first (remove* children keys)))

(define parent-children (for/hash ([e (in-list input)])
                          (values (first e) (third e))))
(define base-weights (for/hash ([e (in-list input)])
                       (values (first e) (string->number (second e)))))

(define (find-actual-weights)
  (define weights (make-hash))
  (define (find-weight-rec node)
    (define children (hash-ref parent-children node))
    (define weight-children (foldl + 0 (map find-weight-rec children)))
    (define weight (+ (hash-ref base-weights node) weight-children))
    (hash-set! weights node weight)
    weight)
  (find-weight-rec root)
  weights)
(define actual-weights (find-actual-weights))

(define (same-weights el)
  (define weights (map (lambda (e) (hash-ref actual-weights e)) (hash-ref parent-children (first el))))
  (if (empty? weights)
      #f
      (not (apply = weights))))
(define only-unbalanced (make-hash (filter same-weights (hash->list parent-children))))

root
only-unbalanced
(map (lambda (e) (hash-ref actual-weights e)) (hash-ref parent-children "hmgrlpj"))
(hash-ref base-weights "drjmjug")