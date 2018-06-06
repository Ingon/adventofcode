#lang racket

(define sample-input (open-input-file "day08-input-sample.txt"))
(define real-input (open-input-file "day08-input.txt"))
;(define input sample-input)
(define input real-input)

(define registers (make-hash))

(define (find-op str)
  (cond
    [(equal? str "inc") +]
    [(equal? str "dec") -]
    [else (raise "wtf")]))
(define (find-cond-op str)
  (cond
    [(equal? str ">") >]
    [(equal? str "<") <]
    [(equal? str ">=") >=]
    [(equal? str "<=") <=]
    [(equal? str "==") =]
    [(equal? str "!=") (lambda (a b) (not (= a b)))]
    [else (raise "mwtf")]))

(define (execute input)
  (define current-max 0)
  (for ([l (in-lines input)])
    (define parts (string-split l))
    (define target-reg (first parts))
    (define target-op (second parts))
    (define target-value (string->number (third parts)))
    (define cond-reg (fifth parts))
    (define cond-op (sixth parts))
    (define cond-num (string->number (seventh parts)))
    (define op (find-op target-op))
    (define cop (find-cond-op cond-op))
    (define target-reg-val (if (hash-has-key? registers target-reg)
                               (hash-ref registers target-reg)
                               0))
    (define cond-reg-val (if (hash-has-key? registers cond-reg)
                             (hash-ref registers cond-reg)
                             0))
    (when (cop cond-reg-val cond-num)
      (define new-val (op target-reg-val target-value))
      (when (> new-val current-max)
        (set! current-max new-val))
      (hash-set! registers target-reg new-val)))
  current-max)

(execute input)

(define ordered (sort (hash->list registers) (lambda (l r) (> (cdr l) (cdr r)))))

(first ordered)