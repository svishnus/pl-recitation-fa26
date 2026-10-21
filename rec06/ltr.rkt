#lang racket

(define count 0)

(define (first-fn)
  (set! count (+ count 1))
  (printf "[first]  count = ~a\n" count)
  10)

(define (second-fn)
  (set! count (+ count 10))
  (printf "[second] count = ~a\n" count)
  20)

(define (print-sum a b)
  (printf "Result of a + b: ~a\n" (+ a b)))

(printf "=== Demonstration 1: Function Argument Evaluation Order ===\n")
(print-sum (first-fn) (second-fn))

(printf "\n=== Demonstration 2: Expression Operand Evaluation Order ===\n")
(define total (+ (first-fn) (second-fn)))
(printf "Total: ~a\n" total)
