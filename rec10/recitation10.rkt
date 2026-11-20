#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;;  Recitation 10: Effect handlers                                            ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup: this file needs the `effect-racket` package. In DrRacket, go to
;; File -> Install Package... and enter `effect-racket`, or run
;;   raco pkg install effect-racket
;;
;; Three forms do most of the work:
;;   (effect name (arg ...))    declares an effect. Calling (name arg ...)
;;                              *performs* it: control jumps to the nearest
;;                              handler for that effect.
;;   (handler [pattern body] ...)
;;                              a handler: what to do for each effect it matches.
;;                              Effects it doesn't match go to the next handler out.
;;   (with (h ...) body ...)    runs body with the handlers h ... installed.
;;
;; Today:
;;   1. Handling: effects as exceptions.
;;   2. Resuming: `continue` sends a value back to where the effect was performed.
;;   3. Handlers as values, and `return` clauses.
;;   4. State, and combining handlers. Order matters.
;;   5. Resuming more than once: nondeterminism.
;;   6. Handlers that perform effects; `handler-append`.
;;   7. Exercises.

(require effect-racket)

;; ============================================================================
;; Part 1: Handling
;; ============================================================================
;; If a handler arm doesn't call `continue`, its result becomes the result of
;; the whole `with`, like catching an exception.
(effect throw (msg))

(define (safe-divide x y)
  (if (= y 0)
      (throw "div-by-zero")
      (quotient x y)))

(define (throw-const)
  (with ((handler [(throw msg) 42]))
        (+ 8 (safe-divide 1 0))))

(define (no-throw)
  (with ((handler [(throw msg) 42]))
        (+ 8 (safe-divide 10 2))))

(effect note (msg))

(define (forwarding)
  (with ((handler [(throw msg) (list 'caught msg)]))
        (with ((handler [(note msg) (continue (void))]))
              (note "about to divide")
              (safe-divide 1 0))))

(define (rethrow)
  (with ((handler [(throw msg) (list 'outer msg)]))
        (with ((handler [(throw msg) (throw (string-append "wrapped: " msg))]))
              (safe-divide 1 0))))

;; ============================================================================
;; Part 2: Resuming
;; ============================================================================
;; Inside a handler arm, (continue v) resumes the code that performed the
;; effect, with v as the effect's result. When that code finishes, `continue`
;; returns, and the arm can keep computing with its result.
(effect ask ())

(define (add-twice)
  (+ (ask) (ask)))

(define (ask-const)
  (with ((handler [(ask) (continue 21)]))
        (add-twice)))

(define (ask-random)
  (with ((handler [(ask) (continue (random 100))]))
        (add-twice)))

(define (ask-once)
  (define count 0)
  (with ((handler [(ask)
                   (set! count (add1 count))
                   (if (<= count 1) (continue 42) 0)]))
        (add-twice)))

(define (ask-times-ten)
  (with ((handler [(ask) (* 10 (continue 1))]))
        (add-twice)))

(effect yield (i))

(define (traverse xs)
  (match xs
    [(cons x xx) (when (yield x) (traverse xx))]
    ['() (void)]))

(define (print-elems)
  (with ((handler [(yield i)
                   (printf "yielded ~a\n" i)
                   (continue (<= i 2))]))
        (traverse '(1 2 3 4))))

(effect width ())

(define (string-truncate s n)
  (if (> (string-length s) n) (substring s 0 n) s))

(define (pretty-internal line)
  (string-truncate line (width)))

(define (pretty d)
  (string-join (map pretty-internal (string-split d "\n")) "\n"))

(define (width-is n)
  (handler [(width) (continue n)]))

(define (pretty-thin d)
  (with ((width-is 40))
        (pretty d)))

;; ============================================================================
;; Part 3: Abstracting handlers and return clauses
;; ============================================================================
(effect emit (msg))

(define (ehello)
  (emit "hello")
  (emit "world"))

(define emit-console
  (handler [(emit msg) (displayln msg) (continue (void))]))

(define (ehello-console)
  (with (emit-console)
        (ehello)))

;; A [(return x) ...] arm runs when the body of the `with` finishes normally
;; with value x. Its result becomes the result of the `with`.
;; This one is a function so that each use gets a fresh `lines`.
(define (emit-collect/mutable)
  (define lines '())
  (handler [(emit msg) (set! lines (cons msg lines)) (continue (void))]
           [(return x) (string-join (reverse lines) "\n")]))

;; The same thing without mutation. `continue*` resumes *without* this handler
;; installed, so we install a new one that remembers one more line.
(define (emit-collect [lines '()])
  (handler
    [(emit msg) (with ((emit-collect (cons msg lines)))
                      (continue* (void)))]
    [(return x) (string-join (reverse lines) "\n")]))

(define (ehello-collect)
  (with ((emit-collect))
        (ehello)))

(define (catch hnd)
  (handler [(throw msg) (hnd msg)]))

(define (catch-example)
  (with ((catch (λ (msg) (printf "error: ~a\n" msg) 42)))
        (safe-divide 1 0)))

(struct just (value) #:transparent)

(define throw-maybe
  (handler
    [(return x) (just x)]
    [(throw msg) 'nothing]))

(define (maybe-default m d)
  (if (just? m) (just-value m) d))

(define (div42)
  (maybe-default (with (throw-maybe) (safe-divide 1 0)) 42))

;; ============================================================================
;; Part 4: State and combining handlers
;; ============================================================================
(effect get ())
(effect put (x))

(define (sumdown [sum 0])
  (define i (get))
  (if (<= i 0)
      sum
      (begin (put (- i 1))
        (sumdown (+ sum i)))))

(define (state-service st)
  (handler
    [(get) (continue st)]
    [(put x) (with ((state-service x))
                   (continue* (void)))]))

(define (state-example)
  (with ((state-service 10))
        (sumdown)))

(define (pstate-service st)
  (handler
    [(get) (continue st)]
    [(put x) (with ((pstate-service x))
                   (continue* (void)))]
    [(return v) (cons v st)]))

(define (pstate-example)
  (with ((pstate-service 10))
        (sumdown)))

;; From here on, names like state-throw list the handlers from outer to inner:
;; state-throw has the state handler outside and throw-maybe inside.
(define (no-odds)
  (define i (get))
  (if (odd? i)
      (throw "no odds")
      (begin (put (quotient i 2))
        i)))

(define (state-throw init [thunk no-odds])
  (with ((pstate-service init))
        (with (throw-maybe)
              (thunk))))

(define (throw-state init [thunk no-odds])
  (with (throw-maybe)
        (with ((pstate-service init))
              (thunk))))

(define (no-odds-twice)
  (+ (no-odds) (no-odds)))

;; ============================================================================
;; Part 5: Resuming more than once
;; ============================================================================
(effect choice ())

(define (xor*)
  (define p (choice))
  (define q (choice))
  (if p (not q) q))

(define choice-random
  (handler [(choice) (continue (zero? (random 2)))]))

(define choice-all
  (handler
    [(return x) (list x)]
    [(choice) (append (continue #f) (continue #t))]))

(define (xor-all)
  (with (choice-all) (xor*)))

(define (surprising)
  (define p (choice))
  (define i (get))
  (put (+ i 1))
  (if (and (> i 0) p) (xor*) #f))

(define (state-choice)
  (with ((pstate-service 0))
        (with (choice-all)
              (surprising))))

(define (choice-state)
  (with (choice-all)
        (with ((pstate-service 0))
              (surprising))))

(define (pstate/mutable init)
  (define st init)
  (handler
    [(get) (continue st)]
    [(put x) (set! st x) (continue (void))]
    [(return v) (cons v st)]))

(define (choice-state/mutable)
  (with (choice-all)
        (with ((pstate/mutable 0))
              (surprising))))

;; ============================================================================
;; Part 6: Overriding and appending handlers
;; ============================================================================
;; A handler arm runs outside its own handler, so an effect it performs goes
;; to the next handler out.
(define emit-quoted
  (handler [(emit msg) (continue (emit (string-append "\"" msg "\"")))]))

(define (ehello-quoted)
  (with (emit-console)
        (with (emit-quoted)
              (ehello))))

(define extra-wide
  (handler [(width) (continue (* 2 (width)))]))

(define (wide-example)
  (with ((width-is 3))
        (with (extra-wide)
              (pretty "abcdefghij"))))

;; (handler-append h1 h2) acts like (with (h1) (with (h2) ...)): h1 is outer,
;; so h2 sees each effect first. (with (h1 h2) ...) nests the same way.
(effect go (x))
(define twice-service (handler [(go x) (continue (go (* x 2)))]))
(define decr-service (handler [(go x) (continue (sub1 x))]))
(define twice-then-decr-service (handler-append decr-service twice-service))

(define (append-example)
  (with (twice-then-decr-service) (go 10)))

;; ============================================================================
;; Exercises
;; ============================================================================

;; 1. Write `(ask-from xs)`: a handler that answers the i-th `ask` with the
;;    i-th element of xs. (You can assume xs is long enough.)
;;    Hint: the handler has to remember which elements are left. Look at how
;;    emit-collect and state-service keep track of things without mutation.
(define (ask-from xs)
  (handler
    [(ask) (error 'ask-from "TODO")]))

;; 2. Write `(fuel n)`: a handler that allows n ticks. On tick n+1 the whole
;;    `with` returns 'out-of-fuel.
;;    Same idea as Exercise 1. What's new is that sometimes the handler
;;    shouldn't resume at all.
(effect tick ())

(define (count-to k)
  (for ([i (in-range k)]) (tick))
  'done)

(define (fuel n)
  (handler
    [(tick) (error 'fuel "TODO")]))

;; 3. Write `(yield->list thunk)`: run thunk and return a list of everything
;;    it yielded, in order.
;;    Remember that `traverse` stops early if `yield` returns #f.
(define (yield->list thunk)
  (error 'yield->list "TODO"))

;; 4. (a) Write `choice-all+fail`: choice-all, plus an arm for `fail` that
;;        drops the current branch, so it contributes no results.
;;    (b) Write `(choose-int lo hi)` using only `choice` and `fail` (and
;;        recursion). Under choice-all+fail, it should produce every integer
;;        in [lo, hi], smallest first.
;;        Remember that choice-all tries #f before #t.
;;    (c) Run (with (choice-all+fail) (pythag 15)). Why do the triples come
;;        out in this order?
(effect fail ())

(define choice-all+fail
  (handler
    [(fail) (error 'choice-all+fail "TODO")]))

(define (choose-int lo hi)
  (error 'choose-int "TODO"))

(define (pythag n)
  (define a (choose-int 1 n))
  (define b (choose-int a n))
  (define c (choose-int b n))
  (if (= (+ (* a a) (* b b)) (* c c))
      (list a b c)
      (fail)))

;; 5. (Open-ended) `dynamic-wind` runs its first thunk every time control
;;    enters the middle thunk, and its last thunk every time control leaves.
;;    Predict how many times "in" and "out" print, then run it in the
;;    interactions window. Does it match your prediction? Why or why not?
;;    (with (choice-all)
;;      (dynamic-wind (λ () (displayln "in"))
;;                    (λ () (choice))
;;                    (λ () (displayln "out"))))

;; ============================================================================
;; Tests
;; ============================================================================
(module+ test
  (require rackunit)

  ;; Part 1
  (check-equal? (throw-const) 42)
  (check-equal? (no-throw) 13)
  (check-equal? (forwarding) '(caught "div-by-zero"))
  (check-equal? (rethrow) '(outer "wrapped: div-by-zero"))

  ;; Part 2
  (check-equal? (ask-const) 42)
  (check-pred integer? (ask-random))
  (check-equal? (ask-once) 0)
  (check-equal? (ask-times-ten) 200)
  (check-equal? (with-output-to-string print-elems)
                "yielded 1\nyielded 2\nyielded 3\n")
  (check-equal? (with ((width-is 5)) (pretty "hello world\nhi")) "hello\nhi")
  (check-equal? (pretty-thin (make-string 50 #\x)) (make-string 40 #\x))

  ;; Part 3
  (check-equal? (with-output-to-string ehello-console) "hello\nworld\n")
  (check-equal? (with ((emit-collect/mutable)) (ehello)) "hello\nworld")
  (check-equal? (ehello-collect) "hello\nworld")
  (check-equal? (with-output-to-string catch-example) "error: div-by-zero\n")
  (check-equal? (parameterize ([current-output-port (open-output-nowhere)])
                  (catch-example))
                42)
  (check-equal? (with (throw-maybe) (safe-divide 10 2)) (just 5))
  (check-equal? (with (throw-maybe) (safe-divide 1 0)) 'nothing)
  (check-equal? (div42) 42)

  ;; Part 4
  (check-equal? (state-example) 55)
  (check-equal? (pstate-example) '(55 . 0))
  (check-equal? (state-throw 42) (cons (just 42) 21))
  (check-equal? (throw-state 42) (just '(42 . 21)))
  (check-equal? (state-throw 21) '(nothing . 21))
  (check-equal? (throw-state 21) 'nothing)
  (check-equal? (state-throw 42 no-odds-twice) '(nothing . 21))
  (check-equal? (throw-state 42 no-odds-twice) 'nothing)

  ;; Part 5
  (check-pred boolean? (with (choice-random) (xor*)))
  (check-equal? (xor-all) '(#f #t #t #f))
  (check-equal? (state-choice) '((#f #f #t #t #f) . 2))
  (check-equal? (choice-state) '((#f . 1) (#f . 1)))
  (check-equal? (choice-state/mutable)
                '((#f . 1) (#f . 2) (#t . 2) (#t . 2) (#f . 2)))

  ;; Part 6
  (check-equal? (with-output-to-string ehello-quoted) "\"hello\"\n\"world\"\n")
  (check-equal? (wide-example) "abcdef")
  (check-equal? (append-example) 19))

;; Exercises (fail until the TODOs are filled in)
(module+ test
  (check-equal? (with ((ask-from '(1 2))) (add-twice)) 3)
  (check-equal? (with ((ask-from '(1 2))) (list (ask) (ask))) '(1 2))
  (check-equal? (with ((fuel 3)) (count-to 3)) 'done)
  (check-equal? (with ((fuel 3)) (count-to 4)) 'out-of-fuel)
  (check-equal? (yield->list (λ () (traverse '(1 2 3 4)))) '(1 2 3 4))
  (check-equal? (with (choice-all+fail) (choose-int 1 3)) '(1 2 3))
  (check-equal? (with (choice-all+fail) (pythag 15))
                '((3 4 5) (5 12 13) (6 8 10) (9 12 15))))
