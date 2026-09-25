#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;;  Recitation 3a: Typed Boolean Expressions                                ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Last week we wrote an interpreter and an optimization pass for Boolean
;; expressions in plain Racket. This week we redo both in plai-typed and
;; see what the type checker buys us.

;; Only print tests that fail.
(print-only-errors true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Evaluating Boolean expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Instead of four separate structs, define-type declares one type bexp-b
;; with four variants. Every field has a type.
(define-type bexp-b
  [leaf-node-b (b : boolean)]
  [and-node-b (b1 : bexp-b) (b2 : bexp-b)]
  [or-node-b (b1 : bexp-b) (b2 : bexp-b)]
  [not-node-b (b : bexp-b)])

;; Exercise: implement evaluate-bexp-b.
;; type-case plays the role that match played last week.
(define (evaluate-bexp-b [b : bexp-b]) : boolean
  (type-case bexp-b b
    [leaf-node-b (v) (error 'evaluate-bexp-b "TODO")]
    [and-node-b (b1 b2) (error 'evaluate-bexp-b "TODO")]
    [or-node-b (b1 b2) (error 'evaluate-bexp-b "TODO")]
    [not-node-b (b1) (error 'evaluate-bexp-b "TODO")]))

(test (evaluate-bexp-b (leaf-node-b #t)) #t)
(test (evaluate-bexp-b (and-node-b (leaf-node-b #t) (leaf-node-b #f))) #f)
(test (evaluate-bexp-b (not-node-b (or-node-b (leaf-node-b #f) (leaf-node-b #f)))) #t)

;; Discussion: what do the types buy us?
;;
;; (a) Uncomment the line below and press Run. When do you get the error,
;;     and how does that compare to building (leaf-node 's) last week?

; (evaluate-bexp-b (and-node-b (leaf-node-b #t) (leaf-node-b 's)))

;; (b) Last week, evaluate-bexp-m missed cases and only failed when we
;;     called it on a non-leaf. Uncomment evaluate-bexp-b-m and press Run.

#;
(define (evaluate-bexp-b-m [b : bexp-b]) : boolean
  (type-case bexp-b b
    [leaf-node-b (v) v]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. De Morgan optimization, typed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbolic Boolean expressions: variables at the leaves.
(define-type bexp
  [leaf-node (s : symbol)]
  [and-node (b1 : bexp) (b2 : bexp)]
  [or-node (b1 : bexp) (b2 : bexp)]
  [not-node (b1 : bexp)])

;; Recall the rewrite rules:
;;   ¬(P ∧ Q) = ¬P ∨ ¬Q
;;   ¬(P ∨ Q) = ¬P ∧ ¬Q
;;   ¬¬P      = P

;; Exercise: port demorgan-opt to plai-typed.
;;
;; Last week we matched nested patterns like (not-node (and-node b1 b2))
;; directly. type-case only looks one level deep, so the not-node case
;; needs a second type-case on its argument.
(define (demorgan-opt [b : bexp]) : bexp
  (type-case bexp b
    [leaf-node (v) (leaf-node v)]
    [and-node (b1 b2) (and-node (demorgan-opt b1) (demorgan-opt b2))]
    [or-node (b1 b2) (or-node (demorgan-opt b1) (demorgan-opt b2))]
    [not-node (b1)
      (error 'demorgan-opt "TODO")]))

;; ¬(¬P ∨ Q)  ~>  P ∧ ¬Q
(define bexp-1
  (not-node (or-node (not-node (leaf-node 'P)) (leaf-node 'Q))))
;; ¬¬P ∨ ¬Q  ~>  P ∨ ¬Q
(define bexp-2
  (or-node (not-node (not-node (leaf-node 'P))) (not-node (leaf-node 'Q))))
;; P ∨ ¬Q  ~>  P ∨ ¬Q  (already optimized)
(define bexp-3
  (or-node (leaf-node 'P) (not-node (leaf-node 'Q))))

;; Note that we don't need syn-equal? anymore: test compares define-type
;; values structurally.
(test (demorgan-opt bexp-1) (and-node (leaf-node 'P) (not-node (leaf-node 'Q))))
(test (demorgan-opt bexp-2) (or-node (leaf-node 'P) (not-node (leaf-node 'Q))))
(test (demorgan-opt bexp-3) (or-node (leaf-node 'P) (not-node (leaf-node 'Q))))
