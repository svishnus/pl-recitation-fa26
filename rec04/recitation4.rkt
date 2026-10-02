#lang plai-typed
(require (typed-in racket/base [gensym : (-> symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 4: Pattern matching                                                                          ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let us extend our running language with pattern matching.
;; For now, we will only support value-based (number-based) pattern matching,
;; a la our factorial and fibonacci functions, and wildcard pattern matching.
;; Thus, pattern matching expressions will look like:
;; (match e (n1 e1) (n2 e2) .... (_ en))
;; where n1, n2 are numbers, and _ is a special wildcard symbol.

;; The semantics evaluates e and compares it against n1, n2 ... in turn.
;; The first matching clause wins; _ matches anything; if no clause matches, it's an error.

;; 1. Type definitions
;; Let us define a type for patterns, and extend our ArithE type with them.

(define-type PatternE
             [numP (n : number) (e : ArithE)]
             [anyP (e : ArithE)])

(define-type ArithE
             [numC (n : number)]
             [plusC (e1 : ArithE) (e2 : ArithE)]
             [timesC (e1 : ArithE) (e2 : ArithE)]
             [letC (x : symbol) (e1 : ArithE) (e2 : ArithE)]
             [idC (x : symbol)]
             [patternC (e : ArithE) (patterns : (listof PatternE))])

; Example: match 5 with
;         | 5 -> 0
;         | 6 -> 1
; Expected: 0
(define example-1 : ArithE
  (patternC
    (numC 5)
    (list
      (numP 5 (numC 0))
      (numP 6 (numC 1)))))

; Example: match (1 + 2) with
;         | 3 -> 1
;         | _ -> x
; Expected: 1
(define example-2 : ArithE
  (patternC
    (plusC (numC 1) (numC 2))
    (list
      (numP 3 (numC 1))
      (anyP (idC 'x)))))

; Example: match x with
;         | 0 -> z
;         | _ -> match y with
;               | 0 -> x
;               | 1 -> y
; Expected: unbound variable!
(define example-3 : ArithE
  (patternC
    (idC 'x)
    (list
      (numP 0 (idC 'z))
      (anyP (patternC
              (idC 'y)
              (list
                (numP 0 (idC 'x))
                (numP 1 (idC 'y))))))))

; Example: let x = 1 in
;            let y = 1 in
;              match x with
;             | 0 -> z
;             | _ -> match y with
;                   | 0 -> x
;                   | 1 -> y + 1
; Expected: 2
(define example-4 : ArithE
  (letC
    'x
    (numC 1)
    (letC
      'y
      (numC 1)
      (patternC
        (idC 'x)
        (list
          (numP 0 (idC 'z))
          (anyP (patternC
                  (idC 'y)
                  (list
                    (numP 0 (idC 'x))
                    (numP 1 (plusC (numC 1) (idC 'y)))))))))))

;; 2. Parsing
;; Next, let's parse pattern matching expressions.
;; Hint: first define a helper function parse-patterns,
;; that takes in a list of s-expressions, and parses them to create a list of PatternE objects.
;; Then, extend our existing parse function to handle pattern matching.


(define (parse-patterns (s-pats : (listof s-expression))) : (listof PatternE)
  (error 'parse-patterns "TODO"))


(define (parse (s : s-expression)) : ArithE
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
         [(match) (error 'parse "TODO")]))]))

(define example-1-sexp '(match 5 (5 0) (6 1)))
(define example-2-sexp '(match (+ 1 2) (3 1) (_ x)))
(define example-3-sexp '(match x (0 z) (_ (match y (0 x) (1 y)))))
(define example-4-sexp '(let x 1 (let y 1 (match x (0 z) (_ (match y (0 x) (1 (+ 1 y))))))))

(test (parse example-1-sexp) example-1)
(test (parse example-2-sexp) example-2)
(test (parse example-3-sexp) example-3)
(test (parse example-4-sexp) example-4)

;; 3. Substitution
;; Why do we need to define substitution for pattern matching expressions?


(define (subst-pattern (erep : ArithE) (x : symbol)) : (PatternE -> PatternE)
  (lambda (p) (type-case PatternE p
                         [numP (n e) (error 'subst-pattern "TODO")]
                         [anyP (e) (error 'subst-pattern "TODO")])))

; Use the subst-pattern function to implement subst for patternC. Hint: use map!

(define (subst (erep : ArithE) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
             [idC (y) (if (equal? x y) erep (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst erep x e1) (subst erep x e2))]
             [timesC (e1 e2) (timesC (subst erep x e1) (subst erep x e2))]
             [patternC (e pats) (error 'subst "TODO")]
             [letC (y e1 e2)
              (cond
                [(equal? x y) (letC y (subst erep x e1) e2)]
                [else
                 (let [(z (gensym))]
                   (letC z (subst erep x e1) (subst erep x (subst (idC z) y e2))))])]))

;; 4. Evaluation
;; Finally, let's extend the eval function to evaluate pattern-matching.
;; Again, let's define a helper function get-pat-expr that given a pattern:
;; (match e (n1 e1) (n2 e2) .... (_ en))
;; represented as a list of PatternE objects, and num which is the evaluation result of e,
;; returns the matching ei.

(define (get-pat-expr (num : number) (pats : (listof PatternE))) : ArithE
  (error 'get-pat-expr "TODO"))

; Use get-pat-expr to implement eval for patternC.


(define (eval (e : ArithE)) : number
  (type-case ArithE e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval e1) (eval e2))]
             [timesC (e1 e2) (* (eval e1) (eval e2))]
             [patternC (e pats) (error 'eval "TODO")]
             [letC (x e1 e2)
              (eval (subst e1 x e2))
              ]
             [idC (x) (error 'eval "unbound variable!")]
             )
  )

(test (eval (parse example-1-sexp)) 0)
(test (eval (parse example-2-sexp)) 1)
(test (eval (parse example-4-sexp)) 2)
(test/exn (eval (parse example-3-sexp)) "unbound")