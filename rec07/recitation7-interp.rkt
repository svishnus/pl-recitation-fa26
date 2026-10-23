#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 7 (challenge): rec without boxes                                                             ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In lecture 7 we added (rec f x body) to our interpreter in two ways:
;;   - a new kind of value, recV, that remembers the function's own name.
;;   - rewriting rec into a call to fix, which ties the knot with a box.
;;
;; In recitation7.rkt we wrote the Z combinator in Racket, and got recursion from nothing but
;; functions. Here we use Z to implement rec: no new kind of value, and no boxes.
;;
;; Today:
;;   a) Write Z in our language.
;;   b) Implement rec by rewriting it into a call to Z.
;;   c) Use Y instead of Z. What goes wrong?
;;   d) Compare the three ways of implementing rec.

;; 0. The language
;; The language from lecture 7: numbers, +, *, let, lambda (one argument), application, and
;;
;;   (if0 e1 e2 e3)    e2 if e1 evaluates to 0, otherwise e3
;;   (rec f x body)    a function of x; inside body, f refers to the function itself
;;
;; There are no boxes.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [if0C (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [recC (f : symbol) (x : symbol) (e : Expr)])

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)])

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(rec) (recC (s-exp->symbol (second l)) (s-exp->symbol (third l)) (parse (fourth l)))]
            [(if0) (if0C (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [else (appC (parse (first l)) (parse (second l)))])]
         [else (appC (parse (first l)) (parse (second l)))]))]))

(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [numC (n) (numV n)]
    [lambdaC (x e) (closV env x e)]
    [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
    [letC (x e1 e2)
          (let ([v1 (eval-env env e1)])
            (eval-env (extend-env (bind x v1) env) e2))]
    [appC (e1 e2)
          (let [(v1 (eval-env env e1))
                (v2 (eval-env env e2))]
            (eval-env (extend-env (bind (closV-x v1) v2) (closV-env v1)) (closV-e v1)))]
    [if0C (guard e1 e2)
          (let [(v (eval-env env guard))]
            (if (equal? v (numV 0))
                (eval-env env e1)
                (eval-env env e2)))]
    [idC (x) (lookup x env)]
    ;; b) Rewrite rec into a call to Z (defined below), and evaluate that.
    ;;    Hint: in lecture 7 we rewrote rec into a call to fix. Do the same with Z.
    [recC (f x e) (error 'eval-env "TODO")]))

(define (eval (e : Expr)) : Value
  (eval-env empty-env e))

;; Parses and evaluates a program.
(define (run (s : s-expression)) : Value
  (eval (parse s)))

;; a) Z in our language
;; Write Z from recitation7.rkt in our language's syntax. Remember that lambda takes exactly one
;; argument, written (lambda x body), and that application is written (e1 e2).
;; Until you fill it in, the tests below fail with "lookup: No binding found" (for TODO).

(define Z
  (parse '(lambda h TODO)))

;; fact-gen from recitation7.rkt, in our language. The parameter is called self instead of rec,
;; because rec is a keyword in our language: (rec (+ n -1)) would parse as a rec expression.
(define fact-gen
  (parse '(lambda self
            (lambda n
              (if0 n 1 (* n (self (+ n -1))))))))

(test (eval (appC (appC Z fact-gen) (numC 0))) (numV 1))
(test (eval (appC (appC Z fact-gen) (numC 5))) (numV 120))

;; Y from recitation7.rkt, in our language. You'll use it in c).
(define Y
  (parse '(lambda h
            ((lambda x (h (x x)))
             (lambda x (h (x x)))))))

;; b) rec
;; Fill in the recC case of eval-env above.

(test (run '((rec fact x (if0 x 1 (* x (fact (+ x -1))))) 5)) (numV 120))
(test (run '(let fact (rec f x
                        (if0 x
                             1
                             (* x (f (+ x -1)))))
              (fact 6)))
      (numV 720))
(test (run '(let fibb (rec f x
                        (if0 x
                             0
                             (if0 (+ x -1)
                                  1
                                  (+ (f (+ x -1)) (f (+ x -2))))))
              (fibb 6)))
      (numV 8))
;; The body of a rec can still use variables from outside it.
(test (run '(let k 10
              (let f (rec g x (if0 x k (g (+ x -1))))
                (f 3))))
      (numV 10))

;; c) Y instead of Z
;; In your recC case, replace Z with Y (defined above, after the Z tests) and run the file again.
;; What happens? Why? (Press Stop in DrRacket to get control back.) Then change it back.

;; d) Three ways to implement rec
;; We've now seen rec implemented three ways: with recV, with fix, and here with Z.
;; For each one: does it need a new kind of Value? Does it need boxes (a store)?
