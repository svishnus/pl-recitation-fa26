#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 5: Environments and scope                                                                    ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In lecture 5 we replaced substitution with environments. A naive environment interpreter
;; gave us *dynamic* scope (lecture5d.rkt), and we fixed it with closures to get *static*
;; (lexical) scope (lecture5e.rkt).
;;
;; Today:
;;   1. A different way to represent environments: as functions.
;;   2. One interpreter that can run with either static or dynamic scope.
;;   3. Predict what programs do under each kind of scope, then check.
;;   4. Pen and paper: scope in languages with assignment.

;; 0. The language
;; Same as lecture5e.rkt: numbers, +, *, let, single-argument lambda, and application.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)])

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
            [else (appC (parse (first l)) (parse (second l)))])]
         [else (appC (parse (first l)) (parse (second l)))]))]))

;; 1. Environments as functions
;; In lecture, an environment was a list of bindings, and lookup walked down the list.
;; But the only thing we ever do with an environment is ask "what is x bound to?"
;; So an environment can simply *be* a function that answers that question:
;; given a variable, it returns (some v) if the variable is bound to v, and (none) if it is unbound.

(define-type-alias Env (symbol -> (optionof Value)))

;; The empty environment: nothing is bound.
(define empty-env : Env
  (lambda (x) (none)))

;; Bind x to v, on top of env.
;; (Lecture's extend-env took a Binding; here we pass the name and value directly.)
;; Hint: return a new function. When is it asked about x? What should it do otherwise?
(define (extend-env (x : symbol) (v : Value) (env : Env)) : Env
  (lambda (y)
    (if (equal? x y)
        (some v)
        (env y))))

;; Look up x, signaling an error if it is unbound.
;; Hint: you can type-case on an optionof, just like on a type you defined:
;;   (type-case (optionof Value) ...
;;     [some (v) ...]
;;     [none () ...])
(define (lookup (x : symbol) (env : Env)) : Value
  (type-case (optionof Value) (env x)
    [some (v) v]
    [none () (error 'lookup (string-append "unbound variable: " (symbol->string x)))]))

(define env-1 (extend-env 'x (numV 1) (extend-env 'y (numV 2) empty-env)))
(define env-2 (extend-env 'x (numV 3) env-1))

(test (lookup 'x env-1) (numV 1))
(test (lookup 'y env-1) (numV 2))
(test/exn (lookup 'z env-1) "unbound")
(test (lookup 'x env-2) (numV 3))   ; the newer binding shadows the older one
(test (lookup 'x env-1) (numV 1))   ; env-1 is unchanged

;; Discussion:
;; a) How long does lookup take, compared to the list version? (Same: it walks back through
;;    one function per binding, just like walking down the list.)
;; b) What can you do with a list environment that you can't do with a function environment?
;;    (Print it, count it, iterate over it, or drop bindings from it. A function is a
;;    black box: you can only ask it about one variable at a time.)

;; 2. The interpreter
;; Below is lecture5e.rkt's interpreter, using our new environments, with one extra
;; argument: dynamic?. When dynamic? is #f we get static scope (like lecture5e.rkt);
;; when it is #t we get dynamic scope (like lecture5d.rkt).
;; Note that every case except appC is the same for both!

(define (eval-env (dynamic? : boolean) (env : Env) (e : Expr)) : Value
  (type-case Expr e
    [numC (n) (numV n)]
    [plusC (e1 e2) (numV (+ (numV-n (eval-env dynamic? env e1)) (numV-n (eval-env dynamic? env e2))))]
    [timesC (e1 e2) (numV (* (numV-n (eval-env dynamic? env e1)) (numV-n (eval-env dynamic? env e2))))]
    [letC (x e1 e2)
          (let ([v1 (eval-env dynamic? env e1)])
            (eval-env dynamic? (extend-env x v1 env) e2))]
    [idC (x) (lookup x env)]
    [lambdaC (x body) (closV env x body)]
    ;; Fill in appC. Which environment should the function body run in?
    ;;   static:  the environment saved in the closure (where the lambda was written)
    ;;   dynamic: the current environment (where the function is called)
    [appC (e1 e2)
          (let ([v1 (eval-env dynamic? env e1)]
                [v2 (eval-env dynamic? env e2)])
            (let ([body-env (if dynamic? env (closV-env v1))])
              (eval-env dynamic? (extend-env (closV-x v1) v2 body-env) (closV-e v1))))]))

(define (eval-static (s : s-expression)) : Value
  (eval-env #f empty-env (parse s)))

(define (eval-dynamic (s : s-expression)) : Value
  (eval-env #t empty-env (parse s)))

;; The example from the end of lecture 5:
(define lecture-example
  '(let x 2
     (let addx (lambda y (+ y x))
       (let x 6
         (addx 3)))))

(test (eval-static lecture-example) (numV 5))
(test (eval-dynamic lecture-example) (numV 9))

;; 3. Predict, then check
;; For each program, write down what you think it evaluates to under static scope and under
;; dynamic scope *before* running it. Then check by running, e.g., (eval-static ex-a-f) and
;; (eval-dynamic ex-a-f) in the interactions window.
;; The answer might be an error rather than a value; if so, say which error and why.
;; Our functions take exactly one argument, so functions that don't need one take a dummy
;; argument _ and are called with 0.

;; a) In C-like syntax:
;;      int x = 14;
;;      int h()  { return x; }
;;      int f()  { int x = 13; return h(); }
;;      int g()  { int x = 12; return h(); }
;;    What do f() and g() return?
(define ex-a-f
  '(let x 14
     (let h (lambda _ x)
       (let f (lambda _ (let x 13 (h 0)))
         (let g (lambda _ (let x 12 (h 0)))
           (f 0))))))

(define ex-a-g
  '(let x 14
     (let h (lambda _ x)
       (let f (lambda _ (let x 13 (h 0)))
         (let g (lambda _ (let x 12 (h 0)))
           (g 0))))))

;; static: h's x is the x in scope where h was written, the global one.
;; dynamic: h's x is the most recent binding when h is called, the one in f (or g).
(test (eval-static ex-a-f) (numV 14))
(test (eval-dynamic ex-a-f) (numV 13))
(test (eval-static ex-a-g) (numV 14))
(test (eval-dynamic ex-a-g) (numV 12))

;; b) In C-like syntax:
;;      const int b = 5;
;;      int foo() { int a = b + 5; return a; }
;;      int bar() { int b = 2; return foo(); }
;;    What does bar() return?
(define ex-b
  '(let b 5
     (let foo (lambda _ (let a (+ b 5) a))
       (let bar (lambda _ (let b 2 (foo 0)))
         (bar 0)))))

(test (eval-static ex-b) (numV 10))
(test (eval-dynamic ex-b) (numV 7))

;; c) f uses a variable y that isn't bound where f is written.
(define ex-c
  '(let f (lambda _ y)
     (let y 3
       (f 0))))

;; static: y is unbound in f's body, so it's an error.
;; dynamic: it "works", because some y happens to be bound when f is called.
(test/exn (eval-static ex-c) "unbound")
(test (eval-dynamic ex-c) (numV 3))

;; d) A function that returns a function.
(define ex-d
  '(let make-adder (lambda n (lambda m (+ n m)))
     (let add5 (make-adder 5)
       (add5 1))))

;; static: add5's closure remembers n = 5.
;; dynamic: n = 5 was only bound while make-adder was running; by the time add5 is
;; called it's gone. Dynamic scope breaks functions that return functions.
(test (eval-static ex-d) (numV 6))
(test/exn (eval-dynamic ex-d) "unbound")

;; e) Same as d, but now some other n is around when add5 is called.
(define ex-e
  '(let make-adder (lambda n (lambda m (+ n m)))
     (let add5 (make-adder 5)
       (let n 100
         (add5 1)))))

(test (eval-static ex-e) (numV 6))
(test (eval-dynamic ex-e) (numV 101))

;; 4. Pen and paper: scope with assignment
;; Our language has no assignment (x = ...), but most languages do. For each program, what
;; does it print under static scope? Under dynamic scope?

;; a)  int x;
;;     void h() { printf("%d\n", x); }
;;     void f() { x = 13; h(); }
;;     void g() { x = 12; h(); }
;;     int main() { x = 14; f(); g(); }
;;
;; Answer: 13, 12 under both. f and g assign to the global x instead of declaring a
;; new local one, so there is only one x and scope doesn't matter. (Compare with 3a.)

;; b)  This one is bash, which really is dynamically scoped. You can run it in a terminal.
;;
;;     x=1
;;     function g () { echo $x ; x=2 ; }
;;     function f () { local x=3 ; g ; }
;;     f
;;     echo $x
;;
;; Answer: static: 1, 2. g's x is the global x: it prints 1, then sets the global x to 2.
;;         dynamic (what bash actually prints): 3, 1. When g runs, the most recent x is f's
;;         local x = 3, so g prints 3 and sets f's local x to 2. The global x is still 1.

;; c)  From Scott, Programming Language Pragmatics, ch. 3:
;;
;;     n : integer                 -- global
;;     procedure first()
;;         n := 1
;;     procedure second()
;;         n : integer             -- local
;;         first()
;;     n := 2
;;     if read_integer() > 0
;;         second()
;;     else
;;         first()
;;     write_integer(n)
;;
;; Answer: static: always 1. first always assigns the global n.
;;         dynamic: depends on the input! If it is positive, second runs and first assigns
;;         second's local n, so the global n stays 2 and we print 2. Otherwise first assigns
;;         the global n and we print 1. With dynamic scope, which variable a name refers to
;;         can depend on the input.
