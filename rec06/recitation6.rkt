#lang plai-typed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 6: Mutable state and tables                                                                  ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In lecture 6 we added mutable state to our language: first boxes (lecture6c.rkt), then
;; mutable variables with set! (lecture6d.rkt). Both work by passing a *store* through the
;; interpreter: eval-env takes a store, and returns a value together with the updated store.
;;
;; Today:
;;   1. Warm-up: lecture's examples, and evaluation order.
;;   2. Add tables (key-value maps) to the language: helper functions.
;;   3. Tables in the interpreter.
;;   4. Tables vs. boxes: what can change, and what can't?
;;   5. Challenge: passing the store with monads (lecture6f.rkt).

;; 0. The language
;; Same as lecture6d.rkt (numbers, +, *, let, lambda, application, boxes, set!, begin),
;; plus four new forms for tables:
;;
;;   (make-table (k1 e1) (k2 e2) ...)   a new table mapping each key ki to the value of ei
;;   (get-table t k)                     the value for key k in table t
;;   (set-table t k v)                   a *new* table: like t, but with k mapped to v
;;   (remove-table t k)                  a *new* table: like t, but without k
;;
;; Keys are symbols written directly in the program (like field names), not expressions.
;; Tables never change: set-table and remove-table leave t alone and return a new table,
;; just like Racket's hash-set and hash-remove.

(define-type Entry
  [entry (k : symbol) (e : Expr)])

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [boxC (e : Expr)]
  [unboxC (e : Expr)]
  [setboxC (e1 : Expr) (e2 : Expr)]
  [setC (x : symbol) (e2 : Expr)]
  [seqC (e1 : Expr) (e2 : Expr)]
  [maketableC (entries : (listof Entry))]
  [gettableC (tbl : Expr) (k : symbol)]
  [settableC (tbl : Expr) (k : symbol) (val : Expr)]
  [removetableC (tbl : Expr) (k : symbol)])

(define-type-alias Table (hashof symbol Value))

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  [tableV (t : Table)])

;; Parses one (k e) pair of a make-table.
(define (parse-entry (s : s-expression)) : Entry
  (let ([l (s-exp->list s)])
    (entry (s-exp->symbol (first l)) (parse (second l)))))

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
            [(begin) (seqC (parse (second l)) (parse (third l)))]
            [(box) (boxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(set!) (setC (s-exp->symbol (second l)) (parse (third l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(make-table) (maketableC (map parse-entry (rest l)))]
            [(get-table) (gettableC (parse (second l)) (s-exp->symbol (third l)))]
            [(set-table) (settableC (parse (second l)) (s-exp->symbol (third l)) (parse (fourth l)))]
            [(remove-table) (removetableC (parse (second l)) (s-exp->symbol (third l)))]
            [else (appC (parse (first l)) (parse (second l)))])]
         [else (appC (parse (first l)) (parse (second l)))]))]))

;; Environments map variables to locations, and the store maps locations to values.
(define-type-alias Location number)

(define-type Binding
  [bind (name : symbol) (loc : Location)])
(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) (val : Value)])
(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No location found")]))

(define (lookup (x : symbol) (env : Env)) : Location
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-loc (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

(define-type Result
  [res (v : Value) (s : Store)])

(define new-loc
  (let ([counter (box 0)])
    (lambda ()
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))

;; The interpreter: lecture6d.rkt's eval-env, plus the four table cases at the bottom.
;; Every case evaluates its subexpressions left to right, passing the store along.
(define (eval-env (env : Env) (sto : Store) (e : Expr)) : Result
  (type-case Expr e
    [numC (n) (res (numV n) sto)]
    [lambdaC (x e) (res (closV env x e) sto)]
    [plusC (e1 e2)
           (type-case Result (eval-env env sto e1)
             [res (v1 sto-1)
                  (type-case Result (eval-env env sto-1 e2)
                    [res (v2 sto-2)
                         (res (numV (+ (numV-n v1) (numV-n v2)))
                              sto-2)])])]
    [timesC (e1 e2)
            (type-case Result (eval-env env sto e1)
              [res (v1 sto-1)
                   (type-case Result (eval-env env sto-1 e2)
                     [res (v2 sto-2)
                          (res (numV (* (numV-n v1) (numV-n v2)))
                               sto-2)])])]
    [letC (x e1 e2) (eval-env env sto (appC (lambdaC x e2) e1))]
    [appC (e1 e2)
          (type-case Result (eval-env env sto e1)
            [res (v1 sto-1)
                 (type-case Result (eval-env env sto-1 e2)
                   [res (v2 sto-2)
                        (let ([l (new-loc)])
                          (eval-env
                           (extend-env (bind (closV-x v1) l) (closV-env v1))
                           (override-store (cell l v2) sto-2)
                           (closV-e v1)))])])]
    [setC (x e)
          (type-case Result (eval-env env sto e)
            [res (v sto-1)
                 (let [(l (lookup x env))]
                   (res v (override-store (cell l v) sto-1)))])]
    [seqC (e1 e2)
          (type-case Result (eval-env env sto e1)
            [res (v1 sto-1)
                 (eval-env env sto-1 e2)])]
    [idC (x) (res (fetch (lookup x env) sto) sto)]
    [boxC (a)
          (type-case Result (eval-env env sto a)
            [res (v sto-1)
                 (let [(l (new-loc))]
                   (res (boxV l) (override-store (cell l v) sto-1)))])]
    [unboxC (a)
            (type-case Result (eval-env env sto a)
              [res (v sto-1)
                   (res (fetch (boxV-l v) sto-1) sto-1)])]
    [setboxC (e1 e2)
             (type-case Result (eval-env env sto e1)
               [res (v1 sto-1)
                    (type-case Result (eval-env env sto-1 e2)
                      [res (v2 sto-2)
                           (res v2 (override-store (cell (boxV-l v1) v2) sto-2))])])]

    ;; Section 3: fill these in once you've written the helpers in section 2.
    ;; Use make-table, starting from an empty table.
    [maketableC (entries) (error 'eval-env "TODO")]
    ;; Evaluate tbl, and use expect-table to get the table out of its value.
    ;; Then look up k with get-table.
    [gettableC (tbl k) (error 'eval-env "TODO")]
    ;; Evaluate tbl, then val, passing the store from one to the other.
    ;; Return a tableV holding the new table.
    [settableC (tbl k val) (error 'eval-env "TODO")]
    ;; Evaluate tbl, and return a tableV holding the table without k.
    [removetableC (tbl k) (error 'eval-env "TODO")]))

(define (eval (e : Expr)) : Result
  (eval-env empty-env empty-store e))

;; Parses and evaluates a program, and returns just its value.
(define (run (s : s-expression)) : Value
  (res-v (eval (parse s))))

;; 1. Warm-up: references
;; These are the examples from lecture 6. Before running, predict what each one evaluates to.

(define ex0 '(let v (+ 1 1) (+ v v)))
(define ex1 '(begin (+ 1 1) (+ 2 2)))
(define ex2 '(let b (box 0) (set-box! b 1)))
(define ex3 '(let b (box 0) (unbox b)))
(define ex4 '(let b (box 0) (begin (begin (set-box! b 1)
                                         (set-box! b (+ 1 (unbox b))))
                                  (unbox b))))
(define ex5 '(let a (box 1)
               (let f (lambda x (+ x (unbox a)))
                 (begin
                   (set-box! a 2)
                   (f 10)))))
(define ex6 '(let v (+ 1 1) (begin (set! v 3) (+ v v))))

(test (run ex0) (numV 4))
(test (run ex1) (numV 4))
(test (run ex2) (numV 1))   ; set-box! returns the new contents
(test (run ex3) (numV 0))
(test (run ex4) (numV 2))
(test (run ex5) (numV 12))  ; f sees the box's contents when it is called, not when it was made
(test (run ex6) (numV 6))

;; Evaluation order: our interpreter evaluates e1 before e2 in (+ e1 e2), and passes the store
;; from e1 on to e2. When subexpressions have side effects, the order is visible:
;;   left to right:  (begin (set-box! b 5) 4) runs first, so (unbox b) sees 5:  4 + 5 = 9
;;   right to left:  (unbox b) runs first and sees 10:                          4 + 10 = 14
(define ex-order
  '(let b (box 10)
     (+ (begin (set-box! b 5) 4)
        (unbox b))))

(test (run ex-order) (numV 9))

;; 2. Table helpers
;; A Table is a Racket hash from keys to values. Useful functions:
;;   (hash empty)       an empty hash
;;   (hash-ref t k)     (some v) if k maps to v in t, (none) otherwise
;;   (hash-set t k v)   a new hash: like t, but with k mapped to v
;;   (hash-remove t k)  a new hash: like t, but without k

;; Returns the table inside v, or signals an error if v isn't a table.
;; Use the error message "expected a table".
;; Hint: type-case can have an else case, for "every other kind of value":
;;   (type-case Value v
;;     [tableV (t) ...]
;;     [else ...])
(define (expect-table (v : Value)) : Table
  (error 'expect-table "TODO"))

;; Evaluates each entry's expression, left to right, passing the store along, and adds each key
;; and its value to acc. Returns the finished table, along with the final store.
;; If a key appears twice, the later entry wins.
;; Hint: look at how the plusC case of eval-env passes the store from e1 to e2. Here you do
;; the same thing, from each entry to the next.
(define (make-table (env : Env) (sto : Store) (entries : (listof Entry)) (acc : Table)) : Result
  (error 'make-table "TODO"))

;; Returns the value for key k in t, or signals an error if t has no key k.
;; Use the error message (string-append "no such key: " (symbol->string k)).
;; Hint: you can type-case on an optionof, like in recitation 5:
;;   (type-case (optionof Value) ...
;;     [some (v) ...]
;;     [none () ...])
(define (get-table (t : Table) (k : symbol)) : Value
  (error 'get-table "TODO"))

;; Returns a new table: like t, but with k mapped to v.
(define (set-table (t : Table) (k : symbol) (v : Value)) : Table
  (error 'set-table "TODO"))

;; Returns a new table: like t, but without k. Signals the same error as get-table if t has
;; no key k.
;; Hint: which helper above already signals that error?
(define (remove-table (t : Table) (k : symbol)) : Table
  (error 'remove-table "TODO"))

(test (expect-table (tableV (hash empty))) (hash empty))
(test/exn (expect-table (numV 1)) "expected a table")

(define t-ab (hash (list (values 'a (numV 1)) (values 'b (numV 2)))))
(test (get-table t-ab 'b) (numV 2))
(test/exn (get-table t-ab 'c) "no such key: c")
(test (get-table (set-table t-ab 'c (numV 3)) 'c) (numV 3))
(test (get-table (set-table t-ab 'a (numV 3)) 'a) (numV 3))
(test (get-table t-ab 'a) (numV 1))   ; t-ab itself is unchanged
(test/exn (get-table (remove-table t-ab 'a) 'a) "no such key: a")
(test/exn (remove-table t-ab 'c) "no such key: c")

;; 3. Tables in the interpreter
;; Go back to the four table cases at the bottom of eval-env and fill them in.
;; - maketableC: use make-table, starting from an empty hash.
;; - gettableC, settableC, removetableC: evaluate tbl and use expect-table on the result.
;;   In settableC, evaluate tbl *before* val, and pass the store from one to the other.

(test (run '(get-table (make-table (a 1) (b 2)) b)) (numV 2))
(test (run '(get-table (make-table (a (+ 1 2))) a)) (numV 3))
(test (run '(make-table)) (tableV (hash empty)))
(test (run '(make-table (a 1) (b 2))) (tableV t-ab))
(test (run '(get-table (make-table (a 1) (a 2)) a)) (numV 2))   ; the later entry wins
(test/exn (run '(get-table (make-table (a 1)) b)) "no such key: b")

(test (run '(get-table (set-table (make-table (a 1)) b 2) b)) (numV 2))
(test (run '(get-table (set-table (make-table (a 1)) a 2) a)) (numV 2))
(test/exn (run '(get-table (remove-table (make-table (a 1)) a) a)) "no such key: a")
(test/exn (run '(remove-table (make-table (a 1)) b)) "no such key: b")

;; Entries are evaluated in the current environment, and can hold any value: here, a table
;; and a function.
(test (run '(let y 5 (get-table (make-table (a y)) a))) (numV 5))
(test (run '(get-table (get-table (make-table (inner (make-table (x 7)))) inner) x)) (numV 7))
(test (run '(let t (make-table (inc (lambda x (+ x 1))))
              ((get-table t inc) 41)))
      (numV 42))

;; make-table passes the store from one entry to the next: the second entry sees the first
;; entry's set-box!.
(test (run '(let b (box 0)
              (get-table (make-table (x (set-box! b 1)) (y (unbox b))) y)))
      (numV 1))

;; Using something that isn't a table as a table.
(test/exn (run '(get-table 5 a)) "expected a table")
(test/exn (run '(set-table (box 1) a 2)) "expected a table")
(test/exn (run '(remove-table (lambda x x) a)) "expected a table")

;; 4. Tables vs. boxes
;; For each program, write down what you think it evaluates to *before* running it.

;; a) set-table returns a new table. What happens to t?
(define ex-a
  '(let t (make-table (a 1))
     (begin
       (set-table t a 2)
       (get-table t a))))

;; b) This time we use the table that set-table returns.
(define ex-b
  '(let t (make-table (a 1))
     (get-table (set-table t a 2) a)))

;; c) set! can change what the *variable* t refers to.
(define ex-c
  '(let t (make-table (a 1))
     (begin
       (set! t (set-table t a 2))
       (get-table t a))))

;; d) Two variables, one table. Does u see the change?
(define ex-d
  '(let t (make-table (a 1))
     (let u t
       (begin
         (set! t (set-table t a 2))
         (get-table u a)))))

;; e) A box inside a table.
(define ex-e
  '(let b (box 1)
     (let t (make-table (a b))
       (begin
         (set-box! b 5)
         (unbox (get-table t a))))))

;; f) Which runs first: the table or the value?
(define ex-f
  '(let b (box 0)
     (get-table (set-table (begin (set-box! b 1) (make-table))
                           a
                           (unbox b))
                a)))

;; Write your predictions here, then fill in the blanks below, uncomment the tests and run.
;; ex-a: ____   ex-b: ____   ex-c: ____   ex-d: ____   ex-e: ____   ex-f: ____

;; (test (run ex-a) (numV ___))
;; (test (run ex-b) (numV ___))
;; (test (run ex-c) (numV ___))
;; (test (run ex-d) (numV ___))
;; (test (run ex-e) (numV ___))
;; (test (run ex-f) (numV ___))

;; Discussion:
;; a) Another design: a table maps each key to a *location* in the store, and the store holds
;;    the value. Would that change any of the answers above? When would it matter?
;; b) Which of ex-a ... ex-f would change if tables could be changed in place, like Racket's
;;    mutable hash tables or JavaScript objects?

;; 5. Challenge: passing the store with monads
;; In lecture6f.rkt, mret and mbind did the store-passing for us, so the interpreter never
;; had to name sto-1, sto-2, ... Here they are again, for our Result type (which always holds a
;; Value).

;; A Stateful is a computation that needs a store to run, and gives back a Result.
(define-type-alias Stateful (Store -> Result))

;; A computation that just returns v, without touching the store.
(define (mret (v : Value)) : Stateful
  (lambda (sto) (res v sto)))

;; Run m, then pass its value to f, and run the computation f returns, using the store m
;; returned.
(define (mbind (m : Stateful) (f : (Value -> Stateful))) : Stateful
  (lambda (sto)
    (type-case Result (m sto)
      [res (v sto-1) ((f v) sto-1)])))

;; Our eval-env isn't written this way, but we can turn a call to it into a Stateful.
(define (meval (env : Env) (e : Expr)) : Stateful
  (lambda (sto) (eval-env env sto e)))

;; a) Rewrite make-table using mret, mbind and meval: no sto, and no type-case Result.
(define (make-table/m (env : Env) (entries : (listof Entry)) (acc : Table)) : Stateful
  (error 'make-table/m "TODO"))

;; b) Rewrite the settableC case of eval-env the same way.
(define (set-table/m (env : Env) (tbl : Expr) (k : symbol) (val : Expr)) : Stateful
  (error 'set-table/m "TODO"))

;; Uncomment the tests below when you try this.

;; To test these, we need an environment and store to run them in. Here the variable b is bound
;; to a box holding 0. (We use negative locations, so they can't clash with ones from new-loc.)
(define env-b (extend-env (bind 'b -1) empty-env))
(define sto-b (override-store (cell -1 (boxV -2))
                              (override-store (cell -2 (numV 0)) empty-store)))

;; The second entry sees the first entry's set-box!.
;; (test (get-table (expect-table
;;                   (res-v ((make-table/m env-b
;;                                         (list (entry 'x (parse '(set-box! b 1)))
;;                                               (entry 'y (parse '(unbox b))))
;;                                         (hash empty))
;;                           sto-b)))
;;                  'y)
;;       (numV 1))

;; set-table/m evaluates the table before the value, and returns the final store.
;; (define set-table/m-result
;;   ((set-table/m env-b
;;                 (parse '(begin (set-box! b 1) (make-table)))
;;                 'a
;;                 (parse '(unbox b)))
;;    sto-b))
;; (test (get-table (expect-table (res-v set-table/m-result)) 'a) (numV 1))
;; (test (fetch -2 (res-s set-table/m-result)) (numV 1))

;; c) Want more? Use make-table/m and set-table/m in eval-env's maketableC and settableC cases,
;;    and check that all the tests still pass.
