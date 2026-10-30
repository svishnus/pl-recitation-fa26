#lang scribble/lp2
@(require scribble/bnf)
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))


@chunk[<*>
       (require racket)
       (require test-engine/racket-tests)
       <is-lc>
       <is-lc-tests>
       <rename>
       <rename-tests>
       <equiv>
       <equiv-tests>
       <subst>
       <subst-tests>
       <step>
       <step-tests>
       <interp>
       <interp-tests>
       <bools>
       <if>
       <if-tests>
       <not-and>
       <not-and-tests>
       <nats>
       <nat-ops>
       <nat-ops-tests>
       <pred-zero>
       <pred-zero-tests>
       <sugar>
       <Y>
       <fact-gen>
       <fact-tests>
       (test)
       ]

@title{The Lambda Calculus}
Our interpreters keep growing: numbers, booleans, @racket[let], boxes, @racket[rec].
Today we go the other way. We start from a language with @emph{only} functions,
and see how much of the rest we can build out of them.

That language is the @emph{lambda calculus} (LC). It has three kinds of terms:
@BNF[(list @nonterm{LC}
           @BNF-seq[@litchar{var} @nonterm{symbol}]
           @BNF-seq[@litchar{lam} @nonterm{symbol} @nonterm{LC}]
           @BNF-seq[@litchar{app} @nonterm{LC} @nonterm{LC}])]

We write LC terms as s-expressions. For example, the identity function
@tt{λx. x} is @racket['(lam x (var x))], and applying it to @tt{y} is
@racket['(app (lam x (var x)) (var y))].

We build and take apart terms with @emph{quasiquote}.
@racket[`(app ,e1 ,e2)] builds the list @racket[(list 'app e1 e2)]:
the backquote quotes the list, and each comma plugs in a Racket value.
In a @racket[match] pattern, the same notation takes a list apart:
@racket[`(app ,e1 ,e2)] matches any three-element list starting with @racket['app]
and binds @racket[e1] and @racket[e2].

@section{Given: Syntax and Substitution}
Everything in this section is given. Read through it, since the rest of the recitation uses it.

@racket[is-lc?] checks that an s-expression is an LC term:

@chunk[<is-lc>
       (define (is-lc? lc)
         (match lc
           [`(var ,(? symbol?)) #t]
           [`(lam ,(? symbol?) ,body) (is-lc? body)]
           [`(app ,e1 ,e2) (and (is-lc? e1) (is-lc? e2))]
           [_ #f]))]

@chunk[<is-lc-tests>
       (check-expect (is-lc? '(app (lam x (var x)) (var y))) #t)
       (check-expect (is-lc? '(lam x x)) #f)
       (check-expect (is-lc? '(var 5)) #f)]

@subsection{Renaming}
@racket[(rename-var lc x x-new)] renames the @emph{free} occurrences of @racket[x] in @racket[lc]
to @racket[x-new]. It stops at a @racket[lam] that binds @racket[x] again, since the
@racket[x]s under it refer to that inner binder.

@racket[x-new] must be @emph{fresh}: it must not appear anywhere in @racket[lc].
Otherwise it could be captured: renaming @racket[x] to @racket[y] in
@racket['(lam y (var x))] would give @racket['(lam y (var y))], a different function.
We only call @racket[rename-var] with names made by @racket[gensym], which are always fresh.

@chunk[<rename>
       (define (rename-var lc x x-new)
         (match lc
           [`(var ,v) (if (eq? v x) `(var ,x-new) lc)]
           [`(lam ,v ,body)
            (if (eq? v x)
                lc
                `(lam ,v ,(rename-var body x x-new)))]
           [`(app ,e1 ,e2) `(app ,(rename-var e1 x x-new) ,(rename-var e2 x x-new))]))]

@chunk[<rename-tests>
       (check-expect (rename-var '(var x) 'x 'y) '(var y))
       (check-expect (rename-var '(var z) 'x 'y) '(var z))
       (check-expect (rename-var '(lam x (var x)) 'x 'y) '(lam x (var x)))
       (check-expect (rename-var '(lam y (var z)) 'z 'w) '(lam y (var w)))
       (check-expect (rename-var '(app (var x) (var y)) 'x 'z) '(app (var z) (var y)))]

@subsection{Alpha-equivalence}
@racket['(lam x (var x))] and @racket['(lam y (var y))] are the same function:
they differ only in the name of the bound variable. We say they are
@emph{alpha-equivalent}. @racket[equiv-LC] checks this by renaming both binders
to the same fresh name and comparing the bodies.

@chunk[<equiv>
       (define (equiv-LC lc1 lc2)
         (match* (lc1 lc2)
           [(`(var ,x1) `(var ,x2)) (eq? x1 x2)]
           [(`(lam ,x1 ,body1) `(lam ,x2 ,body2))
            (let ([x-new (gensym)])
              (equiv-LC (rename-var body1 x1 x-new) (rename-var body2 x2 x-new)))]
           [(`(app ,e11 ,e12) `(app ,e21 ,e22))
            (and (equiv-LC e11 e21) (equiv-LC e12 e22))]
           [(_ _) #f]))]

@chunk[<equiv-tests>
       (check-expect (equiv-LC '(lam x (var x)) '(lam y (var y))) #t)
       (check-expect (equiv-LC '(lam x (var y)) '(lam y (var y))) #f)
       (check-expect (equiv-LC '(app (var f) (var a)) '(app (var f) (var a))) #t)
       (check-expect (equiv-LC '(app (var f) (var a)) '(app (var f) (var f))) #f)
       (check-expect (equiv-LC '(lam x (lam y (app (var x) (var y))))
                               '(lam y (lam x (app (var y) (var x)))))
                     #t)]

@subsection{Substitution}
@racket[(subst lc x arg)] replaces the free occurrences of @racket[x] in @racket[lc]
with @racket[arg], avoiding capture.

To avoid capture, whenever @racket[subst] goes under a @racket[lam], it renames the binder
to a fresh name made by @racket[gensym]. It does this every time, even when no capture
could happen. That keeps the code short: we never have to work out which variables are
free in @racket[arg]. The cost is that results contain names like @racket[y1825], so in tests we
compare terms with @racket[equiv-LC] instead of @racket[equal?].

@chunk[<subst>
       (define (subst lc x arg)
         (match lc
           [`(var ,v) (if (eq? v x) arg lc)]
           [`(lam ,v ,body)
            (if (eq? v x)
                lc
                (let ([v-new (gensym v)])
                  `(lam ,v-new ,(subst (rename-var body v v-new) x arg))))]
           [`(app ,e1 ,e2) `(app ,(subst e1 x arg) ,(subst e2 x arg))]))]

The last two tests are the capture case: substituting @racket['(var y)] for @racket[x]
in @racket['(lam y (var x))] must not produce the identity function.

@chunk[<subst-tests>
       (check-expect (subst '(var x) 'x '(var z)) '(var z))
       (check-expect (subst '(var y) 'x '(var z)) '(var y))
       (check-expect (subst '(app (var x) (var x)) 'x '(var z)) '(app (var z) (var z)))
       (check-expect (subst '(lam x (var x)) 'x '(var z)) '(lam x (var x)))
       (check-expect (equiv-LC (subst '(lam y (var x)) 'x '(var y)) '(lam w (var y))) #t)
       (check-expect (equiv-LC (subst '(lam y (var x)) 'x '(var y)) '(lam y (var y))) #f)]

@section{Evaluation}
A @emph{redex} is a function applied to an argument: @racket[`(app (lam ,x ,body) ,arg)].
Reducing it means substituting @racket[arg] for @racket[x] in @racket[body].
A term with no redex left anywhere in it is in @emph{normal form}; that is our answer.

Our interpreter evaluates in @emph{normal order}:
@itemlist[
 @item{Reduce the @emph{outermost} redex first. In @racket['(app (lam x (var z)) (app (lam y (var y)) (var b)))],
       we reduce the whole term to @racket['(var z)] without touching the argument.
       Our earlier interpreters (and Racket) would evaluate the argument first.}
 @item{Among subterms, go @emph{left to right}.}
 @item{Also reduce @emph{under} @racket[lam]. In our earlier interpreters a function was a value and
       we never looked inside it. Here, @racket['(lam x (app (lam y (var y)) (var x)))] steps to
       @racket['(lam x (var x))].}]

@exercise{One step}
@racket[(step-LC lc)] finds the redex that normal order picks, reduces it, and returns the new term.
If @racket[lc] has no redex, it returns @racket[#f].

Start by replacing each @racket['???] in the tests below with the expected result.

Hint: for @racket[`(app ,e1 ,e2)] that is not a redex, try to step @racket[e1].
If that returns @racket[#f], try @racket[e2].
The order of the @racket[match] cases matters: the redex case must come before the general @racket[app] case.

@chunk[<step>
       (define (step-LC lc)
         (match lc
           [`(app (lam ,x ,body) ,arg) 'TODO]
           [`(app ,e1 ,e2) 'TODO]
           [`(lam ,x ,body) 'TODO]
           [`(var ,_) 'TODO]))]

@chunk[<step-tests>
       (check-expect (step-LC '(app (lam x (var x)) (var y)))
                     '???)
       (check-expect (step-LC '(var x))
                     '???)
       (check-expect (step-LC '(app (var f) (var a)))
                     '???)
       (check-expect (step-LC '(app (lam x (var z)) (app (lam y (var y)) (var b))))
                     '???)
       (check-expect (step-LC '(app (app (lam x (var x)) (var a)) (app (lam y (var y)) (var b))))
                     '???)
       (check-expect (step-LC '(lam x (app (lam y (var y)) (var x))))
                     '???)]

@exercise{Normal form}
@racket[(interp-LC lc)] keeps stepping until there is no redex left, and returns the normal form.
If your tests never finish, press @onscreen{Stop}: some terms have no normal form (see below).

@chunk[<interp>
       (define (interp-LC lc)
         'TODO)]

@racket[omega] applies @tt{λw. w w} to itself. Reducing it gives back @racket[omega],
so it has no normal form: @racket[(interp-LC omega)] runs forever.

@chunk[<interp-tests>
       (define omega
         '(app (lam w (app (var w) (var w))) (lam w (app (var w) (var w)))))
       (check-expect (step-LC omega) omega)
       (check-expect (interp-LC '(app (app (lam x (lam y (var x))) (var a)) (var b)))
                     '(var a))
       (check-expect (interp-LC `(app (lam x (var z)) ,omega))
                     '(var z))
       (check-expect (equiv-LC (interp-LC '(app (lam x (lam y (app (var x) (var y)))) (var y)))
                               '(lam z (app (var y) (var z))))
                     #t)]

@nested[#:style 'inset]{
 Discuss with the classmate to your @emph{left}:
 What would an interpreter that evaluates arguments first (like Racket) do with
 @racket[`(app (lam x (var z)) ,omega)]?
}

@section{Booleans}
LC has no booleans, but we can @emph{encode} them as functions.
A boolean is a function that chooses between two arguments:
@tt{true} picks the first, @tt{false} picks the second.
@racket[NF-to-bool] reads a normal form back as a Racket boolean.

@chunk[<bools>
       (define LC-true  '(lam x (lam y (var x))))
       (define LC-false '(lam x (lam y (var y))))

       (define (NF-to-bool nf)
         (cond
           [(equiv-LC nf LC-true) #t]
           [(equiv-LC nf LC-false) #f]
           [else (error 'NF-to-bool "not a boolean")]))]

@exercise{If}
Define @racket[LC-if]: a term that takes a boolean @racket[c] and two terms
@racket[t] and @racket[e], and returns @racket[t] if @racket[c] is @tt{true} and @racket[e] otherwise.

@chunk[<if>
       (define LC-if
         'TODO)]

@chunk[<if-tests>
       (check-expect (NF-to-bool (interp-LC LC-true)) #t)
       (check-expect (NF-to-bool (interp-LC LC-false)) #f)
       (check-expect (interp-LC `(app (app (app ,LC-if ,LC-true) (var yes)) (var no)))
                     '(var yes))
       (check-expect (interp-LC `(app (app (app ,LC-if ,LC-false) (var yes)) (var no)))
                     '(var no))]

@exercise{Not, And}
Define @racket[LC-not] and @racket[LC-and] in the same style.
You can use @racket[LC-true] and @racket[LC-false] inside them with quasiquote.

@chunk[<not-and>
       (define LC-not
         'TODO)
       (define LC-and
         'TODO)]

@chunk[<not-and-tests>
       (check-expect (NF-to-bool (interp-LC `(app ,LC-not ,LC-true))) #f)
       (check-expect (NF-to-bool (interp-LC `(app ,LC-not ,LC-false))) #t)
       (check-expect (NF-to-bool (interp-LC `(app (app ,LC-and ,LC-true) ,LC-true))) #t)
       (check-expect (NF-to-bool (interp-LC `(app (app ,LC-and ,LC-true) ,LC-false))) #f)
       (check-expect (NF-to-bool (interp-LC `(app (app ,LC-and ,LC-false) ,LC-true))) #f)
       (check-expect (NF-to-bool (interp-LC `(app (app ,LC-and ,LC-false) ,LC-false))) #f)]

@section{Numbers}
We encode the natural number @tt{n} as the function that applies @tt{f} to @tt{x}, @tt{n} times.
These are called @emph{Church numerals}:
@verbatim{
  0 = λf. λx. x
  1 = λf. λx. f x
  2 = λf. λx. f (f x)
  3 = λf. λx. f (f (f x))
}
@racket[LC-nat] turns a Racket number into a numeral, and @racket[NF-to-nat] reads a normal form back.

@chunk[<nats>
       (define (LC-nat n)
         (define (apply-f k)
           (if (zero? k)
               '(var x)
               `(app (var f) ,(apply-f (- k 1)))))
         `(lam f (lam x ,(apply-f n))))

       (define (NF-to-nat nf)
         (define (count-f f x body)
           (match body
             [`(var ,v) #:when (eq? v x) 0]
             [`(app (var ,v) ,inner) #:when (eq? v f) (+ 1 (count-f f x inner))]
             [_ (error 'NF-to-nat "not a numeral")]))
         (match nf
           [`(lam ,f (lam ,x ,body)) (count-f f x body)]
           [_ (error 'NF-to-nat "not a numeral")]))]

@exercise{Arithmetic}
Define @racket[LC-succ], @racket[LC-add], and @racket[LC-mul].
Each takes numerals and returns a numeral, i.e. something of the form @tt{λf. λx. ...}.

Hints:
@itemlist[
 @item{@tt{n f x} applies @tt{f} to @tt{x}, @tt{n} times. @racket[LC-succ] applies it once more.}
 @item{@tt{m + n}: apply @tt{f} @tt{n} times, then @tt{m} more times.}
 @item{@tt{m * n}: @tt{n f} is a function that applies @tt{f} @tt{n} times. Apply @emph{that} @tt{m} times.}]

@chunk[<nat-ops>
       (define LC-succ
         'TODO)
       (define LC-add
         'TODO)
       (define LC-mul
         'TODO)]

@chunk[<nat-ops-tests>
       (check-expect (NF-to-nat (LC-nat 5)) 5)
       (check-expect (NF-to-nat (interp-LC `(app ,LC-succ ,(LC-nat 0)))) 1)
       (check-expect (NF-to-nat (interp-LC `(app ,LC-succ ,(LC-nat 1)))) 2)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-add ,(LC-nat 6)) ,(LC-nat 4)))) 10)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-add ,(LC-nat 0)) ,(LC-nat 0)))) 0)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-mul ,(LC-nat 5)) ,(LC-nat 3)))) 15)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-mul ,(LC-nat 0)) ,(LC-nat 7)))) 0)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-mul ,(LC-nat 3)) ,(LC-nat 0)))) 0)]

@subsection{Given: Predecessor and Zero Test}
@racket[LC-is-zero] applies the function that always returns @tt{false} @tt{n} times,
starting from @tt{true}. Zero applications leave @tt{true}; one or more give @tt{false}.

@racket[LC-pred] is famously tricky, so it is given.
It builds the result one @tt{f} at a time, but its starting value @tt{λu. x} throws away the first @tt{f}.
So @tt{n} copies of @tt{f} become @tt{n - 1}. The predecessor of 0 is 0.
See @hyperlink["https://en.wikipedia.org/wiki/Church_encoding"]{Church encoding} on Wikipedia for the details.

@chunk[<pred-zero>
       (define LC-is-zero
         `(lam n (app (app (var n) (lam u ,LC-false)) ,LC-true)))
       (define LC-pred
         '(lam n (lam f (lam x (app (app (app (var n)
                                              (lam g (lam h (app (var h) (app (var g) (var f))))))
                                         (lam u (var x)))
                                    (lam u (var u)))))))]

@chunk[<pred-zero-tests>
       (check-expect (NF-to-bool (interp-LC `(app ,LC-is-zero ,(LC-nat 0)))) #t)
       (check-expect (NF-to-bool (interp-LC `(app ,LC-is-zero ,(LC-nat 1)))) #f)
       (check-expect (NF-to-bool (interp-LC `(app ,LC-is-zero ,(LC-nat 2)))) #f)
       (check-expect (NF-to-nat (interp-LC `(app ,LC-pred ,(LC-nat 4)))) 3)
       (check-expect (NF-to-nat (interp-LC `(app ,LC-pred ,(LC-nat 0)))) 0)]

@section{Challenge: Recursion with Y}
Last week, @racket[Y] looped forever in Racket, and we needed @racket[Z] instead.
Let's try @racket[Y] again, this time in LC.

These helpers are ordinary Racket functions that build bigger LC terms, so we don't have to
write out every @racket[app] by hand. For example, @racket[(SLC-if c t e)] builds the term
@racket[`(app (app (app ,LC-if ,c) ,t) ,e)].
Their arguments are LC terms too: write @racket[(SLC-zero? '(var n))], not @racket[(SLC-zero? 'n)].
@racket[SLC-if] and @racket[SLC-mul] use your @racket[LC-if] and @racket[LC-mul],
so if the factorial tests fail, check those first.

@chunk[<sugar>
       (define (SLC-if c t e)  `(app (app (app ,LC-if ,c) ,t) ,e))
       (define (SLC-zero? e)   `(app ,LC-is-zero ,e))
       (define (SLC-pred e)    `(app ,LC-pred ,e))
       (define (SLC-mul e1 e2) `(app (app ,LC-mul ,e1) ,e2))]

@exercise{Y and factorial}
Write @racket[LC-Y], the Y-combinator from recitation 7, as an LC term:
@centered{@tt{Y = λh. (λx. h (x x)) (λx. h (x x))}}
Then write @racket[LC-fact-gen], the LC version of @racket[fact-gen]:
it takes the recursive call @racket[fact] and returns a function of @racket[n].

@chunk[<Y>
       (define LC-Y
         'TODO)]

@chunk[<fact-gen>
       (define LC-fact-gen
         'TODO)]

@chunk[<fact-tests>
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-Y ,LC-fact-gen) ,(LC-nat 0)))) 1)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-Y ,LC-fact-gen) ,(LC-nat 3)))) 6)
       (check-expect (NF-to-nat (interp-LC `(app (app ,LC-Y ,LC-fact-gen) ,(LC-nat 4)))) 24)]

@nested[#:style 'inset]{
 Discuss with the classmate to your @emph{right}:
 Why does @racket[Y] work here, when it looped in Racket last week?
}

Now try @racket[(interp-LC `(app ,LC-Y ,LC-fact-gen))], without a number. Does it finish?

Write @racket[Z] as an LC term too. Does it compute factorial in LC?
What about @racket[Z] applied to @racket[LC-fact-gen] alone, without a number?

