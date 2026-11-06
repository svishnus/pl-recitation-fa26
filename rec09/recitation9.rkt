#lang scribble/lp2
@(require scribble/bnf)
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))

@chunk[<*>
       (require racket)
       (require test-engine/racket-tests)
       (require racket/trace)
       <parse-nt>
       <interp-basic>
       <interp-basic-tests>
       <interp-cps>
       <interp-cps-tests>
       <interp-defun>
       <interp-defun-tests>
       (test)
       ]

@title{CPS and Defunctionalization}
In lecture, the interpreter with @racket[try] and @racket[raise] looked unusual.
@racket[eval-env] took an extra argument @racket[k] of type @racket[Contin],
every case ended by calling @racket[eval-env] or @racket[apply-contin], and
@racket[find-handler] handled a @racket[raise] by walking along @racket[k]
to find the nearest @racket[try].
Today we see where that style comes from, by deriving it step by step on a much smaller language.
We use two program transformations:
@itemlist[
 #:style 'compact
 @item{the @emph{continuation-passing style} (CPS) transformation, which makes every call a tail call, and}
 @item{@emph{defunctionalization}, which turns the continuation functions into plain data.}]

@section{NT-lang: Number Trees}
Our language is a binary tree with numbers at the leaves.
An @nonterm{NT-lang} program interprets to a number: the sum of all the leaves of the tree.

@BNF[(list @nonterm{n}
           @elem{@racket[number?]})
     (list @nonterm{NT-lang}
           @BNF-seq[@litchar{(nt-num} @nonterm{n} @litchar{)}]
           @BNF-seq[@litchar{(nt-add} @nonterm{NT-lang} @nonterm{NT-lang} @litchar{)}])]

To save some typing, here is a parser from nested two-element lists of numbers into @nonterm{NT-lang}.
For instance, @racket['(1 (2 2))] parses to
@racket['(nt-add (nt-num 1) (nt-add (nt-num 2) (nt-num 2)))].

@chunk[<parse-nt>
       (define (parse-nt-exp e)
         (match e
           [`(,e1 ,e2) `(nt-add ,(parse-nt-exp e1) ,(parse-nt-exp e2))]
           [x #:when (number? x) `(nt-num ,x)]
           [_ (error 'bad-parse "bad parse")]))]

Here is the interpreter you would write without thinking about it:

@chunk[<interp-basic>
       (define (interp-nt-lang-basic n)
         (match n
           [`(nt-num ,x) x]
           [`(nt-add ,n1 ,n2)
            (let* ([v1 (interp-nt-lang-basic n1)]
                   [v2 (interp-nt-lang-basic n2)])
              (+ v1 v2))]))]

@chunk[<interp-basic-tests>
       (check-expect (interp-nt-lang-basic (parse-nt-exp '2)) 2)
       (check-expect (interp-nt-lang-basic (parse-nt-exp '(1 (2 2)))) 5)
       (check-expect (interp-nt-lang-basic (parse-nt-exp '((3 (4 2)) (2 2)))) 13)]

This interpreter is @emph{not} tail recursive.
Neither recursive call is in tail position: after each one returns,
there is still work left to do (evaluate the right sub-tree, or add).
Racket remembers that work on its call stack, which grows with the depth of the tree.
To watch it grow, run @racket[(trace interp-nt-lang-basic)] in the interactions window, then
@racket[(interp-nt-lang-basic (parse-nt-exp '((3 (4 2)) (2 2))))].
Each level of indentation is a call that hasn't returned yet.

That works fine here, but the "work left to do" is hidden inside Racket,
where our interpreter can't look at it. The lecture's @racket[find-handler] needs to look at it.

@section{The CPS Transformation}
In continuation-passing style, we represent the state of our program as two things:
@itemlist[
 #:style 'ordered
 @item{something we're @emph{"working on"}, and}
 @item{something @emph{"to do with the result"} (the @emph{continuation}).}]

@exercise{What are we working on?}
@bold{Q1.} When we are interpreting an @nonterm{NT-lang} program, what are we "working on"?

@bold{Q2.} When we are interpreting an @nonterm{NT-lang} program, what might we have to "do with the result"?

@exercise{A CPS interpreter}
The inner function @racket[interp-cps] takes the expression @racket[n] we are "working on"
and a function @racket[kont] that says what to do with its result.
The @racket[nt-num] case is done for you: there's nothing left to compute, so hand
@racket[x] to the continuation. Fill in the @racket[nt-add] case.
Every call you make should be a tail call!

@chunk[<interp-cps>
       (define (interp-nt-lang-cps n)
         (define (interp-cps n kont)
           (match n
             [`(nt-num ,x)      (kont x)]
             [`(nt-add ,n1 ,n2) 'TODO]))
         (interp-cps n (lambda (v) v)))]

@chunk[<interp-cps-tests>
       (check-expect (interp-nt-lang-cps (parse-nt-exp '2)) 2)
       (check-expect (interp-nt-lang-cps (parse-nt-exp '(1 (2 2)))) 5)
       (check-expect (interp-nt-lang-cps (parse-nt-exp '((3 (4 2)) (2 2)))) 13)]

@bold{Q3.} Suppose we interpret the tree @racket[(nt-add B C)], where @racket[B] and @racket[C] are any two trees.
The first call is @racket[(interp-cps (nt-add B C) (lambda (v) v))].
Write out the second call to @racket[interp-cps], with its continuation written out in full.

@bold{Q4.} Is @racket[interp-cps] tail recursive?
Is it more efficient than @racket[interp-nt-lang-basic]?

@bold{Q5.} In @racket[interp-nt-lang-basic], the pending additions lived on the call stack.
Where are they now?

@section{Defunctionalization}
Our continuations are Racket functions, so the only thing we can do with one is call it.
We can't look inside one and ask, say, "is there a @racket[try] waiting for my result?"

Defunctionalization turns the continuation into @emph{data}.
Each @racket[lambda] we passed as a continuation above becomes one case of a datatype
@nonterm{NT-kont}, a representation of "the things left to do".
The free variables of each @racket[lambda] become the fields of its case.

@exercise{Find the continuations}
@bold{Q6.} List every @racket[lambda] that your @racket[interp-nt-lang-cps] creates as a continuation
(don't forget the initial one). For each, what are its free variables?
What would you name it?

Compare your answer with ours:

@BNF[(list @nonterm{NT-kont}
           @litchar{(ntk-done)}
           @BNF-seq[@litchar{(ntk-done-left} @nonterm{NT-lang} @nonterm{NT-kont} @litchar{)}]
           @BNF-seq[@litchar{(ntk-done-both} @nonterm{n} @nonterm{NT-kont} @litchar{)}])]

@itemlist[
 @item{@racket[(ntk-done)] is the initial continuation @racket[(lambda (v) v)]: nothing is left to do.}
 @item{@racket[(ntk-done-left expr-right kont-rec)] is the continuation we use while evaluating the
  left sub-tree. When the left sub-tree is done, evaluate @racket[expr-right] next.}
 @item{@racket[(ntk-done-both val-left kont-rec)] is the continuation we use while evaluating the
  right sub-tree. It remembers that the left sub-tree evaluated to @racket[val-left].
  When the right sub-tree is done, both are done: add them up and pass the sum to @racket[kont-rec].}]

In both of the last two, @racket[kont-rec] is the continuation of the whole @racket[nt-add]:
what to do once we have the sum.

These are the lecture's @racket[haltK], @racket[plusLK] and @racket[plusRK].
The lecture's versions also carry an @racket[env], because that language has variables.

@exercise{Applying a continuation}
Instead of calling @racket[kont], we now call @racket[(interp-kont kont val)], which
pattern matches on the continuation to decide what to do with @racket[val] next.
This is the lecture's @racket[apply-contin], and @racket[interp-exp] is its @racket[eval-env].
@racket[interp-exp] is done for you.
Fill in the cases of @racket[interp-kont]: each one should do what the body of the
corresponding @racket[lambda] did in @racket[interp-nt-lang-cps].

@chunk[<interp-defun>
       (define (interp-nt-lang-defun n)
         (define (interp-kont kont val)
           (match kont
             [`(ntk-done)                            'TODO]
             [`(ntk-done-left ,expr-right ,kont-rec) 'TODO]
             [`(ntk-done-both ,val-left ,kont-rec)   'TODO]))

         (define (interp-exp n kont)
           (match n
             [`(nt-num ,x)      (interp-kont kont x)]
             [`(nt-add ,n1 ,n2) (interp-exp n1 `(ntk-done-left ,n2 ,kont))]))

         (interp-exp n `(ntk-done)))]

@chunk[<interp-defun-tests>
       (check-expect (interp-nt-lang-defun (parse-nt-exp '2)) 2)
       (check-expect (interp-nt-lang-defun (parse-nt-exp '(1 (2 2)))) 5)
       (check-expect (interp-nt-lang-defun (parse-nt-exp '((3 (4 2)) (2 2)))) 13)]

@bold{Q7.} Are @racket[interp-kont] and @racket[interp-exp] tail recursive?
Where are the pending additions now?

@bold{Q8.} Is there anything stopping you from writing this interpreter as a loop in a language without closures, like C?

@bold{Q9.} The lecture's @racket[find-handler] handles a @racket[raise] by walking along the continuation,
skipping cases until it reaches a @racket[tryRK]. Why couldn't we do that with the
continuations in @racket[interp-nt-lang-cps]?
