#lang scribble/lp2
@(require scribble/bnf)
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))


@chunk[<*>
       (require racket)
       (require test-engine/racket-tests)
       <self-box>
       <self-box-tests>
       <fact-box>
       <fact-box-tests>
       <circ-list>
       <circ-list-helpers>
       <circ-list-tests>
       <tree-box>
       <tree-box-tests>
       <fact-gen>
       <Y>
       <Z>
       <fact-z>
       <fact-z-tests>
       <tree-z>
       <tree-z-tests>
       (test)
       ]

@title{Recursion}
Today we look at two closely related questions:
@itemlist[
 #:style 'ordered
 @item{How do we build a @emph{circular} data structure?}
 @item{How do we let something refer to @emph{itself} in its own definition?}]

A plain @racket[let] does not help. The right-hand side is evaluated @emph{before}
the name is bound, so @racket[b] is unbound when we try to use it:

@racketblock[
 (let ([b b])
   b)]

Same story for functions. The body of the @racket[lambda] mentions @racket[fact],
but @racket[fact] is not in scope inside its own right-hand side:

@racketblock[
 (let ([fact (lambda (n)
               (if (equal? 0 n)
                   1
                   (* n (fact (- n 1)))))])
   (fact 5))]

We will check out two ways around this: tying the knot with mutation (boxes),
and fixed-point combinators (no mutation at all).

@section{Tying the Knot with Boxes}
The trick: allocate a box holding a dummy value first,
then go back and overwrite it once the thing we want to point to exists.

Here is a box that contains itself (@racket[self-box] → @racket[self-box]),
and @racket[two-cycle], two boxes that point to each other
(@racket[b1] → @racket[b2] → @racket[b1]):

@chunk[<self-box>
       (define self-box
         (let ([b (box 'dummy)])
           (begin
             (set-box! b b)
             b)))
       (define two-cycle
         (let* ([b1 (box 'dummy)]
                [b2 (box b1)])
           (begin
             (set-box! b1 b2)
             b1)))]

@chunk[<self-box-tests>
       (check-expect (eq? (unbox self-box) self-box) #t)
       (check-expect (eq? (unbox (unbox two-cycle)) two-cycle) #t)
       (check-expect (eq? (unbox two-cycle) two-cycle) #f)]

Try evaluating @racket[self-box] in the DrRacket console. How does Racket print it?

@nested[#:style 'inset]{
 It prints as @litchar{#0='#&#0#}. @tt{#&} is a box, @tt{#0=} gives the value the label 0,
 and @tt{#0#} refers back to it: a box that contains itself.
 @racket[two-cycle] prints as @litchar{#0='#&#&#0#}: a box containing a box containing the first box.
}

@exercise{Recursive functions with boxes}
Same trick, but now the box holds a @emph{function}.
Inside the body we call @racket[(unbox fact)] instead of @racket[fact]:
by the time the body actually runs, the box has been filled in with the real function.

Implement factorial this way.
Do @emph{not} use Racket's @racket[define] to recurse: @racket[fact-box] must not call itself by name.

@chunk[<fact-box>
       (define (fact-box n)
         (let* ([fact (box 'dummy)]
                [fact-fun (lambda (k)
                            (if (equal? 0 k)
                                1
                                (* k ((unbox fact) (- k 1)))))])
           (begin
             (set-box! fact fact-fun)
             ((unbox fact) n))))]

@chunk[<fact-box-tests>
       (check-expect (fact-box 0) 1)
       (check-expect (fact-box 5) 120)]

What happens if the function in the box does nothing but call itself?

@racketblock[
 (let ([nonterminating (box 'dummy)])
   (begin
     (set-box! nonterminating (lambda (x) ((unbox nonterminating) x)))
     ((unbox nonterminating) 5)))]

@nested[#:style 'inset]{
 Discuss with the classmate to your @emph{left}:
 Nothing here is defined recursively, yet it loops forever. Where does the loop come from?
 Compare this to an infinite loop written with @racket[define].
}

@nested[#:style 'inset]{
 The loop goes through the box: each time the function runs, it looks up the box,
 finds itself there, and calls itself again.
 With @racket[define], the function finds itself by its name; here it finds itself in the box.
 So mutation alone is enough to get recursion, and with it infinite loops,
 without any recursive definition.
 Lecture 7 uses this to implement @racket[rec] in our interpreter.
}

@exercise{Circular linked lists}
Write a function that returns a circular linked list of @racket[n] boxes (@racket[n] ≥ 1).
Each box holds a pair @racket[(cons value next)]: a number, and the next box.
The head holds @racket[n], the next box holds @racket[(- n 1)], and so on down to @racket[1].
The box holding @racket[1] points back to the head:

@centered{head (n) → (n-1) → … → (1) → head}

Hint: allocate the head first. Then write a recursive helper (stored in a box!)
that takes a number @racket[k] and a box @racket[b]:
it fills @racket[b] with @racket[k] and the next box, then builds the rest of the list.
When @racket[k] is @racket[1], the next box is the head.

@chunk[<circ-list>
       (define (circ-list n)
         (if (< n 1)
             (error 'circ-list "n must be at least 1")
             (let* ([head (box 'dummy)]
                    [build (box 'dummy)]
                    [build-fun
                     (lambda (k b)
                       (if (equal? 1 k)
                           (set-box! b (cons 1 head))
                           (let ([next-box (box 'dummy)])
                             (begin
                               (set-box! b (cons k next-box))
                               ((unbox build) (- k 1) next-box)))))])
               (begin
                 (set-box! build build-fun)
                 ((unbox build) n head)
                 head))))]

These helpers read the value in a box, and follow it to the next box:

@chunk[<circ-list-helpers>
       (define (node-value b) (car (unbox b)))
       (define (node-next b) (cdr (unbox b)))]

Fill in the expected results:

@chunk[<circ-list-tests>
       (define circ-1 (circ-list 1))
       (define circ-3 (circ-list 3))
       (check-expect (node-value circ-3) 3)
       (check-expect (node-value (node-next circ-3)) 2)
       (check-expect (eq? (node-next circ-1) circ-1) #t)
       (check-expect (eq? (node-next circ-3) circ-3) #f)
       (check-expect (eq? (node-next (node-next (node-next circ-3))) circ-3) #t)
       (check-error (circ-list 0) "circ-list: n must be at least 1")]

Once it works, try @racket[(circ-list 3)] in the DrRacket console.

@nested[#:style 'inset]{
 It prints as @litchar{#0='#&(3 . #&(2 . #&(1 . #0#)))}:
 the box holding 1 points back to the head, labelled @tt{#0=}.
}

@exercise{Recursive trees}
Let's build values of the recursive type @nonterm{tree}:
@BNF[(list @nonterm{tree}
           @BNF-seq[@litchar{leaf} @nonterm{number}]
           @BNF-seq[@litchar{node} @nonterm{tree} @nonterm{tree}])]

We represent a @nonterm{tree} as an s-expression: @racket['(leaf v)] or @racket['(node left right)].
@racket[tree-box] turns a nested list into a tree: a number becomes a leaf,
and @racket[(list l r)] becomes a node. Anything else is an error.
The pattern @racket[(? number? n)] matches any number and binds it to @racket[n].
Again, tie the knot with a box: @racket[tree-box] must not call itself by name.

@tabular[#:style 'boxed
         #:column-properties '(left left)
         #:row-properties '(bottom-border ())
         (list (list @bold{input} @bold{output})
               (list @racket[5] @racket['(leaf 5)])
               (list @racket[(list 5 (list 6 7))]
                     @racket['(node (leaf 5) (node (leaf 6) (leaf 7)))])
               (list @racket[(list 1 2 3)] "error"))]

@chunk[<tree-box>
       (define (tree-box input)
         (let* ([tree (box 'dummy)]
                [tree-fun
                 (lambda (t)
                   (match t
                     [(? number? n) (list 'leaf n)]
                     [(list left right)
                      (list 'node ((unbox tree) left) ((unbox tree) right))]
                     [_ (error 'tree "not a tree")]))])
           (begin
             (set-box! tree tree-fun)
             ((unbox tree) input))))]

@chunk[<tree-box-tests>
       (check-expect (tree-box 5) '(leaf 5))
       (check-expect (tree-box (list 5 (list 6 7)))
                     '(node (leaf 5) (node (leaf 6) (leaf 7))))
       (check-error (tree-box (list 1 2 3)) "tree: not a tree")
       (check-error (tree-box (list 5 (list 6))) "tree: not a tree")]

@section{Recursion without Mutation}
Can we get recursion without boxes?
Idea: instead of referring to itself, a function takes @emph{the recursive call} as an argument.

@chunk[<fact-gen>
       (define fact-gen
         (lambda (rec)
           (lambda (n)
             (if (equal? 0 n)
                 1
                 (* n (rec (- n 1)))))))]

@subsection{The Y-combinator}
We want some @racket[Y] such that, for any @racket[f],
@centered{Y f = f (Y f)}
i.e. @racket[(Y f)] is a @emph{fixed point} of @racket[f].
Then @racket[(Y fact-gen)] is factorial.
In the working below, @litchar{=>} is one evaluation step,
and @litchar{=} means the two sides are equal, but not by a single step.

@verbatim{
  Y fact-gen
  =  fact-gen (Y fact-gen)
  =  (lambda (n) (if (equal? 0 n) 1 (* n ((Y fact-gen) (- n 1)))))
}

@chunk[<Y>
       (define Y
         (lambda (h)
           ((lambda (x) (h (x x)))
            (lambda (x) (h (x x))))))]

Check that @racket[Y] really satisfies the equation:

@verbatim{
  Y f
  => (lambda (x) (f (x x))) (lambda (x) (f (x x)))
  => f ((lambda (x) (f (x x))) (lambda (x) (f (x x))))
  =  f (Y f)
}

The last step is @litchar{=}, not @litchar{=>}: the argument of @racket[f]
is what @racket[(Y f)] stepped to on the first line, but nothing steps back to @racket[(Y f)].

What happens if you run this in the DrRacket console?
(It never finishes: press @onscreen{Stop} to get control back.)

@racketblock[
 (define fact-y (Y fact-gen))]

@nested[#:style 'inset]{
 Discuss with the classmate to your @emph{right}:
 Racket evaluates arguments eagerly.
 Step through @racket[(Y fact-gen)] by hand. Where does it go wrong?
 Would it work in a lazy language?
}

@nested[#:style 'inset]{
 Write @tt{X} for @racket[(lambda (x) (fact-gen (x x)))]. Then
 @verbatim{
  Y fact-gen
  => X X
  => fact-gen (X X)
 }
 To call @racket[fact-gen], Racket first evaluates its argument @tt{(X X)},
 which steps to @tt{fact-gen (X X)} again, and so on forever.
 @racket[fact-gen] itself is never called.

 In a lazy language, @tt{(X X)} is not evaluated until @racket[rec] is used.
 @racket[fact-gen] returns its @racket[(lambda (n) ...)] right away,
 and only calls @racket[rec] when @racket[n] isn't 0, so it works.
}

@exercise{The Z-combinator}
@racket[Z] is the eager-evaluation-friendly version of @racket[Y].
For any @racket[f] and value @racket[v]:
@centered{Z f v = f (Z f) v}

Hint: start from @racket[Y] and delay the self-application by wrapping it in a @racket[lambda]:
@centered{@racket[(x x)]  ⇝  @racket[(lambda (v) ((x x) v))]}
The two behave the same when called.
But Racket doesn't evaluate a @racket[lambda]'s body until the @racket[lambda] is called,
so @racket[(x x)] waits.

@chunk[<Z>
       (define Z
         (lambda (h)
           ((lambda (x) (h (lambda (v) ((x x) v))))
            (lambda (x) (h (lambda (v) ((x x) v)))))))]

@chunk[<fact-z>
       (define fact-z
         (lambda (n) ((Z fact-gen) n)))]

@chunk[<fact-z-tests>
       (check-expect (fact-z 0) 1)
       (check-expect (fact-z 5) 120)]

Optional: step through @racket[((Z fact-gen) 3)] by hand and convince yourself it terminates.

@nested[#:style 'inset]{
 Write @tt{X} for @racket[(lambda (x) (fact-gen (lambda (v) ((x x) v))))]. Then
 @verbatim{
  Z fact-gen
  => X X
  => fact-gen (lambda (v) ((X X) v))
  => (lambda (n) (if (equal? 0 n) 1 (* n ((lambda (v) ((X X) v)) (- n 1)))))
 }
 The argument to @racket[fact-gen] is a @racket[lambda], so it is already a value and
 Racket doesn't evaluate @tt{(X X)} inside it. @racket[(Z fact-gen)] is a function,
 and @tt{(X X)} only runs when that function calls @racket[rec], i.e. when @racket[n] isn't 0.
 For @racket[3] that happens three times, and the call with @racket[0] stops.
}

@exercise{Putting it together}
Rewrite @racket[tree-box] using @racket[Z] instead of a box.
First write @racket[tree-gen], which takes the recursive call as an argument (like @racket[fact-gen]).

@chunk[<tree-z>
       (define tree-gen
         (lambda (rec)
           (lambda (t)
             (match t
               [(? number? n) (list 'leaf n)]
               [(list left right) (list 'node (rec left) (rec right))]
               [_ (error 'tree "not a tree")]))))
       (define tree-z
         (lambda (t) ((Z tree-gen) t)))]

@chunk[<tree-z-tests>
       (check-expect (tree-z 5) '(leaf 5))
       (check-expect (tree-z (list 5 (list 6 7)))
                     '(node (leaf 5) (node (leaf 6) (leaf 7))))
       (check-error (tree-z (list 1 2 3)) "tree: not a tree")
       (check-error (tree-z (list 5 (list 6))) "tree: not a tree")]

Compare @racket[Z] with @racket[fix] from lecture 7.
Both turn a function like @racket[fact-gen] into a recursive function:
@racket[fix] ties the knot with a box, while @racket[Z] uses nothing but functions.
(The Racket version of @racket[fix] from lecture takes @racket[rec] and @racket[n] together,
as in @racket[(lambda (rec n) ...)], rather than one at a time.)

@section{Challenge: @racket[rec] in the Interpreter}
In lecture 7 we added @racket[rec] to our interpreter in two ways:
with a new kind of value,
and by rewriting @racket[rec] into a call to @racket[fix].
In @tt{recitation7-interp.rkt} you'll rewrite @racket[rec] into a call to @racket[Z] instead,
so the interpreter needs neither a new value nor boxes.
