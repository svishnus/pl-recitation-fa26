#lang scribble/lp2
@(require scribble/bnf)
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))


@chunk[<*>
       (require racket)
       (require test-engine/racket-tests)
       <bexp>
       <leaf-bool>
       <syn-equal>
       <bexp-interp>
       <bexp-interp-tests>
       <bexp-interp-oops>
       <bexp-unopt>
       <bexp-opt>
       <demorgan-opt-check>
       <demorgan-opt>
       <demorgan-opt-test>
       (test)
       ]

@title{Boolean Expressions}
Let's consider the language @nonterm{bexp} of boolean expressions.
@BNF[(list @nonterm{bexp}
           @nonterm{boolean}
           @BNF-seq[@nonterm{bexp} @litchar{∧} @nonterm{bexp}]
           @BNF-seq[@nonterm{bexp} @litchar{∨} @nonterm{bexp}]
           @BNF-seq[@litchar{¬} @nonterm{bexp}])]

Here's one way of representing the grammar of @nonterm{bexp} in racket.

@chunk[<bexp>
       (struct leaf-node (arg))
       (struct or-node (left right))
       (struct and-node (left right))
       (struct not-node (arg))]

With this representation, you can write @nonterm{bexp}s as follows:

@chunk[<leaf-bool>
       (define bexp-bool-1 (and-node (leaf-node #t) (leaf-node #f)))
       (define bexp-sym-1 (and-node (leaf-node 'x) (leaf-node 'y)))]

@section{Interpreting @nonterm{bexp}}

@exercise{Get comfy with @nonterm{bexp}}
Let's write a quick helper function to check whether two @nonterm{bexp}s are
@emph{syntactically} equal.

@chunk[<syn-equal>
       (define
         (syn-equal? b1 b2)
         (match* (b1 b2)
           [((and-node l1 r1) (and-node l2 r2)) (and (syn-equal? l1 l2) (syn-equal? r1 r2))]
           [((or-node l1 r1) (or-node l2 r2)) 'TODO]
           [((not-node a1) (not-node a2)) 'TODO]
           [((leaf-node v1) (leaf-node v2)) (equal? v1 v2)]
           [(_ _) #f]))]

@exercise{@nonterm{bexp} interpreter}
Define an interpreter for the bexp language.
The interpreter should calculate the boolean value represented by the expression.

@chunk[<bexp-interp>
       (define (evaluate-bexp b)
         (match b
           [(leaf-node v)    'TODO]
           [(or-node b1 b2)  'TODO]
           [(and-node b1 b2) 'TODO]
           [(not-node b1)   'TODO]))]

Write your tests here:

@chunk[<bexp-interp-tests>
       (check-expect (evaluate-bexp (leaf-node #t)) 'TODO)
       (check-expect (evaluate-bexp (and-node (leaf-node #t) (leaf-node #f))) 'TODO)]

What happens when you miss cases in the pattern match?

@chunk[<bexp-interp-oops>
       (define (evaluate-bexp-m b)
         (match b
           [(leaf-node v) v]))]
Try it out in the DrRacket console and discuss what happens.

@section{Optimization Pass}
In propositional logic, @hyperlink["https://en.wikipedia.org/wiki/De_Morgan%27s_laws"]{De Morgan's laws} state the following equivalences: 
@itemlist[
 @item{¬(P ∧ Q)  =  ¬P ∨ ¬Q}
 @item{¬(P ∨ Q)  =  ¬P ∧ ¬Q}]

Also, double negation elimination states:
¬¬P = P

One possible way to optimize a @nonterm{bexp} program is to use the above equivalences to
@itemlist[
 #:style 'compact
 @item{eliminate redundant negations}
 @item{"push" all negations down to the leaves}]

Here are some example symbolic @nonterm{bexp}s:

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list "name" @bold{before} @bold{after})
               (list @racket[bexp-1] "¬(¬P ∨ Q)" "P ∧ ¬Q")
               (list @racket[bexp-2] "¬¬P ∨ ¬Q" "P ∨ ¬Q")
               (list @racket[bexp-3] "P ∨ ¬Q" "P ∨ ¬Q"))]


@chunk[<bexp-unopt>
       (define bexp-1
         (not-node
          (or-node
           (not-node (leaf-node 'P))
           (leaf-node 'Q))))
       (define bexp-2
         (or-node
          (not-node (not-node (leaf-node 'P)))
          (not-node (leaf-node 'Q))))
       (define bexp-3
         (or-node
          (leaf-node 'P)
          (not-node (leaf-node 'Q))))]

@chunk[<bexp-opt>
       (define bexp-1-opt
         (and-node
          (leaf-node 'P)
          (not-node (leaf-node 'Q))))
       (define bexp-2-opt 
         (or-node
          (leaf-node 'P)
          (not-node (leaf-node 'Q))))
       (define bexp-3-opt
         (or-node
          (leaf-node 'P)
          (not-node (leaf-node 'Q))))]


@exercise{Thinking about testing}
Before we start writing the @racket[demorgan-opt] function implementing this optimization,
consider what it means for @racket[demorgan-opt] to be correct and how to check.

@nested[#:style 'inset]{                        
 Discuss with the classmate to your @emph{right}:
 What does it mean for an optimization to be correct?
 How would you check whether an optimization is indeed correct?
}

@chunk[<demorgan-opt-check>
       (define (preserves-sem? unopt opt) 'TODO)
       (define (is-optimizing? unopt opt) 'TODO)
       (define (is-correct? unopt opt) (and (preserves-sem? unopt opt) (is-optimizing? unopt opt)))]

@exercise{Optimizing with De Morgan's Law}
Define a function to perform this optimization pass.
Carefully consider the pattern match cases.
You will need more cases than we used in @racket[evaluate-bexp].

@chunk[
 <demorgan-opt>
 (define
   (demorgan-opt b)
   (match b
     [(leaf-node v)    (leaf-node v)]
     [(or-node b1 b2)  (or-node (demorgan-opt b1) (demorgan-opt b2))]
     [(and-node b1 b2) (and-node (demorgan-opt b1) (demorgan-opt b2))]
     [(not-node b1) 'TODO]))]

Test your optimization pass using the test functions you defined above.

@chunk[<demorgan-opt-test>
       (check-expect (is-correct? bexp-1 (demorgan-opt bexp-1)) 'TODO)]