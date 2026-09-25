#lang scribble/lp2
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))


@chunk[<*>
       (require racket)
       (require test-engine/racket-tests)
       <factorial>
       <fibonacci-if>
       <fibonacci>
       <fibonacci-tests>
       <else-gotcha>
       <third>
       <third-tests>
       <third-fixed>
       <third-fixed-tests>
       (test)
       ]

@title{Pattern Matching}
We've been using @racket[match] to take apart @racket[bexp] structs.
It can do a lot more than that: it can match literal values, ignore parts of the
input with wildcards, and take apart lists.

@section{Matching on values}
You can think of pattern matching as a more flexible, concise alternative to conditionals.
Here's a simple example with two patterns, which a single @racket[if] could also express:

@chunk[<factorial>
       (define (factorial x)
         (match x
           [0 1]
           [_ (* x (factorial (- x 1)))]))

       (define (factorial-if x)
         (if (= x 0)
             1
             (* x (factorial-if (- x 1)))))]

The pattern @racket[_] is a @emph{wildcard}: it matches anything and binds nothing.

The more cases there are, the more concise @racket[match] gets compared with @racket[if].
Here's Fibonacci written with nested @racket[if]s:

@chunk[<fibonacci-if>
       (define (fibonacci-if x)
         (if (= x 0)
             0
             (if (= x 1)
                 1
                 (+ (fibonacci-if (- x 1)) (fibonacci-if (- x 2))))))]

@exercise{Fibonacci with @racket[match]}
Rewrite @racket[fibonacci-if] using @racket[match].

@chunk[<fibonacci>
       (define (fibonacci x)
         (match x
           [0 0]
           [1 1]
           [_ (+ (fibonacci (- x 1)) (fibonacci (- x 2)))]))]

@chunk[<fibonacci-tests>
       (check-expect (fibonacci 1) 1)
       (check-expect (fibonacci 10) 55)
       (check-expect (fibonacci 10) (fibonacci-if 10))]

@exercise{What is @racket[else]?}
You'll often see @racket[[else ...]] as the last clause of a @racket[match].
It looks like the @racket[else] of @racket[cond], but it isn't.
Predict what this evaluates to, then run it.

@chunk[<else-gotcha>
       (check-expect (match 5 [else else]) 5)]

@nested[#:style 'inset]{
 Discuss with your neighbors: why do you get that result?
 What is @racket[else] bound to inside the clause?

 It evaluates to @racket[5]. Inside a @racket[match], @racket[else] isn't a keyword:
 it's a pattern variable, just like @racket[x]. It matches anything and binds it,
 so here @racket[else] is bound to @racket[5] and the clause returns it.
 That's why @racket[[else ...]] happens to work as a catch-all.
 Use @racket[_] instead, which matches anything without binding a name.
}

@section{Destructuring lists}
Patterns can also take apart lists. Here are two ways to grab the third element of a list:

@chunk[<third>
       (define (my-third x)
         (match x
           [(list a b c d) c]
           [_ x]))

       (define (other-third lst)
         (match lst
           [(cons _ (cons _ (cons third _))) third]
           [_ (error "List too short")]))]

@exercise{Predict}
Before running anything, predict what each call returns.
If you think a call raises an error, use @racket[check-error] instead of @racket[check-expect].

@nested[#:style 'inset]{
 Discuss with the classmate to your @emph{left}:
 On which inputs do the two functions behave differently, and why?
}

@chunk[<third-tests>
       (check-expect (my-third (list 2 3)) (list 2 3))
       (check-expect (my-third 0) 0)
       (check-expect (my-third (list '(2 3) 2 4 5 6 7)) (list '(2 3) 2 4 5 6 7))
       (check-expect (my-third (list 1 2 3 4)) 3)

       (check-error (other-third (list 2 3)) "List too short")
       (check-error (other-third 0) "List too short")
       (check-expect (other-third (list '(2 3) 2 4 5 6 7)) 4)
       (check-expect (other-third (list 1 2 3 4)) 3)]

@nested[#:style 'inset]{
 @racket[(list a b c d)] only matches lists of @emph{exactly} four elements.
 @racket[(cons _ (cons _ (cons third _)))] matches any list with @emph{at least} three.
}

@exercise{Fix @racket[my-third]}
Fix @racket[my-third] so that it returns the third element of any list with at least three elements,
and raises an error otherwise.
Try to do it with a @racket[list] pattern: the pattern @racket[_ ...] matches zero or more elements.

@chunk[<third-fixed>
       (define (my-third-fixed x)
         (match x
           [(list _ _ c _ ...) c]
           [_ (error "List too short")]))]

@chunk[<third-fixed-tests>
       (check-error (my-third-fixed (list 2 3)) "List too short")
       (check-expect (my-third-fixed (list 1 2 3)) 3)
       (check-expect (my-third-fixed (list '(2 3) 2 4 5 6 7)) 4)]
