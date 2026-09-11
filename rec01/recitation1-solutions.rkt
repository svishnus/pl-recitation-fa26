#lang scribble/lp2
@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))

@title{Recitation 1: Functional Programming in Racket}
This recitation will cover basic functional programming in Racket
and working in the DrRacket IDE.
This document is authored with @hyperlink["https://docs.racket-lang.org/scribble/index.html"]{@tt{scribble}},
a documentation language written in Racket itself.
In DrRacket, press "Scribble HTML/PDF" to render this recitation handout in HTML/PDF.
This should make it easier to read and give you clickable links to racket library functions you may be unfamiliar with.
When you want to execute your code, press "Run".

First, we'll import everything we need from the base racket language.

@chunk[<requires>
       (require racket)
       (require test-engine/racket-tests)]
Any code written inside a chunk block will be run when you press run.
@codeblock|{
@chunk[<some-name>
...]
}|
For instance, you should see "Hello, world!" printed to the DrRacket console
when you press run.

@chunk[<hello-world>
       (print "Hello, world!\n")]
Change the message above and rerun to see a different message printed to the DrRacket console.

Here are all codeblocks in this recitation:

@chunk[<*>
       <requires>
       <all>
       <hello-world>
       <is-even>
       <traffic-lights>
       <can-floor-it>
       <next-light-iter>
       <next-light-iter-tests>
       <scroll-setup>
       <time-to-change>
       <safe-to-scroll>
       <all-even-rec>
       <all-even-map>
       <reverse-list>
       <reverse>

       (test)
       ]
This includes both the helper code @racket['TODO] items you must complete during the recitation.

If that works, you're all set to continue!

@section{Expressions and Functions}
Racket is an expression-oriented language.
Thus, Racket programs are essentially lists of expression.
Every Racket program evaluates by reducing itself to a value.

@chunk[<is-even>
       (define (is-even x)
         (= 0 (modulo x 2)))]

@nested[#:style 'inset]{
 What will the following expressions evaluate to? Try them in the DrRacket REPL:
 @itemlist[
 @item{@racket[is-even]}
 @item{@racket[(is-even)]}
 @item{@racket[(is-even 2)]}
 @item{@racket[(is-even 3)]}
 @item{@racket[(is-even #t)]}
 @item{@racket[(is-even 'four)]}]}

In the functional programming style, we build programs by function composition.
In this exercise, we will practice this form of composition.

A traffic light is one of @racket['red], @racket['yellow], @racket['green].
The function @racket[can-go] @emph{EXPECTS} a traffic light and @emph{PRODUCES} a boolean.
@racket[(can-go l)] evaluates to @racket[#t] when a vehicle approaching a light can safely enter the intersection.

The function @racket[next-light]
@emph{EXPECTS} a traffic light and @emph{PRODUCES} a traffic light.
@racket[(next-light l)] evaluates to the color of the light after @racket[l]

@margin-note{What is the difference between @racket[eq?] and @racket[equal?]}

@chunk[<traffic-lights>
       (define (can-go l) (equal? l 'green))

       (check-expect (can-go 'green) #t)
       (check-expect (can-go 'yellow) #f)
       (check-expect (can-go 'red) #f)

       (define (next-light l)
         (match l
           ['red    'green]
           ['yellow 'red]
           ['green  'yellow]))

       (check-expect (next-light 'green) 'yellow)
       (check-expect (next-light 'yellow) 'red)
       (check-expect (next-light 'red) 'green)]

@exercise{Pedal to the Metal}

Fill in the following function:

The function @racket[can-floor-it] @emph{EXPECTS} a traffic light and @emph{PRODUCES} a boolean.
@racket[(can-floor-it l)] evaluates to @racket[#t] when a vehicle can safely enter the intersection at either the current light, or the next light.

@chunk[<can-floor-it>
       (define (can-floor-it l) 
         (or (can-go l) (can-go (next-light l))))
       (check-expect (can-floor-it 'green) #t)
       (check-expect (can-floor-it 'red) #t)
       (check-expect (can-floor-it 'yellow) #f)]

Once you're done, practice writing @racket[check-expect] tests for @racket[can-floor-it],
and evaluating @racket[can-floor-it] in the DrRacket REPL.

@section{Specifications}
When thinking about @emph{how} to write a program, it is helpful to think about the valid inputs and expected outputs.
Given the following specification, answer the following questions.

The function @racket[next-light-iter]
@itemlist[
 @item{@emph{EXPECTS}: a non-negative integer @racket[n]}
 @item{@emph{EXPECTS}: a traffic light @racket[l]}
 @item{@emph{PRODUCES}: a traffic light}
 ]
@racket[(next-light-iter n l)] gives the color of the traffic light @racket[l], after waiting for it to change @racket[n] times.
@itemlist[
 @item{When @racket[n] is zero, what do we expect @racket[(next-light-iter n l)] to evaluate to?}
 @item{When @racket[n] is greater than zero, what do we expect @racket[(next-light-iter n l)] to evaluate to?}
 ]

@exercise{Next Light}
Using the answers to the questions above, fill in this partially completed implementation of @racket[next-light-iter].

@chunk[<next-light-iter>
       (define (next-light-iter n l)
         (if (eq? n 0)
             l
             (next-light-iter (- n 1) (next-light l))))
       ]

@subsection{Exercise: Testing}

Using the answers to the questions above, write tests for @racket[next-light-iter].

@chunk[<next-light-iter-tests>
       (check-expect (next-light-iter 0 'green) 'green)
       (check-expect (next-light-iter 1 'green) 'yellow)
       (check-expect (next-light-iter 2 'yellow) 'green)
       ]

@section{Recursion}
Recursive functions are a common idiom in functional programming.
In this exercise, we will walk through the process for implementing a recursive function.
@margin-note{
 The verb tense of "recursion" is @emph{"recur"}.
 Some computer scientists use "recurse", but
 as of Sep. 11 2026, "recurse" is not a recognized word in
 @hyperlink["https://www.merriam-webster.com/dictionary/recurse"]{most}
 @hyperlink["https://dictionary.cambridge.org/spellcheck/english/?q=recurse"]{dict}@hyperlink["https://www.dictionary.com/browse/recurse?noredirect=true"]{iona}@hyperlink["https://www.collinsdictionary.com/submission/421277/recurse"]{ries}
}

@exercise{Scroll}

Using the same process, fill in the following recursive function.

A vehicle is one of @racket['sports-car], @racket['suv], or @racket['prius].

The function @racket[speed] EXPECTS a vehicle and PRODUCES an integer.
@racket[(speed v)] gives the speed of the vehicle in meters per second.

@chunk[<scroll-setup>
       (define (speed v)
         (match v
           ['sports-car 136]
           ['suv       48]
           ['prius     27]))]

       

The function time-to-change
EXPECTS a traffic light l and
RETURNS a non-negative integer.
@racket[(time-to-change l)] gives the number of seconds a traffic light.

@chunk[<time-to-change>
       (define (time-to-change l)
         (match l
           ['red       10]
           ['yellow    2]
           ['green     23]))
       ]

The function @racket[safe-to-scroll] EXPECTS a vehicle @racket[v],
a traffic light @racket[l],
and a non-negative integer @racket[d], representing the distance from the vehicle to the light. It RETURNS a boolean value.
The function @racket[(safe-to-scroll v d l)] returns @racket[#t]
if @racket[v] is @racket[d] meters away from an intersection which has just changed to the light @racket[l],
@racket[v] will enter the intersection without incurring a traffic violation.

@itemlist[
 @item{What do we expect @racket[(safe-to-scroll v l d)] to be when the vehicle will enter the intersection before it switches?}
 @item{What do we expect @racket[(safe-to-scroll v l d)] to be when the vehicle will not enter the intersection before it switches?}
 ]

@emph{HINT:} Consider defining a helper function to determine if the vehicle will enter the intersection before or after the light switches.

@chunk[<safe-to-scroll>
       (define (will-enter v d l)
         (<= d (* (time-to-change l) (speed v))))
       (define (safe-to-scroll v d l)
         (if (will-enter v d l)
             (can-go l)
             (safe-to-scroll
              v
              (- d (* (time-to-change l) (speed v)))
              (next-light l))))
       (check-expect (safe-to-scroll 'sports-car 137 'green) #t)
       ]

@section{Lists and Higher-Order Programming}

Recall the function @racket[is-even].
In this section, we will practice functional programming by writing a program to check if every element in a list is even, using this function.

@subsection[#:style 'unnumbered]{Warmup: List operations cons and append}

What do we expect the following to return?
@itemlist[
 @item{@racket[(cons 3 4)]}
 @item{@racket[(cons 3 '(4))]}
 @item{@racket[(cons '(4) 3)]}
 @item{@racket[(append 3 4)]}
 @item{@racket[(append 3 '(4))]}
 @item{@racket[(append '(3) 4)]}
 ]

@exercise{All even}

First, we can write a recursive function just like we did above.

The function @racket[all-even-rec] EXPECTS a list of integers @racket[l] and PRODUCES a boolean.
@racket[(all-even-rec l)] is @racket[#t] when all numbers in the list are even.

@chunk[<all-even-rec>
       (define (all-even-rec l)
         (match l
           ['()              #t]
           [(cons head tail) (and (is-even head) (all-even-rec tail))]))
       (check-expect (all-even-rec '()) #t)
       (check-expect (all-even-rec '(2 4 6)) #t)
       (check-expect (all-even-rec '(2 4 6 7)) #f)]

@exercise{Map}

In class you learned about the @racket[map] function, which lets you apply a function to every element of a list.

Racket comes with built-in ways to avoid writing our own recursive functions all the time.
We include a function @racket[all] which checks to see if a list of booleans are all @racket[#t].

@chunk[<all>
       (define (all l) (andmap identity l))]
You can click on @racket[andmap] and @racket[identity] to read the documentation about these functions.
Using @racket[map] and @racket[all], write a function to check if all elements of a list are even.

@chunk[<all-even-map>
       (check-expect (all (list)) #t)
       (check-expect (all (list #t #t #t)) #t)
       (check-expect (all (list #t #f #t)) #f)

       (define (all-even-map l) (andmap is-even l))
       (check-expect (all-even-map'()) #t)
       (check-expect (all-even-map '(2 4 6)) #t)
       (check-expect (all-even-map '(2 4 6 7)) #f)
       ]

@exercise{Reverse}

Write a function to reverse a list.

@chunk[<reverse-list>
       (define (reverse-list l)
         (match l
           ['() '()]
           [(cons head tail) (append (reverse-list tail) (list head))]))
       (check-expect (reverse-list '(2 4 6)) '(6 4 2))
       (check-expect (reverse-list '(#t 3)) '(3 #t))
       (check-expect (reverse-list '('(#t 3) "list")) '("list" '(#t 3)))
       ]

@exercise{Tail recursion}
Imagine a list with millions of elements.
In that case, a function that goes over every element of the list would create a huge call stack.

For example, say we run @racket[(reverse-list '(1 2 3 4 5 6 7))].
The call would reduce to:
@racketblock[
 (append (reverse-list '(2 3 4 5 6 7)) (list 1))
 (append (append (reverse-list '(3 4 5 6 7)) (list 2)) (list 1))
 (append (append (append (reverse-list '(4 5 6 7)) (list 3)) (list 2)) (list 1))
 ]

To prevent such situations from arising,
many programming languages (especially those with functional origins)
support @emph{tail-call optimization}.
When the last operation of a function is just another function call, Racket pops the current function from the call stack.
@margin-note{
 You'll sometimes also see the term @emph{tail-call elimination}.
 This means the language guarantees that the optimization will happen.
 Racket does guarantee it, but a many languages do not (e.g. Rust, Java, etc).
}

Write your own version of @racket[reverse] that can leverage tail-call optimization.
@emph{Hint:} Use a helper function. @racket[reverse] can only have one argument.
On the other hand, you can use the helper function to have multiple arguments that might store some more information.

@chunk[<reverse>
       (define (reverse-helper l acc)
         (match l
           ['() acc]
           [(cons head tail) (reverse-helper tail (cons head acc))]))
       (define (reverse-list-tail l)
         (reverse-helper l '()))
       (check-expect (reverse-list-tail '(2 4 6)) '(6 4 2))
       (check-expect (reverse-list-tail '(#t 3)) '(3 #t))
       (check-expect (reverse-list-tail '('(#t 3) "list")) '("list" '(#t 3)))
       ]