#lang scribble/lp2

@(define (exercise . content)
   (apply subsection #:style 'unnumbered "Exercise: " content))

@chunk[<*>
       (require racket)
       <swap>
       ]

@title{Scopes and Macros}
In lecture we learned about dynamic and static scope,
and came to the conclusion that for most uses, static scope seems more reasonable.
Let's consider a slightly different context though: macros.

Informally, macros are a way to "generate" code with some shorthands.
There are a few ways we can implement macros.
Let's see one possible implementation.

@section{Text Macros (C)}
You may be familiar with, for example, C macros through the C preprocessor.
Open @tt{unhygienic.c}, where you will find the following code.
@verbatim{
#include <stdio.h>
#define SWAP(a, b) do { \
  int temp = (a);       \
  (a) = (b);            \
  (b) = temp;           \
} while(0)

int main(void) {
  int x = 10;
  int y = 20;
  SWAP(x, y);
  printf("x = %d, y = %d\n", x, y);
  return 0;
}
}

@subsection{Alpha equivalence with C macros}
Recall that two programs p and q are alpha equivalent (p ≡@subscript{α} q)
if we can rename variables in p to obtain q.

A program transformation can be thought of as a function that takes a program and returns a program.
Furthermore, we state that a program transformation T @emph{preserves} alpha equivalence when the following is true:

@centered{
 If p ≡@subscript{α} q, then T(p) ≡@subscript{α} T(q).
}
In this sequence of exercises, we'll consider whether C preprocessor macros preserve alpha equivalence.
First, compile and execute @tt{unhygienic.c} as-is.
What does the program print?

@exercise{A strange renaming}
Now, in the body of main, rename @tt{x} to @tt{temp}.
Don't compile and run it yet.

@exercise{Predicting the behavior}
Instead, discuss with your neighbor what you @emph{think} the program will print.

@exercise{Explaining the behavior}
Now, compile and run the program. Did it do what you expected? Why did it do that?

@exercise{Reasoning about alpha equivalence}
Let us consider the C preprocessor as a program transformation (it takes some C code and produces C code).
Does the C preprocessor preserve α-equivalence?

@section{Hygienic Macros}
Now, let's examine the behavior of Racket.
Below is the same swap macro, written in Racket.

@chunk[<swap>
       (define-syntax-rule (swap a b)
         (let ([temp a])
           (set! a b)
           (set! b temp)))
       (let ([x 5] [y 6])
         (swap x y)
         (list x y))
       (let ([temp 5] [y 6])
         (swap temp y)
         (list temp y))]