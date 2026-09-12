# Programming Languages — Recitations (Fall 2026)

Recitation handouts are written as [Scribble](https://docs.racket-lang.org/scribble/index.html)
literate programs (`#lang scribble/lp2`), so each file is both the handout you read and
the program you run.

In DrRacket:

- **Run** — executes the code chunks (including the `check-expect` tests).
- **Scribble HTML/PDF** — renders the handout with clickable links to the Racket docs.

## Schedule

| #   | Topic                                                                                 | Handout                                  | Solutions                                                    |
| --- | ------------------------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------ |
| 1   | Functional programming in Racket, DrRacket, recursion, lists & higher-order functions | [recitation1.rkt](rec01/recitation1.rkt) | [recitation1-solutions.rkt](rec01/recitation1-solutions.rkt) |

## How these files work

- Handouts are posted **before** recitation with exercises left as `'TODO`. We fill them in
  together during the session, so bring a laptop with DrRacket if you want to code along.
- Solutions are posted **after** recitation as a separate file.

Layout convention:

```
recNN/
  recitationN.rkt            # handout (exercises as 'TODO)
  recitationN-solutions.rkt  # posted after recitation
```

## Setup

1. Install [Racket](https://download.racket-lang.org/) (DrRacket ships with it).
2. Open the handout in DrRacket, e.g. [rec01/recitation1.rkt](rec01/recitation1.rkt).
3. Press **Run**. You should see `"Hello, world!"` in the interactions pane — then you're set.
