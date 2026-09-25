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
| 2   | Boolean expressions: structs, writing an interpreter, and optimizations               | [recitation2.rkt](rec02/recitation2.rkt) | [recitation2-solutions.rkt](rec02/recitation2-solutions.rkt) |
| 3a  | Typed boolean expressions in `plai-typed`: `define-type`, `type-case`, exhaustiveness | [recitation3a.rkt](rec03/recitation3a.rkt) | _after recitation_                                             |
| 3b  | Pattern matching: literals, wildcards, and destructuring lists                        | [recitation3b.rkt](rec03/recitation3b.rkt) | _after recitation_                                             |

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

Some weeks are split into parts (`recitationNa.rkt`, `recitationNb.rkt`, each with its own
solutions file). Parts written in `#lang plai-typed` (e.g. 3a) are plain Racket files with
comments rather than Scribble handouts, so there's no HTML/PDF version — just open and **Run**.

## Setup

1. Install [Racket](https://download.racket-lang.org/) (DrRacket ships with it).
2. Open the handout in DrRacket, e.g. [rec01/recitation1.rkt](rec01/recitation1.rkt).
3. Press **Run**. You should see `"Hello, world!"` in the interactions pane — then you're set.
4. From recitation 3 on, you'll also need `plai-typed`. In DrRacket, go to
   **File → Install Package…** and enter `plai-typed`, or run `raco pkg install plai-typed`.
   To check it worked, open [rec03/recitation3a.rkt](rec03/recitation3a.rkt) and press **Run**.
   If you see test failures that say `TODO`, you're set. If you see
   `collection not found`, the package isn't installed.
