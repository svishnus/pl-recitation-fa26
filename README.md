# Programming Languages — Recitations (Fall 2026)

Each handout is a single `.rkt` file you open in DrRacket and **Run**. Running it executes
the code, including the tests.

From recitation 3 on, most handouts are written in `#lang plai-typed`. These are plain
Racket files with comments, so you read them in the editor.

A few untyped handouts (recitations 1, 2, and 3b) are
[Scribble](https://docs.racket-lang.org/scribble/index.html) literate programs
(`#lang scribble/lp2`). For these, the **Scribble HTML/PDF** button also renders the
handout as a page with clickable links to the Racket docs.

## Schedule

| #   | Topic                                                                                 | Handout                                  | Solutions                                                    |
| --- | ------------------------------------------------------------------------------------- | ---------------------------------------- | ------------------------------------------------------------ |
| 1   | Functional programming in Racket, DrRacket, recursion, lists & higher-order functions | [recitation1.rkt](rec01/recitation1.rkt) | [recitation1-solutions.rkt](rec01/recitation1-solutions.rkt) |
| 2   | Boolean expressions: structs, writing an interpreter, and optimizations               | [recitation2.rkt](rec02/recitation2.rkt) | [recitation2-solutions.rkt](rec02/recitation2-solutions.rkt) |
| 3a  | Typed boolean expressions in `plai-typed`: `define-type`, `type-case`, exhaustiveness | [recitation3a.rkt](rec03/recitation3a.rkt) | [recitation3a-solutions.rkt](rec03/recitation3a-solutions.rkt) |
| 3b  | Pattern matching: literals, wildcards, and destructuring lists                        | [recitation3b.rkt](rec03/recitation3b.rkt) | [recitation3b-solutions.rkt](rec03/recitation3b-solutions.rkt) |

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
solutions file).

## Setup

1. Install [Racket](https://download.racket-lang.org/) (DrRacket ships with it).
2. Open the handout in DrRacket, e.g. [rec01/recitation1.rkt](rec01/recitation1.rkt).
3. Press **Run**. You should see `"Hello, world!"` in the interactions pane — then you're set.
4. From recitation 3 on, you'll also need `plai-typed`. In DrRacket, go to
   **File → Install Package…** and enter `plai-typed`, or run `raco pkg install plai-typed`.
   To check it worked, open [rec03/recitation3a.rkt](rec03/recitation3a.rkt) and press **Run**.
   If you see test failures that say `TODO`, you're set. If you see
   `collection not found`, the package isn't installed.
