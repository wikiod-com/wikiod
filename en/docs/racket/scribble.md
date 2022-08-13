---
title: "Scribble"
slug: "scribble"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Scribble is the tool used to create [Racket's documentation](https://docs.racket-lang.org/), and you can document your own packages with it too! When published, their documentation will appear at https://docs.racket-lang.org/, alongside the main Racket documentation.

Scribble is implemented as a language for the Racket platform. Scribble documents will therefore usually start with `#lang scribble/manual`

## Paragraphs and sections
```
#lang scribble/manual

@section{Introduction}

First paragraph. Some text, some text, some text,
some text, some text, some text.

@section{More stuff}

@subsection{This is a subsection}

Second paragraph. More text, more text, more text,
more text, more text, more text.
```

## Documenting a binding provided by a package
```
#lang scribble/manual

@; Make sure that code highlighting recognises identifiers from my-package:
꩜require[@for-label[my-package]]

@; Indicate which module is exporting the identifiers documented here.
@defmodule[my-package]

@defproc[(my-procedure [arg1 number?] [arg2 string?]) symbol?]{
  The @racket[my-procedure] function repeats the @racket[arg2] string
  @racket[arg1] times, and transforms the result into a symbol.

  @history[#:added "1.0"
           #:changed "1.1" @elem{Improved performance,
                                 from @tt{O(n²)} to @tt{O(n)}}]
}

```

As a rule of thumb, a module (something that could appear on the right of a `(require foo/bar)`, i.e. `foo/bar`) should be documented by a single `.scribble` file. A `.scribble` file can document several modules, as long as each one is documented in a separate `@section`.

