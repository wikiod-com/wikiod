---
title: "Getting started with lisp"
slug: "getting-started-with-lisp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Dialects of Lisp and their implementations
Invented by John McCarthy around 1958, Lisp (**Lis**t **P**rocessor) has continued to grow into an entire family of languages.

Since StackOverflow is more about practical programming problems, typically problems will involve actual Lisp dialects or derived languages and their implementations. Problems that are generally Lisp-related may be tagged with [*lisp*][1]. There are many dialects and implementations, but not all are significant for StackOverflow.

Important dialects and related languages are:

* [Common Lisp][2] ([tag][3]), a general purpose language with many implementations and a standard – popular implementations are [SBCL][4], [CLISP][5], [Clozure CL][6] and others
* Emacs Lisp ([tag][7]), a Lisp dialect and its implementation mainly used in the GNU Emacs editor
* Scheme ([tag][8]), a [Lisp-like][13] language with many implementations and an evolving standard
* [Racket][9] ([tag][10]), a language and its implementation derived from Scheme
* [Clojure][11] ([tag][12]), a language and a main implementation for the JVM


  [1]: http://stackoverflow.com/questions/tagged/lisp
  [2]: https://www.wikiod.com/common-lisp
  [3]: http://stackoverflow.com/questions/tagged/common-lisp
  [4]: http://stackoverflow.com/questions/tagged/sbcl
  [5]: http://stackoverflow.com/questions/tagged/clisp
  [6]: http://stackoverflow.com/questions/tagged/ccl
  [7]: http://stackoverflow.com/questions/tagged/elisp
  [8]: http://stackoverflow.com/questions/tagged/scheme
  [9]: https://www.wikiod.com/racket
  [10]: http://stackoverflow.com/questions/tagged/racket
  [11]: https://www.wikiod.com/clojure
  [12]: http://stackoverflow.com/questions/tagged/clojure
  [13]: http://c2.com/cgi/wiki?IsSchemeLisp

## Lisp Resources
See also [Common Lisp Learning Resources](https://www.wikiod.com/common-lisp/getting-started-with-common-lisp#Common Lisp Learning Resources).

**Online Books**

* [Practical Common Lisp](http://www.gigamonkeys.com/book/), Peter Seibel. Good for experienced programmers.
* [Common Lisp: A Gentle Introduction to Symbolic Computation](http://www-2.cs.cmu.edu/~dst/LispBook/) Good for people new to programming.
* [Common Lisp, the Language](https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html)
* [On Lisp](http://www.paulgraham.com/onlisp.html), Paul Graham
* [The Common Lisp Cookbook](http://cl-cookbook.sourceforge.net/) 

**Offline Books**

* [ANSI Common Lisp](http://www.paulgraham.com/acl.html), Paul Graham. 
* Common Lisp - An interactive approach
* [Common Lisp Recipes](http://weitz.de/cl-recipes/)





**IRC**

#lisp

#ccl

#sbcl

**Libraries**
<a href="https://www.quicklisp.org/beta/">Quicklisp</a> is a package management platform for Lisp libraries.



## Installation or Setup
Probably the two most popular free implementations of Common Lisp are Clozure Common Lisp (CCL) and Steel Bank Common Lisp (SBCL). They are both available for a variety of platforms including Linux on x86-64 and Linux on ARM.

CCL: http://ccl.clozure.com/download.html

SBCL: http://www.sbcl.org/getting.html


Besides the compiler and basic Read-Eval-Print Loop (REPL), you may want some sort of development environment. One popular setup is to use Emacs to edit text interactively. The Superior Lisp Interaction Mode for Emacs (SLIME) allows Emacs to connect to a Lisp implementation and evaluate code interactively, from the editable text file and from a REPL within the Emacs editor:

https://common-lisp.net/project/slime/


