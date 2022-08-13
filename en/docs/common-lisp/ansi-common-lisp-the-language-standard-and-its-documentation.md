---
title: "ANSI Common Lisp, the language standard and its documentation"
slug: "ansi-common-lisp-the-language-standard-and-its-documentation"
draft: false
images: []
weight: 9933
type: docs
toc: true
---

## Common Lisp HyperSpec
Common Lisp has a standard, which was initially published in 1994 as an ANSI standard.

The [Common Lisp HyperSpec][1], short CLHS, provided by [LispWorks][2] is an often used HTML documentation, which is derived from the standard document. [The HyperSpec can also be downloaded and locally used][3].

Common Lisp development environments usually allow lookup of the HyperSpec documentation for Lisp symbols.

* For [GNU Emacs][4] there is [clhs.el][5].
* [SLIME][6] for GNU Emacs provides a version of [hyperspec.el][7].

See also: [cliki on CLHS][8]


  [1]: http://www.lispworks.com/documentation/HyperSpec/Front/
  [2]: http://www.lispworks.com/
  [3]: http://www.lispworks.com/documentation/common-lisp.html
  [4]: https://www.gnu.org/software/emacs/
  [5]: https://sourceforge.net/p/clisp/clisp/ci/default/tree/emacs/clhs.el
  [6]: https://common-lisp.net/project/slime/
  [7]: https://github.com/slime/slime/blob/master/lib/hyperspec.el
  [8]: http://cliki.net/CLHS

## EBNF syntax declarations in documentation
The ANSI CL standard uses an extended EBNF syntax notation. The documentation duplicated on Stackoverflow should use the same syntax notation to reduce confusion.

Example:

    specialized-lambda-list::=
         ({var | (var parameter-specializer-name)}* 
          [&optional {var | (var [initform [supplied-p-parameter] ])}*] 
          [&rest var] 
          [&key{var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
               [&allow-other-keys] ] 
          [&aux {var | (var [initform] )}*] ) 

Notation:
* `[foo]` -> zero or one `foo`
* `{foo}*` -> zero or more `foo`
* `foo | bar` ->`foo` or `bar`


## Common Lisp the Language, 2nd Edition, by Guy L. Steele Jr.
This book is known as CLtL2.

This is the second edition of the book Common Lisp the Language. It was published in 1990, before the ANSI CL standard was final. It took the original language definition from the first edition (published in 1984) and described all changes in the standardization process up to 1990 plus some extensions (like the SERIES iteration facility).

**Note: CLTL2 describes a version of Common Lisp which is slightly different from the published standard from 1994. Thus always use the standard, and not CLtL2, as a reference.**

CLtL2 still can be useful, because it provides information not found in the Common Lisp specification document.

There is a HTML version of [Common Lisp the Language, 2nd Edition][1].


  [1]: http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/cltl2.html

## CLiki - Proposed ANSI Revisions and Clarifications
On CLiki, a Wiki for Common Lisp and *free* Common Lisp software, a list of [Proposed ANSI Revisions and Clarifications][1] is being maintained.

Since the Common Lisp standard has not changed since 1994, users have found several problems with the specification document. These are documented on the CLiki page.

  [1]: http://www.cliki.net/Proposed%20ANSI%20Revisions%20and%20Clarifications

## Common Lisp Quick Reference
The [Common Lisp Quick Reference][1] is a document which can be printed and bound as a booklet in various layouts to have a printed quick reference for Common Lisp.


  [1]: http://clqr.boundp.org

## The ANSI Common Lisp standard in Texinfo format (especially useful for GNU Emacs)
GNU Emacs uses a special format for documentation: *info*.

The Common Lisp standard has been converted to the Texinfo format, which can be used to create documentation browsable with the *info* reader in GNU Emacs.

See here: [dpans2texi.el converts the TeX sources of the draft ANSI Common Lisp standard (dpANS) to the Texinfo format.][1]

Another version has been done for for GCL: [gcl.info.tgz][2].


  [1]: https://github.com/RobBlackwell/dpans2texi
  [2]: ftp://ftp.gnu.org/pub/gnu/gcl/gcl.info.tgz

