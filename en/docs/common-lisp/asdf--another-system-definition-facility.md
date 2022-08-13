---
title: "ASDF - Another System Definition Facility"
slug: "asdf---another-system-definition-facility"
draft: false
images: []
weight: 9911
type: docs
toc: true
---

[ASDF - Another System Definition Facility][1]


  [1]: https://common-lisp.net/project/asdf/

ASDF is a tool for specifying how systems of Common Lisp software are made up of components (sub-systems and files), and how to operate on these components in the right order so that they can be compiled, loaded, tested, etc.

## Simple ASDF system with a flat directory structure
Consider this simple project with a flat directory structure:

```
example
|-- example.asd
|-- functions.lisp
|-- main.lisp
|-- packages.lisp
`-- tools.lisp
```

The `example.asd` file is really just another Lisp file with little more than an ASDF-specific function call. Assuming your project depends on the [`drakma`](http://www.weitz.de/drakma/) and [`clsql`](http://quickdocs.org/clsql/) systems, its contents can be something like this:

```lisp
(asdf:defsystem :example
    :description "a simple example project"
    :version "1.0"
    :author "TheAuthor"
    :depends-on (:clsql
                 :drakma)
    :components ((:file "packages")
                 (:file "tools" :depends-on ("packages"))
                 (:file "functions" :depends-on ("packages"))
                 (:file "main" :depends-on ("packages"
                                            "functions"))))
```

When you load this Lisp file, you tell ASDF about your `:example` system, but you're not loading the system itself yet. That is done either by `(asdf:require-system :example)` or `(ql:quickload :example)`.

And when you load the system, ASDF will:

1. Load the dependencies - in this case the ASDF systems `clsql` and `drakma` 
2. *Compile and load* the components of your system, i.e. the Lisp files, based on the given dependencies
   1. `packages` first (no dependencies)
   2. `functions` after `packages` (as it only depends on `packages`), but before `main` (which depends on it)
   3. `main` after `functions` (as it depends on `packages` and `functions`)
   4. `tools` anytime after `packages`


Keep in mind:

* Enter the dependencies as they are needed (e.g. macro definitions are needed before usage). If you don't, ASDF will error when loading your system. 
* All files listed end on `.lisp` but this postfix should be dropped in the asdf script
* If your system is named the same as its `.asd` file, and you move (or symlink) its folder into `quicklisp/local-projects/` folder, you can then load the project using `(ql:quickload "example")`.
* Libraries your system depends on have to be known to either ASDF (via the `ASDF:*CENTRAL-REGISTRY` variable) or Quicklisp (either via the `QUICKLISP-CLIENT:*LOCAL-PROJECT-DIRECTORIES*` variable or available in any of its dists)


## How to define a test operation for a system
    (in-package #:asdf-user)
    
    (defsystem #:foo
      :components ((:file "foo"))
      :in-order-to ((asdf:test-op (asdf:load-op :foo)))
      :perform (asdf:test-op (o c)
                        (uiop:symbol-call :foo-tests 'run-tests)))
    
    (defsystem #:foo-tests
      :name "foo-test"
      :components ((:file "tests")))

    ;; Afterwards to run the tests we type in the REPL
    (asdf:test-system :foo)

Notes: 
- We are assuming that the *system* :foo-tests defines a *package* named "FOO-TESTS"
- `run-tests` is the entry point for the test runner
- uoip:symbol-call allows as to define a method that calls a function that hasn't been read yet. The package the function is defined in doesn't exist when we define the system 

## In what package should I define my ASDF system?
ASDF provides the package `ASDF-USER` for developers to define their packages in.

