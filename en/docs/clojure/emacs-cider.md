---
title: "Emacs CIDER"
slug: "emacs-cider"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

CIDER is the acronym for **C**lojure(script) **I**nteractive **D**evelopment **E**nvironment that **R**ocks.
It is an extension to emacs. CIDER aims to provide an interactive development environment to the programmer. CIDER is built on top of nREPL, a networked REPL server and SLIME served as the principle inspiration for CIDER. 


## Function Evaluation
CIDER function `cider-eval-last-sexp` can be used to execute the the code while editing the code inside the buffer. This function is by default binded to `C-x C-e` or `C-x C-e`.

CIDER manual says `C-x C-e` or `C-c C-e` will:

> Evaluate the form preceding point and display the result in the echo area and/or in an buffer overlay. 

For example:
```
(defn say-hello
  [username]
  (format "Hello, my name is %s" username))

(defn introducing-bob
  []
  (say-hello "Bob")) => "Hello, my name is Bob"

``` 

Performing `C-x C-e` or  `C-c C-e` while your cursor is just ahead of the ending paren of `say-hello` function call will output the string `Hello, my name is Bob`.

## Pretty Print
CIDER function `cider-insert-last-sexp-in-repl` can be used to execute the the code while editing the code inside the buffer and get the output pretty printed in a different buffer. This function is by default binded to `C-c C-p`.

CIDER manual says `C-c C-p` will

> Evaluate the form preceding point and pretty-print the result in a popup buffer.

For example
```
(def databases {:database1  {:password "password"
                             :database "test"
                             :port "5432"
                             :host "localhost"
                             :user "username"}
                  
                :database2 {:password "password"
                            :database "different_test_db"
                            :port "5432"
                            :host "localhost"
                            :user "vader"}})

(defn get-database-config
  []
  databases)

(get-database-config)
```
Performing `C-c C-p` while your cursor is just ahead of the ending paren of `get-database-config` function call will output the pretty printed map in a new popup buffer.
```
{:database1
 {:password "password",
  :database "test",
  :port "5432",
  :host "localhost",
  :user "username"},
 :database2
 {:password "password",
  :database "different_test_db",
  :port "5432",
  :host "localhost",
  :user "vader"}}

```

