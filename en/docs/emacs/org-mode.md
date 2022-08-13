---
title: "Org-mode"
slug: "org-mode"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

Org is a mode for keeping notes, maintaining TODO lists, and project planning with a fast and effective plain-text system. It also is an authoring system with unique support for literate programming and reproducible research. 

[org Mode official site][1]

  [1]: http://orgmode.org/

## Markup syntax
Org provides a full markup language which helps structuring the document, and is reflected as accurately as possible when exporting to other formats (like HTML or LaTeX).

# Structure

## Document title

```
#+TITLE: This is the title of the document
```

## Sectioning

```
* First level
** Second level
```

## Lists

```
Ordered list (items can also be numbered like '1)', with a perenthesis):
1. foo
2. bar
3. baz
```

```
Unordered list (items can also start with '+' or '*'):
- foo
- bar
- baz
```

```
Description
- lorem ipsum :: this is example text
- foo bar :: these are placeholder words
```

## Checkboxes
Every item in a plain list can be made into a checkbox by starting it with the string ‘[ ]’.
```
* TODO [2/4] (or [50%])
  - [-] call people [1/3]
    - [ ] Peter
      - [X] Sarah
      - [ ] Sam
  - [X] order food
  - [ ] think about what music to play
  - [X] talk to the neighbors
```

- <kbd>C-c C-c</kbd>       org-toggle-checkbox
- <kbd>C-c C-x C-b</kbd>   org-toggle-checkbox
- <kbd>M-S-<RET></kbd>     org-insert-todo-heading
- <kbd>C-c C-x o</kbd>     org-toggle-ordered-property
- <kbd>C-c #</kbd>         org-update-statistics-cookies

# Emphasis and monospace

```
You can make words *bold*, /italic/, _underlined_, =verbatim=
and ~code~, and, if you must, ‘+strike-through+’.
```
Text in the code and verbatim string is not processed for Org mode specific syntax, it is exported verbatim.

# Links and references

## Links

Org-mode will recognize URL formats and activate them as clickable links. However, links can be explicitly declared like this:
```
You will find more information in the [[http://orgmode.org/org.html][Org Manual]].
```

or alternatively :
```
The org manual is located here: [[http://orgmode.org/org.html]]
```

## Footnotes

Footnotes can either be named:
```
See the org manual[fn:manual] to get more details.
...
[fn:manual] You will find it here: http://orgmode.org/org.html
```

or anonymous and inline:

```
See the org manual[fn:: You will find it here: http://orgmode.org/org.html]
to get more details.
```

## Basic Key Bindings
To cycle the level of outline shown:

* <kbd>Tab</kbd>  Cycle outline level for one heading
* <kbd>Shift-Tab</kbd> Cycle outline level for the whole document

To cycle through `TODO` states:

* <kbd>Shift-Right Arrow</kbd> 
* <kbd>Shift-Left Arrow</kbd> 

To increase or decrease hierarchical level for a heading
* <kbd>Meta-Right Arrow</kbd> Make lower level ("increase indent")
* <kbd>Meta-Left Arrow</kbd> Make higher level ("decrease indent")

To cycle the priority for a given heading:
* <kbd>Shift-Up Arrow</kbd>
* <kbd>Shift-Down Arrow</kbd>

To move a heading up or down:
* <kbd>Meta-Up Arrow</kbd>
* <kbd>Meta-Down Arrow</kbd>

(<kbd>Meta</kbd> refers to different keys on different keyboards. Most often it is either <kbd>Alt</kbd> or <kbd>⌘</kbd>).

## Code blocks
To add a code block, surround it with `#+BEGIN_SRC language` and `#+END_SRC`. *language* should correspond to the major mode for the language in question, e.g. the major mode for Emacs Lisp is `emacs-lisp-mode`, so write `#+BEGIN_SRC emacs-lisp`.

    #+BEGIN_SRC emacs-lisp
    (defun hello-world ()
      (interactive)
      (message "hello world"))
    #+END_SRC

    #+BEGIN_SRC python
    print "hello world"
    #+END_SRC

You can open the code block in a separate buffer by typing `C-c '` (for `org-edit-special`).  If you don't have the major mode for the specified language, that will give an error message such as `No such language mode: foo-mode`.

If the content you want to put in the block is not in any programming language, you can use `#+BEGIN_EXAMPLE` and `#+END_EXAMPLE` instead.

    #+BEGIN_EXAMPLE
    output from a command I just ran
    #+END_EXAMPLE

There are [easy templates](http://orgmode.org/manual/Easy-Templates.html) for both of these.  At the beginning of the line, type either `<s` or `<e`, and then hit `TAB`.  It will expand into a block with begin and end markers for `SRC` or `EXAMPLE`, respectively.

These markers are all case insensitive, so you can write `#+begin_src` etc instead if you prefer.

## Tables
    | Name  | Phone | Age |
    |-------+-------+-----|
    | Peter | 1234  | 17  |
    | Anna  | 4321  | 25  |


To add a Table in org-mode, simply surround your columns with a bar (<kbd>|</kbd>)

    | column1 | column2 | this column is wider |

When you press <kbd>Return</kbd> from inside a column, org-mode will automatically create a new row with the bars.
 - <kbd>Tab</kbd> and <kbd>Return</kbd> will respectively move to the next cell or row (or create a new one if there isn't any)
 - You can swap the rows and columns around with <kbd>M-ArrowKey</kbd>
 - <kbd>M-S-Down</kbd> and <kbd>M-S-Right</kbd> will respectively create a row (above the current) and a column (on the left of the current)
 - <kbd>M-S-Up</kbd> and <kbd>M-S-Left</kbd> will respectively remove the current row and the current column
 - <kbd>C-c i</kbd> create a separator







