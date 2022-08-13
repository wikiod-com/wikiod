---
title: "Macros"
slug: "macros"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Syntax
- The `'` symbol used in the macroexpand example is just syntactic sugar for the `quote` operator. You could have written `(macroexpand (quote (infix 1 + 2)))` instead.

Macros are just functions that run at compile time, i.e. during the [`eval`][eval] step in a [read-eval-print-loop].

Reader macros are another form of macro that gets expanded at read time, rather than compile time.

Best practice when defining macro.

- alpha-renaming, Since macro is expand binding name conflict could arise. Binding conflict is not very intuitive to solve when using the macro. This is why, whenever a macro adds a binding to the scope, it is mandatory to use the `#` at the end of each symbol.

[eval]: https://clojuredocs.org/clojure.core/eval
[read-eval-print-loop]: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop

## Simple Infix Macro
Clojure uses prefix notation, that is: The operator comes before its operands.

For example, a simple sum of two numbers would be:

```clojure
(+ 1 2)
;; => 3
```

Macros allow you to manipulate the Clojure language to a certain degree. For example, you could implement a macro that let you write code in infix notation (e.g., `1 + 2`):

```
(defmacro infix [first-operand operator second-operand]
    "Converts an infix expression into a prefix expression"
    (list operator first-operand second-operand))
```

Let's break down what the code above does:

- `defmacro` is a _special form_ you use to define a macro.
- `infix` is the name of the macro we are defining.
- `[first-operand operator second-operand]` are the parameters this macro expects to receive when it is called.
- `(list operator first-operand second-operand)` is the body of our macro. It simply creates a `list` with the values of the parameters provided to the `infix` macro and returns that.

`defmacro` is a _special form_ because it behaves a little differently compared to other Clojure constructs: Its parameters are not immediately evaluated (when we call the macro). This is what allows us to write something like:

```
(infix 1 + 2)
;; => 3
```

The `infix` macro will expand the `1 + 2` arguments into `(+ 1 2)`, which is a valid Clojure form that can be evaluated.

If you want to see what the `infix` macro generates, you can use the `macroexpand` operator:

```
(macroexpand '(infix 1 + 2))
;; => (+ 1 2)
```

`macroexpand`, as implied by its name, will expand the macro (in this case, it will use the `infix` macro to transform `1 + 2` into `(+ 1 2)`) but won't allow the result of the macro expansion to be evaluated by Clojure's interpreter.

## Syntax quoting and unquoting
Example from the standard library ([core.clj:807](https://github.com/clojure/clojure/blob/clojure-1.7.0/src/clj/clojure/core.clj#L807)):

    (defmacro and
      "Evaluates exprs one at a time, from left to right. If a form
      returns logical false (nil or false), and returns that value and
      doesn't evaluate any of the other expressions, otherwise it returns
      the value of the last expr. (and) returns true."
      {:added "1.0"}
      ([] true)
      ([x] x)
      ([x & next]
       `(let [and# ~x]
          (if and# (and ~@next) and#))))

 - <code>`</code> called syntax-quote is like <code>(quote)</code>, but recursive: it causes <code>(let …)</code>, <code>(if …)</code>, etc to not evaluate during macro expansion but to output as is
 - `~` aka unquote cancels syntax-quote for single form inside syntax-quoted form. So `x`'s value is outputted when expanding macro (instead of outputting `x` symbol)
 - `~@` aka unquote-splicing is like unquote but takes list argument and expands it, each list item to separate form
 - `#` appends unique id to symbols to prevent name conflicts. It appends the same id for the same symbol inside syntax-quoted expression, so `and#` inside `let` and `and#` inside `if` will get the same name

