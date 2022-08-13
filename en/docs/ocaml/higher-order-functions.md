---
title: "Higher Order Functions"
slug: "higher-order-functions"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- `val (|>) : 'a -> ('a -> 'b) -> 'b`
- `val (@@) : ('a -> 'b) -> 'a -> 'b`

## Generic algorithms
Higher-order functions can be used to implement generic algorithms, giving up the responsibility of providing final details to the user. For instance `List.sort` expects a comparison function, which allows to implement various ways of sorting. Here we implement case-insensitive sorting of strings:

    let string_case_insensitive_sort lst =
      let case_insensitive_compare a b =
        String.compare (String.lowercase a) (String.lowercase b)
      in
      List.sort case_insensitive_compare lst

There is a rich list of higher-order functions in the standard library, especially in the *[List](http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html)* module, see `List.fold_left` and `List.sort` for instance.  More advanced examples can be found in third-party libraries.  A good example is the [simulated annealing](http://mmottl.github.io/gsl-ocaml/api/Siman.html) implemented in **ocaml-gsl**.  [Simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing) is a generic optimisation procedure which is parametrised by a function used to explore the set of states of the problem and an error function (called here energy function).

Users familiar with C++ can compare this to the *[Strategy](https://en.wikibooks.org/wiki/Computer_Science_Design_Patterns/Strategy)* pattern.

## Dispose system resources even when an exception is raised
Higher-order functions can be used to ensure that system resources are disposed, even when a treatment raises an exception.  The pattern used by `with_output_file` allows a clean separation of concerns: the higher-order `with_output_file` functions takes care of managing the system resources bound to file manipulation while the treatment `f` only consumes the output channel.

    let with_output_file path f =
      let c = open_out path in
      try
        let answer = f c in
        (close_out c; answer)
      with exn -> (close_out c; raise exn)

Let us use this higher-order function to implement a function writing a string to a file:

    let save_string path s =
      (with_output_file path) (fun c -> output_string c s)

Using more advanced functions than `fun c -> output_string c s` it is possible to save more complex values.  See for instance the *[Marshal](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html)* module in the standard library or the *[Yojson](http://mjambon.com/yojson.html)* library by Martin Jambon.

## Composition operators
Two useful higher-order functions are the binary *application* (`@@`) and *reverse-application* or "pipe" (`|>`) operators. Although since [4.01][1] they're available as primitives, it might still be instructive to define them here:

    let (|>) x f = f x
    let (@@) f x = f x

Consider the problem of incrementing the square of 3. One way of expressing that computation is this:

    (* 1 -- Using parentheses *)
    succ (square 3)
    (* - : int = 10 *)

    (* where `square` is defined as: *)
    let square x = x * x

Note that we couldn't simply do `succ square 3` because (due to [left-associativity][2]) that would reduce to the meaningless `(succ square) 3`. Using application (`@@`) we can express that without the parentheses:

    (* 2 -- Using the application operator *)
    succ @@ square 3
    (* - : int = 10 *)

Notice how the last operation to be performed (namely `succ`) occurs first in the expression? The *reverse-application* operator (`|>`) allows us to, well, reverse this:

    (* 3 -- Using the reverse-application operator *)
    3 |> square |> succ
    (* - : int = 10 *)

The number 3 is now "piped" through `square` and then `succ`, as opposed to being applied to `square` to yield a result that `succ` is applied to.


  [1]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
  [2]: http://caml.inria.fr/pub/docs/manual-ocaml/expr.html

