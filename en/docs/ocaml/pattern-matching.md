---
title: "Pattern Matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Factorial Function using Pattern Matching
```ocaml
let rec factorial n = match n with
| 0 | 1 -> 1
| n -> n * (factorial (n - 1))
```

This function matches on both the values 0 and 1 and maps them to the base case of our recursive definition.  Then all other numbers map to the recursive call of this function.

## Evaluation of boolean expressions
We define the type of boolean expressions whose atoms are identified by strings as

    type expr =
    | Atom of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr

and can evaluate these expressions using an `oracle : string -> bool` giving the values of the *atoms* we find as follows:

    let rec eval oracle = function
    | Atom(name) -> oracle name
    | Not(expr) -> not(eval oracle expr)
    | And(expr1, expr2) -> (eval oracle expr1) && (eval oracle expr2)
    | Or(expr1, expr2)  -> (eval oracle expr1) || (eval oracle expr2)
 
See how the function is clear and easy to read. Thanks to correct use of pattern matching, a programmer reading this function needs little time to ensure it is correctly implemented.

## Negation normal form : deep pattern matching
Pattern matching allows to deconstruct complex values and it is by no way limited to the “outer most” level of the representation of a value.  To illustrate this, we implement the function transforming a boolean expression into a boolean expression where all negations are only on atoms, the so called *negation normal form* and a predicate recognising expressions in this form:

We define the type of boolean expressions whose atoms are identified by strings as

    type expr =
    | Atom of string
    | Not of expr
    | And of expr * expr
    | Or of expr * expr

Let us first define a predicate recognising expressions in *negation normal form*:

    let rec is_nnf = function
    | (Atom(_) | Not(Atom(_))) -> true
    | Not(_) -> false
    | (And(expr1, expr2) | Or(expr1, expr2)) -> is_nnf expr1 && is_nnf expr2

As you see, it is possible to match against nested patterns like `Not(Atom(_))`. Now we implement a function mapping a boolean expression to an equivalent boolean expression in negation normal form:

    let rec nnf = function
    | (Atom(_) | Not(Atom(_))) as expr -> expr
    | Not(And(expr1, expr2)) -> Or(nnf(Not(expr1)),nnf(Not(expr2)))
    | Not(Or(expr1, expr2)) -> And(nnf(Not(expr1)),nnf(Not(expr2)))
    | And(expr1, expr2) -> And(nnf expr1, nnf expr2)
    | Or(expr1, expr2) -> Or(nnf expr1, nnf expr2)
    | Not(Not(expr)) -> nnf expr

This second function makes even more uses of nested patterns. We finally can test our code in the toplevel on the negation of an implication:    

    # let impl a b =
    Or(Not(a), b);;
      val impl : expr -> expr -> expr = <fun>
    # let expr = Not(impl (Atom "A") (Atom "B"));;
    val expr : expr = Not (Or (Not (Atom "A"), Atom "B"))
    # nnf expr;;
    - : expr = And (Atom "A", Not (Atom "B"))
    # is_nnf (nnf expr);;
    - : bool = true

The OCaml type system is able to verify the exhaustivity of a pattern matching. For instance, if we omit the `Not(Or(expr1, expr2))` case in the `nnf` function, the compiler issues a warning:

    # let rec non_exhaustive_nnf = function
    | (Atom(_) | Not(Atom(_))) as expr -> expr
    | Not(And(expr1, expr2)) -> Or(nnf(Not(expr1)),nnf(Not(expr2)))
    | And(expr1, expr2) -> And(nnf expr1, nnf expr2)
    | Or(expr1, expr2) -> Or(nnf expr1, nnf expr2)
    | Not(Not(expr)) -> nnf expr;;
              Characters 14-254:
      ..............function
      | (Atom(_) | Not(Atom(_))) as expr -> expr
      | Not(And(expr1, expr2)) -> Or(nnf(Not(expr1)),nnf(Not(expr2)))
      | And(expr1, expr2) -> And(nnf expr1, nnf expr2)
      | Or(expr1, expr2) -> Or(nnf expr1, nnf expr2)
      | Not(Not(expr)) -> nnf expr..
    Warning 8: this pattern-matching is not exhaustive.
    Here is an example of a case that is not matched:
    Not (Or (_, _))
    val non_exhaustive_nnf : expr -> expr = <fun>

(This warning can be treated as an error with the `-w @8` option when invoking the compiler or the interpreter.)

This feature provides an increased level of safety and correctness in programs that are accepted by the compiler. It has however other uses and can for instance be used in explorative programming. It is is very fun to write a conversion to a normal form, starting with crude versions of the function that handle the easy cases and using examples of non-matched cases provided by the compiler to refine the treatment.

## Matching record fields
Pattern matching can be used to deconstruct records. We illustrate this with a record type representing locations in a text file, *e.g.* the source code of a program.

    type location = {
      filename : string;
      line: int;
      column: int;
      offset: int;
    }

A value `x` of type location can be deconstructed like this:

    let { filename; line; column; offset; } = x

A similar syntax can be used to define functions, for instance a function to print locations:

    let print_location { filename; line; column; offset; } =
      Printf.printf "%s: %d: %d" filename line column

or alternatively

    let print_location = function { filename; line; column; offset; } ->
      Printf.printf "%s: %d: %d" filename line column

Patterns matching records do not need to mention all fields of a record. Since the function does not use the `offset` field, we can leave it out:

    let print_location { filename; line; column; } =
      Printf.printf "%s: %d: %d" filename line column

When the record is defined in a module, it is enough to qualify the first field occurring in the pattern:

    module Location =
    struct
      type t = {
          filename : string;
          line: int;
          column: int;
          offset: int;
        }
    end
    
    let print_location { Location.filename; line; column; } =
      Printf.printf "%s: %d: %d" filename line column


## Recursive list processing with pattern matching
Here we demonstrate how to process lists recursively using OCaml's pattern matching syntax.

```
let rec map f lst =
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)
```

In this case, the pattern `[]` matches the empty list, while `hd::tl` matches any list that has *at least one* element, and will assign the first element of the list to `hd` and the rest of the list (could be empty) to `tl`.

Note that `hd::tl` is a very general pattern and will match any list that isn't empty. We can also write patterns that match on lists with a specific number of elements:

```
(* Return the last element of a list. Fails if the list is empty. *)
let rec last lst =
  match lst with
  | [] -> failwith "Empty list"
  | [x] -> x (* Equivalent to x::[], [x] matches a list with only one element *)
  | hd::tl -> last tl

(* The second to last element of a list. *)
let rec second_to_last lst =
  match lst with
  | [] -> failwith "Empty list"
  | x::[] -> failwith "Singleton list"
  | fst::snd::[] -> snd
  | hd::tl -> second_to_last tl
```

Additionally, OCaml supports pattern matching on the elements of lists themselves. We can be more specific about the structure of elements inside a list, and OCaml will infer the correct function type:

```
(* Assuming a list of tuples, return a list with first element of each tuple. *)
let rec first_elements lst =
  match lst with
  | [] -> []
  | (a, b)::tl -> a::(first_elements tl)
(* val first_elements : ('a * 'b) list -> 'a list = <fun> *)
```

By combining these patterns together, we can process any arbitrarily complex list.

## Defining a function using pattern matching
The keyword `function` can be used to initiate pattern-matching on the the last argument of a function. For example, we can write a function called `sum`, which computes the sum of a list of integers, this way

    let rec sum = function
      | [] -> 0
      | h::t -> h + sum t
    ;;

    val sum : int list -> int = <fun>

