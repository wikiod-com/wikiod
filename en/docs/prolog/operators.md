---
title: "Operators"
slug: "operators"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Predefined operators
Predefined operators according to ISO/IEC 13211-1 and 13211-2:

| Priority   | Type | Operator(s) | Use |
| ---------- | ---- | ----------- | --- |
| 1200       | xfx  | `:-` `-->`
| 1200       | fx   | `:-`  `?-`      | Directive, query
| 1100       | xfy  | `;`           
| 1050       | xfy  | `->`          |
| 1000       | xfy  |  `','`        | 
| 900        | fy   | `\+`         |
| 700        | xfx  | `=`  `\\=`      | Term unification
| 700        | xfx  | `==` `\\==` `@<`  `@=<`  `@>` `@>=` | Term comparison
| 700        | xfx  | `=..`                     |
| 700        | xfx  | `is` `=:=`  `=\=`  `<`  `>`  `=<`  `>=` | Arithmetic evaluation and comparison
| 600        | xfy  | `:`                       | Module qualification
| 500        | yfx  | `+`  `-`  `/\`  `\/`          |
| 400        | yfx  | `*`  `/`  `div` `mod` `// ` `rem`  `<<`  `>>`
| 200        | xfx  |  `**`                     | Float power
| 200        | xfy  | `^`                       | Variable quantification, integer power
| 200        |  fy  | `+`  `-`  `\`                | Arithmetic identity, negation ; bitwise complement

Many systems provide further operators as an implementation specific extension:

| Priority   | Type | Operator(s) | Use |
| ---------- | ---- | ----------- | --- |
| 1150       | fx   | `dynamic` `multifile` `discontiguous` `initialization` | Standard directives
| 1150       | fx   | `mode` `public` `volatile` `block` `meta_predicate` 
| 900        | fy   | `spy` `nospy`        |


## Operator declaration
In Prolog, custom operators can be defined using `op/3`:

`op(+Precedence, +Type, :Operator)`

 - Declares Operator to be an operator of a Type with a Precedence. Operator can also be a list of names in which case all elements of the list are declared to be identical operators.

 - Precedence is an integer between 0 and 1200, where 0 removes the declaration.

 - Type is one of: `xf`, `yf`, `xfx`, `xfy`, `yfx`, `fy` or `fx` where `f` indicates the position of the functor and `x` and `y` indicate the positions of the arguments. `y` denotes a term with a precedence lower or equal to the precedence of the functor, whereas `x` denotes a strictly lower precedence.

   - Prefix:   `fx` , `fy`
   - Infix:    `xfx` (not associative), `xfy` (right associative),  `yfx` (left associative)
   - Postfix:  `xf` , `yf`


Example usage:

    :- op(900, xf, is_true).
    
    X_0 is_true :-
      X_0.

Example query:
    
    ?- dif(X, a) is_true.
    dif(X, a).



## Term ordering
Two terms may be compared via the standard ordering:
    
 > variables @< numbers @< atoms @< strings @< structures @< lists

*Notes:*

 -  Structures compare alphabetically by functor first, then by arity and lastly by the comparison of each argument.

 -  Lists compare by length first, then by each element.

| Order operator | Succeeds if               |
| -------------- | ------------------------- |
| X @< Y         | X is less than Y in the standard order   |
| X @> Y         | X is greater than Y in the standard order |
| X @=< Y        | X is less than or equal to Y in the standard order     |
| X @>= Y        | X is greater than or equal to Y in the standard order   |

Example queries:

    ?- alpha @< beta.
    true.

    ?- alpha(1) @< beta.
    false.

    ?- alpha(X) @< alpha(1).
    true.

    ?- alpha(X) @=< alpha(Y).
    true.

    ?- alpha(X) @> alpha(Y).
    false.

    ?- compound(z) @< compound(inner(a)).
    true.

## Term equality
| Equality operator | Succeeds if               |
| ----------------- | ------------------------- |
| X = Y             | X can be unified with Y   |
| X \\= Y           | X cannot be unified with Y |
| X == Y            | X and Y are identical (i.e. they unify with *no* variable bindings occurring) |
| X \\== Y          | X and Y are not identical   |
| X =:= Y           | X and Y are arithmetically equal   |
| X =\\= Y           | X and Y are not arithmetically equal  |


