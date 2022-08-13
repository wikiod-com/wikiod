---
title: "Macros"
slug: "macros"
draft: false
images: []
weight: 9915
type: docs
toc: true
---

A walkthrough of macros can be found in [*The Rust Programming Language* (a.k.a. The Book)](https://doc.rust-lang.org/book/macros.html).

## Fragment specifiers — Kind of patterns
In `$e:expr`, the `expr` is called the *fragment specifier*. It tells the parser what kind of tokens the parameter `$e` is expecting. Rust provides a variety of fragment specifiers to, allowing the input to be very flexible.

| Specifier | Description | Examples
|---|---|---|---|
| `ident` | Identifier | `x`, `foo`
| `path` | Qualified name | `std::collection::HashSet`, `Vec::new` 
| `ty` | Type | `i32`, `&T`, `Vec<(char, String)>`
| `expr` | Expression | `2+2`, `f(42)`, `if true { 1 } else { 2 }`
| `pat` | Pattern | `_`, `c @ 'a' ... 'z'`, `(true, &x)`, `Badger { age, .. }`
| `stmt` | Statement | `let x = 3`, `return 42`
| `block` | Brace-delimited block | `{ foo(); bar(); }`, `{ x(); y(); z() }`
| `item` | Item | `fn foo() {}`, `struct Bar;`, `use std::io;`
| `meta` | Inside of attribute | `cfg!(windows)`, `doc="comment"`
| `tt` | Token tree | `+`, `foo`, `5`, `[?!(???)]`

Note that a doc comment `/// comment` is treated the same as `#[doc="comment"]` to a macro.

    macro_rules! declare_const_option_type {
        (
            $(#[$attr:meta])*
            const $name:ident: $ty:ty as optional;
        ) => {
            $(#[$attr])*
            const $name: Option<$ty> = None;
        }
    }
    
    declare_const_option_type! {
        /// some doc comment
        const OPT_INT: i32 as optional;
    }

    // The above will be expanded to:
    #[doc="some doc comment"]
    const OPT_INT: Option<i32> = None;


# Follow set

Some fragment specifiers requires the token following it must be one of a restricted set, called the "follow set". This allows some flexibility for Rust's syntax to evolve without breaking existing macros.

| Specifier | Follow set |
|---|---|
| `expr`, `stmt` | `=>`  `,`  `;`
| `ty`, `path` | `=>` `,` `=` <code>\|</code> `;` `:` `>` `[` `{` `as` `where`
| `pat` | `=>` `,` `=` <code>\|</code> `if` `in`
| `ident`, `block`, `item`, `meta`, `tt` | *any token*

    macro_rules! invalid_macro {
        ($e:expr + $f:expr) => { $e + $f };
    //           ^
    //           `+` is not in the follow set of `expr`,
    //           and thus the compiler will not accept this macro definition.
        ($($e:expr)/+) => { $($e)/+ };
    //             ^
    //             The separator `/` is not in the follow set of `expr`
    //             and thus the compiler will not accept this macro definition.
    }

## Tutorial
Macros allow us to abstract syntactical patterns that are repeated many times. For instance:

```
/// Computes `a + b * c`. If any of the operation overflows, returns `None`.
fn checked_fma(a: u64, b: u64, c: u64) -> Option<u64> {
    let product = match b.checked_mul(c) {
        Some(p) => p,
        None => return None,
    };
    let sum = match a.checked_add(product) {
        Some(s) => s,
        None => return None,
    };
    Some(sum)
}
```

We notice that the two `match` statements are very similar: both of them have the same pattern
```
match expression {
    Some(x) => x,
    None => return None,
}
```

Imagine we represent the above pattern as `try_opt!(expression)`, then we could rewrite the function into just 3 lines:

```
fn checked_fma(a: u64, b: u64, c: u64) -> Option<u64> {
    let product = try_opt!(b.checked_mul(c));
    let sum = try_opt!(a.checked_add(product));
    Some(sum)
}
```

`try_opt!` cannot write a function because a function does not support the early return. But we could do it with a macro — whenever we have these syntactical patterns that cannot be represented using a function, we may try to use a macro.

We define a macro using the `macro_rules!` syntax:

```
macro_rules! try_opt {
//                  ^ note: no `!` after the macro name
    ($e:expr) => {
//   ^~~~~~~ The macro accepts an "expression" argument, which we call `$e`.
//           All macro parameters must be named like `$xxxxx`, to distinguish from
//           normal tokens.
        match $e {
//            ^~ The input is used here.
            Some(x) => x,
            None => return None,
        }
    }
}
```

That's it! We have created our first macro.

(Try it in [Rust Playground](https://is.gd/ZUbRga))

## Create a HashSet macro
    // This example creates a macro `set!` that functions similarly to the built-in
    // macro vec!

    use std::collections::HashSet;

    macro_rules! set {
        ( $( $x:expr ),* ) => {  // Match zero or more comma delimited items
            {
                let mut temp_set = HashSet::new();  // Create a mutable HashSet
                $(
                    temp_set.insert($x); // Insert each item matched into the HashSet
                )*
                temp_set // Return the populated HashSet
            }
        };
    }

    // Usage
    let my_set = set![1, 2, 3, 4];


## Recursion
A macro can call itself, like a function recursion:

    macro_rules! sum {
        ($base:expr) => { $base };
        ($a:expr, $($rest:expr),+) => { $a + sum!($($rest),+) };
    }

Let's go though the expansion of `sum!(1, 2, 3)`:

       sum!(1, 2, 3)
    //      ^  ^~~~
    //     $a  $rest
    => 1 + sum!(2, 3)
    //          ^  ^
    //         $a  $rest
    => 1 + (2 + sum!(3))
    //               ^
    //               $base
    => 1 + (2 + (3))

# Recursion limit

When the compiler is expanding macros too deeply, it will give up. By default the compiler will fail after expanding macros to 64 levels deep, so e.g. the following expansion will cause failure:

    sum!(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
         21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
         41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62)

    // error: recursion limit reached while expanding the macro `sum`
    //  --> <anon>:3:46
    // 3 |>         ($a:expr, $($rest:expr),+) => { $a + sum!($($rest),+) };
    //   |>                                              ^^^^^^^^^^^^^^^^

When a recursion limit is reached, you should consider refactoring your macro, e.g.

* Maybe recursion could be replaced by repetition?
* Maybe the input format could be changed to something less fancy, so we don't need recursion to match it?

If there is any legitimate reason 64 levels is not enough, you could always increase the limit of the crate invoking the macro with the attribute:

    #![recursion_limit="128"]
    //                  ^~~ set the recursion limit to 128 levels deep.

## Multiple patterns
A macro can produce different outputs against different input patterns:

    /// The `sum` macro may be invoked in two ways:
    /// 
    ///     sum!(iterator)
    ///     sum!(1234, iterator)
    ///
    macro_rules! sum {
        ($iter:expr) => {    // This branch handles the `sum!(iterator)` case
            $iter.fold(0, |a, b| a + *b)
        };
    //   ^ use `;` to separate each branch
        ($start:expr, $iter:expr) => {  // This branch handles the `sum!(1234, iter)` case
            $iter.fold($start, |a, b| a + *b)
        };
    }
    
    fn main() {
        assert_eq!(10, sum!([1, 2, 3, 4].iter()));
        assert_eq!(23, sum!(6, [2, 5, 9, 1].iter()));
    }



## Exporting and importing macros
Exporting a macro to allow other modules to use it:

        #[macro_export]
    //  ^~~~~~~~~~~~~~~ Think of it as `pub` for macros.
        macro_rules! my_macro { (..) => {..} }

Using macros from other crates or modules:

       #[macro_use] extern crate lazy_static;
    // ^~~~~~~~~~~~ Must add this in order to use macros from other crates 

       #[macro_use] mod macros;
    // ^~~~~~~~~~~~ The same for child modules.



## Debugging macros
(All of these are unstable, and thus can only be used from a nightly compiler.)

# log_syntax!()

    #![feature(log_syntax)]

    macro_rules! logged_sum {
        ($base:expr) => { 
            { log_syntax!(base = $base); $base } 
        };
        ($a:expr, $($rest:expr),+) => { 
            { log_syntax!(a = $a, rest = $($rest),+); $a + logged_sum!($($rest),+) } 
        };
    }

    const V: u32 = logged_sum!(1, 2, 3);

During compilation, it will print the following to stdout:

> a = 1 , rest = 2 , 3  
> a = 2 , rest = 3  
> base = 3

# --pretty expanded

Run the compiler with:

```bash
rustc -Z unstable-options --pretty expanded filename.rs
```

This will expand all macros and then prints the expanded result to stdout, e.g. the above will probably output:

    #![feature(prelude_import)]
    #![no_std]
    #![feature(log_syntax)]
    #[prelude_import]
    use std::prelude::v1::*;
    #[macro_use]
    extern crate std as std;
    
    
    const V: u32 = { false; 1 + { false; 2 + { false; 3 } } };

(This is similar to the `-E` flag in the C compilers `gcc` and `clang`.)


