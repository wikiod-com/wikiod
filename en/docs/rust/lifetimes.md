---
title: "Lifetimes"
slug: "lifetimes"
draft: false
images: []
weight: 9896
type: docs
toc: true
---

## Syntax
- fn function<'a>(x: &'a Type)
- struct Struct<'a> { x: &'a Type }
- enum Enum<'a> { Variant(&'a Type) }
- impl<'a> Struct<'a> { fn x<'a>(&self) -> &'a Type { self.x } }
- impl<'a> Trait<'a> for Type
- impl<'a> Trait for Type<'a>
- `fn function<F>(f: F) where for<'a> F: FnOnce(&'a Type)`
- `struct Struct<F> where for<'a> F: FnOnce(&'a Type) { x: F }`
- `enum Enum<F> where for<'a> F: FnOnce(&'a Type) { Variant(F) }`
- `impl<F> Struct<F> where for<'a> F: FnOnce(&'a Type) { fn x(&self) -> &F { &self.x } }`


- All references in Rust have a lifetime, even if they are not explicitly annotated. The compiler is capable of implicitly assigning lifetimes.
- The `'static` lifetime is assigned to references that are stored in the program binary and will be valid throughout its entire execution. This lifetime is most notably assigned to string literals, which have the type `&'static str`.

## Higher-Rank Trait Bounds
    fn copy_if<F>(slice: &[i32], pred: F) -> Vec<i32>
        where for<'a> F: Fn(&'a i32) -> bool
    {
        let mut result = vec![];
        for &element in slice {
            if pred(&element) {
                result.push(element);
            }
        }
        result
    }

This specifies that the reference on i32 in the `Fn` trait bound can have any lifetime.

The following does not work:

    fn wrong_copy_if<'a, F>(slice: &[i32], pred: F) -> Vec<i32>
        where F: Fn(&'a i32) -> bool
    {                                   // <----------------+
        let mut result = vec![];        //       'a scope   |
        for &element in slice {         // <--------+       |
            if pred(&element) {         //          |       |
                result.push(element);   // element's|       |
            }                           //   scope  |       |
        }                               // <--------+       |
        result                          //                  |
    }                                   // <----------------+

The compiler gives the following error:

    error: `element` does not live long enough
    if pred(&element) {         //          |       |
             ^~~~~~~

because the `element` local variable does not live as long as the `'a` lifetime (as we can see from the code's comments).

The lifetime cannot be declared at the function level, because we need another lifetime.
That's why we used `for<'a>`: to specify that the reference can be valid for any lifetime (hence a smaller lifetime can be used).

Higher-Rank trait bounds can also be used on structs:

    struct Window<F>
        where for<'a> F: FnOnce(&'a Window<F>)
    {
        on_close: F,
    }

as well as on other items.

Higher-Rank trait bounds are most commonly used with the `Fn*` traits.

For these examples, the lifetime elision works fine so we do not have to specify the lifetimes.

## Impl Blocks
    impl<'a> Type<'a> {
        fn my_function(&self) -> &'a u32 {
            self.x
        }
    }

This specifies that `Type` has lifetime `'a`, and that the reference returned by `my_function()` may no longer be valid after `'a` ends because the `Type` no longer exists to hold `self.x`.

## Function Parameters (Input Lifetimes)
    fn foo<'a>(x: &'a u32) {
        // ...
    }

This specifies that `foo` has lifetime `'a`, and the parameter `x` must have a lifetime of at least `'a`. Function lifetimes are usually omitted through *lifetime elision*:

    fn foo(x: &u32) {
        // ...
    }

In the case that a function takes multiple references as parameters and returns a reference, the compiler cannot infer the lifetime of result through *lifetime elision*.

    error[E0106]: missing lifetime specifier
    1 | fn foo(bar: &str, baz: &str) -> &i32 {
      |                                 ^ expected lifetime parameter

Instead, the lifetime parameters should be explicitly specified.

    // Return value of `foo` is valid as long as `bar` and `baz` are alive.
    fn foo<'a>(bar: &'a str, baz: &'a str) -> &'a i32 {

Functions can take multiple lifetime parameters too.

    // Return value is valid for the scope of `bar`
    fn foo<'a, 'b>(bar: &'a str, baz: &'b str) -> &'a i32 {


## Struct Fields
    struct Struct<'a> {
        x: &'a u32,
    }

This specifies that any given instance of `Struct` has lifetime `'a`, and the `&u32` stored in `x` must have a lifetime of at least `'a`.

