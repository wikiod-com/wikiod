---
title: "Operators and Overloading"
slug: "operators-and-overloading"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

Most operators in Rust can be defined ("overloaded") for user-defined types. This can be achieved by implementing the respective trait in [`std::ops`](https://doc.rust-lang.org/std/ops/index.html#traits) module.

## Overloading the addition operator (+)
Overloading the addition operator (+) requires implement the [`std::ops::Add`](https://doc.rust-lang.org/std/ops/trait.Add.html) trait.

From the documentation, the full definition of the trait is:

    pub trait Add<RHS = Self> {
        type Output;
        fn add(self, rhs: RHS) -> Self::Output;
    }

How does it work?

 - the trait is implemented for the Left Hand Side type
 - the trait is implemented for *one* Right Hand Side argument, unless specified it defaults to having the same type as the Left Hand Side one
 - the type of the result of the addition is specified in the associated type `Output`

Thus, having 3 different types is possible.

*Note: the trait consumes is left-hand side and right-hand side arguments, you may prefer to implement it for references to your type rather than the bare types.*

---

Implementing `+` for a custom type:

    use std::ops::Add;

    #[derive(Clone)]
    struct List<T> {
        data: Vec<T>,
    }
    //  Implementation which consumes both LHS and RHS
    impl<T> Add for List<T> {
        type Output = List<T>;

        fn add(self, rhs: List<T>) -> List<T> {
            self.data.extend(rhs.data.drain(..));
            self
        }
    }

    //  Implementation which only consumes RHS (and thus where LHS != RHS)
    impl<'a, T: Clone> Add<List<T>> for &'a List<T> {
        type Output = List<T>;

        fn add(self, rhs: List<T>) -> List<T> {
            self.clone() + rhs
        }
    }



