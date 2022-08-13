---
title: "Conversion traits"
slug: "conversion-traits"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

- `AsRef` and `Borrow` are similar but serve distinct purposes. `Borrow` is used to treat multiple borrowing methods similarly, or to treat borrowed values like their owned counterparts, while `AsRef` is used for genericizing references.
- `From<A> for B` implies `Into<B> for A`, but not vice-versa.
- `From<A> for A` is implicitly implemented.

## From
Rust's `From` trait is a general-purpose trait for converting between types. For any two types `TypeA` and `TypeB`,

    impl From<TypeA> for TypeB

indicates that an instance of `TypeB` is *guaranteed* to be constructible from an instance of `TypeA`. An implementation of `From` looks like this:

    struct TypeA {
        a: u32,
    }

    struct TypeB {
        b: u32,
    }

    impl From<TypeA> for TypeB {
        fn from(src: TypeA) -> Self {
            TypeB {
                b: src.a,
            }
        }
    }

## AsRef & AsMut
`std::convert::AsRef` and `std::convert::AsMut` are used for cheaply converting types to references. For types `A` and `B`,

    impl AsRef<B> for A

indicates that a `&A` can be converted to a `&B` and,

    impl AsMut<B> for A

indicates that a `&mut A` can be converted to a `&mut B`.

This is useful for performing type conversions without copying or moving values. An example in the standard library is `std::fs::File.open()`:

    fn open<P: AsRef<Path>>(path: P) -> Result<File>

This allows `File.open()` to accept not only `Path`, but also `OsStr`, `OsString`, `str`, `String`, and `PathBuf` with implicit conversion because these types all implement `AsRef<Path>`.

    

## Borrow, BorrowMut and ToOwned
The `std::borrow::Borrow` and `std::borrow::BorrowMut` traits are used to treat borrowed types like owned types. For types `A` and `B`,

    impl Borrow<B> for A

indicates that a borrowed `A` may be used where a `B` is desired. For instance, `std::collections::HashMap.get()` uses `Borrow` for its `get()` method, allowing a `HashMap` with keys of `A` to be indexed with a `&B`.

---

On the other hand, `std::borrow::ToOwned` implements the reverse relationship.

Thus, with the aforementioned types `A` and `B` one can implement:

    impl ToOwned for B

*Note: while `A` can implement `Borrow<T>` for multiple distinct types `T`, `B` can only implement `ToOwned` once.*

## Deref & DerefMut
The `std::ops::Deref` and `std::ops::DerefMut` traits are used for overloading the dereference operator, `*x`. For types `A` and `B`,

    impl Deref<Target=B> for A

indicates that dereferencing a binding of `&A` will yield a `&B` and,

    impl DerefMut for A

indicates that dereferencing a binding of `&mut A` will yield a `&mut B`.

`Deref` (resp. `DerefMut`) also provides a useful language feature called *deref coercion*, which allows a `&A` (resp. `&mut A`) to automatically coerce to a `&B` (resp. `&mut B`). This is commonly used when converting from `String` to `&str`, as `&String` is implicitly coerced to `&str` as needed.

---

*Note: `DerefMut` does not support specifying the resulting type, it uses the same type as `Deref` does.*

*Note: Due to the use of an associated type (unlike in `AsRef`), a given type can only implement each of `Deref` and `DerefMut` at most once.*

