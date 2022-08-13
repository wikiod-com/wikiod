---
title: "Structures"
slug: "structures"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Syntax
 - struct Foo { field1: Type1, field2: Type2 }
 - let foo = Foo { field1: Type1::new(), field2: Type2::new() };
 - struct Bar ( Type1, Type2 ); // tuple type
 - let _ = Bar(Type1::new(), Type2::new());
 - struct Baz; // unit-like type
 - let _ = Baz;
 - let Foo { field1, .. } = foo; // extract field1 by pattern matching
 - let Foo { field1: x, .. } = foo; // extract field1 as x
 - let foo2 = Foo { field1: Type1::new(), .. foo }; // construct from existing
 - impl Foo { fn fiddle(&self) {} } // declare instance method for Foo
 - impl Foo { fn tweak(&mut self) {} } // declare mutable instance method for Foo
 - impl Foo { fn double(self) {} } // declare owning instance method for Foo
 - impl Foo { fn new() {} } // declare associated method for Foo

## Creating and using structure values
Consider the following `struct` definitions:

    struct Foo {
        my_bool: bool,
        my_num: isize,
        my_string: String,
    }
    struct Bar (bool, isize, String);
    struct Baz;

Constructing new structure values for these types is straightforward:

    let foo = Foo { my_bool: true, my_num: 42, my_string: String::from("hello") };
    let bar = Bar(true, 42, String::from("hello"));
    let baz = Baz;

Access fields of a struct using `.`:

    assert_eq!(foo.my_bool, true);
    assert_eq!(bar.0, true); // tuple structs act like tuples

A mutable binding to a struct can have its fields mutated:

    let mut foo = foo;
    foo.my_bool = false;
    let mut bar = bar;
    bar.0 = false;

Rust's pattern-matching capabilities can also be used to peek inside a `struct`:

    // creates bindings mb, mn, ms with values of corresponding fields in foo
    let Foo { my_bool: mb, my_num: mn, my_string: ms } = foo;
    assert_eq!(mn, 42);
    // .. allows you to skip fields you do not care about
    let Foo { my_num: mn, .. } = foo;
    assert_eq!(mn, 42);
    // leave out `: variable` to bind a variable by its field name
    let Foo { my_num, .. } = foo;
    assert_eq!(my_num, 42);

Or make a struct using a second struct as a "template" with Rust's *update syntax*:

    let foo2 = Foo { my_string: String::from("world"), .. foo };
    assert_eq!(foo2.my_num, 42);

## Defining structures
Structures in Rust are defined using the `struct` keyword. The most common form of structure consists of a set of named fields:

    struct Foo {
        my_bool: bool,
        my_num: isize,
        my_string: String,
    }

The above declares a `struct` with three fields: `my_bool`, `my_num`, and `my_string`, of the types `bool`, `isize`, and `String` respectively.

Another way to create `struct`s in Rust is to create a *tuple struct*:

    struct Bar (bool, isize, String);

This defines a new type, `Bar`, that has three unnamed fields, of type `bool`, `isize`, and `String`, in that order. This is known as the [*newtype* pattern](https://aturon.github.io/features/types/newtype.html), because it effectively introduces a new "name" for a particular type. However, it does so in a more powerful manner than the aliases created using the `type` keyword; `Bar` is here a fully functional type, meaning you can write your own methods for it (below).

Finally, declare a `struct` with *no* fields, called a *unit-like struct*:

    struct Baz;

This can be useful for mocking or testing (when you want to trivially implement a trait), or as a marker type. In general, however, you are unlikely to come across many unit-like structs.

Note that `struct` fields in Rust are all private by default --- that is, they cannot be accessed from code outside of the module which defines the type. You can prefix a field with the `pub` keyword to make that field publicly accessible. In addition, the `struct` type *itself* is private. To make the type available to other modules, the `struct` definition must also be prefixed with `pub`:

    pub struct X {
        my_field: bool,
        pub our_field: bool,
    }

## Generic structures
Structures can be made generic over one or more type parameters. These types are given enclosed in `<>` when referring to the type:

    struct Gen<T> {
        x: T,
        z: isize,
    }
    
    // ...
    let _: Gen<bool> = Gen{x: true, z: 1};
    let _: Gen<isize> = Gen{x: 42, z: 2};
    let _: Gen<String> = Gen{x: String::from("hello"), z: 3};

Multiple types can be given using comma:

    struct Gen2<T, U> {
        x: T,
        y: U,
    }
    
    // ...
    let _: Gen2<bool, isize> = Gen2{x: true, y: 42};

The type parameters are a part of the type, so two variables of the same base type, but with different parameters, are not interchangeable:

    let mut a: Gen<bool> = Gen{x: true, z: 1};
    let b: Gen<isize> = Gen{x: 42, z: 2};
    a = b; // this will not work, types are not the same
    a.x = 42; // this will not work, the type of .x in a is bool

If you want to write a function that accepts a `struct` regardless of its type parameter assignment, that function would also need to be made generic:

    fn hello<T>(g: Gen<T>) {
        println!("{}", g.z); // valid, since g.z is always an isize
    }

But what if we wanted to write a function that could always print `g.x`? We would need to restrict `T` to be of a type that can be displayed. We can do this with type bounds:

    use std::fmt;
    fn hello<T: fmt::Display>(g: Gen<T>) {
        println!("{} {}", g.x, g.z);
    }

The `hello` function is now *only* defined for `Gen` instances whose `T` type implements `fmt::Display`. If we tried passing in a `Gen<(bool, isize)>` for example, the compiler would complain that `hello` is not defined for that type.

We can also use type bounds directly on the type parameters of the `struct` to indicate that you can only construct that `struct` for certain types:

    use std::hash::Hash;
    struct GenB<T: Hash> {
        x: T,
    }

Any function that has access to a `GenB` now knows that the type of `x` implements `Hash`, and thus that they can call `.x.hash()`. Multiple type bounds for the same parameter can be given by separating them with a `+`.

Same as for functions, the type bounds can be placed after the `<>` using the `where` keyword:

    struct GenB<T> where T: Hash {
        x: T,
    }

This has the same semantic meaning, but can make the signature easier to read and format when you have complex bounds.

Type parameters are also available to instance methods and associated methods of the `struct`:

    // note the <T> parameter for the impl as well
    // this is necessary to say that all the following methods only
    // exist within the context of those type parameter assignments
    impl<T> Gen<T> {
        fn inner(self) -> T {
            self.x
        }
        fn new(x: T) -> Gen<T> {
            Gen{x: x}
        }
    }

If you have type bounds on `Gen`'s `T`, those should also be reflected in the type bounds of the `impl`. You can also make the `impl` bounds tighter to say that a given method only exists if the type satisfies a particular property:

    impl<T: Hash + fmt::Display> Gen<T> {
        fn show(&self) {
            println!("{}", self.x);
        }
    }
    
    // ...
    Gen{x: 42}.show(); // works fine
    let a = Gen{x: (42, true)}; // ok, because (isize, bool): Hash
    a.show(); // error: (isize, bool) does not implement fmt::Display

## Structure methods
To declare methods on a struct (i.e., functions that can be called "on" the `struct`, or values of that `struct` type), create an `impl` block:

    impl Foo {
        fn fiddle(&self) {
            // "self" refers to the value this method is being called on
            println!("fiddling {}", self.my_string);
        }
    }

    // ...
    foo.fiddle(); // prints "fiddling hello"

`&self` here indicates an immutable reference to an instance of `struct Foo` is necessary to invoke the `fiddle` method. If we wanted to modify the instance (such as changing one of its fields), we would instead take an `&mut self` (i.e., a mutable reference):

    impl Foo {
        fn tweak(&mut self, n: isize) {
            self.my_num = n;
        }
    }

    // ...
    foo.tweak(43);
    assert_eq!(foo.my_num, 43);

Finally, we could also use `self` (note the lack of an `&`) as the receiver. This requires the instance to be owned by the caller, and will cause the instance to be moved when calling the method. This can be useful if we wish to consume, destroy, or otherwise entirely transform an existing instance. One example of such a use-case is to provide "chaining" methods:

    impl Foo {
        fn double(mut self) -> Self {
            self.my_num *= 2;
            self
        }
    }

    // ...
    foo.my_num = 1;
    assert_eq!(foo.double().double().my_num, 4);

Note that we also prefixed `self` with `mut` so that we can mutate self before returning it again. The return type of the `double` method also warrants some explanation. `Self` inside an `impl` block refers to the type that the `impl` applies to (in this case, `Foo`). Here, it is mostly a useful shorthand to avoid re-typing the signature of the type, but in traits, it can be used to refer to the underlying type that implements a particular trait.

To declare an *associated method* (commonly referred to as a "class method" in other languages) for a `struct` simply leave out the `self` argument. Such methods are called on the `struct` type itself, not on an instance of it:

    impl Foo {
        fn new(b: bool, n: isize, s: String) -> Foo {
            Foo { my_bool: b, my_num: n, my_string: s }
        }
    }
    
    // ...
    // :: is used to access associated members of the type
    let x = Foo::new(false, 0, String::from("nil"));
    assert_eq!(x.my_num, 0);

Note that structure methods can only be defined for types that were declared in the current module. Furthermore, as with fields, all structure methods are private by default, and can thus only be called by code in the same module. You can prefix definitions with the `pub` keyword to make them callable from elsewhere.

