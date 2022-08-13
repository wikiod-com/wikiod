---
title: "Traits"
slug: "traits"
draft: false
images: []
weight: 9808
type: docs
toc: true
---

Traits are a way of describing a 'contract' that a `struct` must implement. Traits typically define method signatures but can also provide implementations based on other methods of the trait, providing the *trait bounds* allow for this.

For those familiar with object oriented programming, traits can be thought of as interfaces with some subtle differences.

## Syntax
- trait Trait { fn method(...) -> ReturnType; ... }
- trait Trait: Bound { fn method(...) -> ReturnType; ... }
- impl Trait for Type { fn method(...) -> ReturnType { ... } ... }
- impl\<T\> Trait for T where T: Bounds { fn method(...) -> ReturnType { ... } ... }

- Traits are commonly likened to interfaces, but it is important to make a distinction between the two. In OO languages like Java, interfaces are an integral part of the classes that extend them. In Rust, the compiler knows nothing of a struct's traits unless those traits are used.

## Basics
## Creating a Trait

    trait Speak {
        fn speak(&self) -> String;
    }

## Implementing a Trait

    struct Person;
    struct Dog;

    impl Speak for Person {
        fn speak(&self) -> String {
            String::from("Hello.")
        }
    }

    impl Speak for Dog {
        fn speak(&self) -> String {
            String::from("Woof.")
        }
    }

    fn main() {
        let person = Person {};
        let dog = Dog {};
        println!("The person says {}", person.speak());
        println!("The dog says {}", dog.speak());
    }

## Static and Dynamic Dispatch
It is possible to create a function that accepts objects that implement a specific trait.

## Static Dispatch

    fn generic_speak<T: Speak>(speaker: &T) {
        println!("{0}", speaker.speak());
    }
    
    fn main() {
        let person = Person {};
        let dog = Dog {};
    
        generic_speak(&person);
        generic_speak(&dog);
    }

Static dispatch is used here, which means that Rust compiler will generate specialized versions of `generic_speak` function for both `Dog` and `Person` types. This generation of specialized versions of a polymorphic function (or any polymorphic entity) during compilation is called **Monomorphization**.

## Dynamic Dispatch

    fn generic_speak(speaker: &Speak) {
        println!("{0}", speaker.speak());
    }

    fn main() {
        let person = Person {};
        let dog = Dog {};

        generic_speak(&person as &Speak);
        generic_speak(&dog); // gets automatically coerced to &Speak
    }

Here, only a single version of `generic_speak` exists in the compiled binary, and the `speak()` call is made using a [vtable][1] lookup at runtime. Thus, using dynamic dispatch results in faster compilation and smaller size of compiled binary, while being slightly slower at runtime.

Objects of type `&Speak` or `Box<Speak>` are called **trait objects**.


  [1]: https://en.wikipedia.org/wiki/Virtual_method_table

## Default methods
    trait Speak {
        fn speak(&self) -> String {
            String::from("Hi.")
        }
    }

The method will be called by default except if it's overwritten in the `impl` block.

    struct Human;
    struct Cat;

    impl Speak for Human {}

    impl Speak for Cat {
        fn speak(&self) -> String {
            String::from("Meow.")
        }
    }

    fn main() {
        let human = Human {};
        let cat = Cat {};
        println!("The human says {}", human.speak());
        println!("The cat says {}", cat.speak());
    }

Output :
> The human says Hi.
>
> The cat says Meow.

## Associated Types
* Use associated type when there is a one-to-one relationship between the type implementing the trait and the associated type.
* It is sometimes also known as the *output type*, since this is an item given to a type when we apply a trait to it.

# Creation

    trait GetItems {
        type First;
    //  ^~~~ defines an associated type. 
        type Last: ?Sized;
    //           ^~~~~~~~ associated types may be constrained by traits as well
        fn first_item(&self) -> &Self::First;
    //                           ^~~~~~~~~~~ use `Self::` to refer to the associated type 
        fn last_item(&self) -> &Self::Last;
    //                          ^~~~~~~~~~ associated types can be used as function output...
        fn set_first_item(&mut self, item: Self::First);
    //                                     ^~~~~~~~~~~  ... input, and anywhere.
    }

# Implemention

    impl<T, U: ?Sized> GetItems for (T, U) {
        type First = T;
        type Last = U;
    //              ^~~ assign the associated types
        fn first_item(&self) -> &Self::First { &self.0 }
        fn last_item(&self) -> &Self::Last { &self.1 }
        fn set_first_item(&mut self, item: Self::First) { self.0 = item; }
    }
    
    impl<T> GetItems for [T; 3] {
        type First = T;
        type Last = T;
        fn first_item(&self) -> &T { &self[0] }
    //                           ^ you could refer to the actual type instead of `Self::First`
        fn last_item(&self) -> &T { &self[2] }
        fn set_first_item(&mut self, item: T) { self[0] = item; }
    }

# Refering to associated types

If we are sure that a type `T` implements `GetItems` e.g. in generics, we could simply use `T::First` to obtain the associated type.

    fn get_first_and_last<T: GetItems>(obj: &T) -> (&T::First, &T::Last) {
    //                                               ^~~~~~~~ refer to an associated type
        (obj.first_item(), obj.last_item())
    }

Otherwise, you need to explicitly tell the compiler which trait the type is implementing 

    let array: [u32; 3] = [1, 2, 3];
    let first: &<[u32; 3] as GetItems>::First = array.first_item();
    //          ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~ [u32; 3] may implement multiple traits which many
    //                                        of them provide the `First` associated type.
    //                                        thus the explicit "cast" is necessary here.
    assert_eq!(*first, 1);


# Constraining with associated types

    fn clone_first_and_last<T: GetItems>(obj: &T) -> (T::First, T::Last)
        where T::First: Clone, T::Last: Clone
    //  ^~~~~ use the `where` clause to constraint associated types by traits
    {
        (obj.first_item().clone(), obj.last_item().clone())
    }

    fn get_first_u32<T: GetItems<First=u32>>(obj: &T) -> u32 {
    //                          ^~~~~~~~~~~ constraint associated types by equality
        *obj.first_item()
    }


## Placing a bound on a trait
When defining a new trait it is possible to enforce that types wishing to implement this trait verify a number of constraints or bounds.

Taking an example from the standard library, the [`DerefMut`](https://doc.rust-lang.org/std/ops/trait.DerefMut.html) trait requires that a type first implement its sibling `Deref` trait:

    pub trait DerefMut: Deref {
        fn deref_mut(&mut self) -> &mut Self::Target;
    }

This, in turn, enables `DerefMut` to use the associated type `Target` defined by `Deref`.

---

While the syntax might be reminiscent of inheritance:

 - it brings in all the associated items (constants, types, functions, ...) of the bound trait
 - it enables polymorphism from `&DerefMut` to `&Deref`

This is different in nature:

 - it is possible to use a lifetime (such as `'static`) as a bound
 - it is not possible to override the bound trait items (not even the functions)

Thus it is best to think of it as a separate concept.

## Multiple bound object types
It's also possible to add multiple object types to a [Static Dispatch](https://www.wikiod.com/rust/traits#Static and Dynamic Dispatch) function.

    fn mammal_speak<T: Person + Dog>(mammal: &T) {
        println!("{0}", mammal.speak());
    }
    
    fn main() {
        let person = Person {};
        let dog = Dog {};
    
        mammal_speak(&person);
        mammal_speak(&dog);
    }

