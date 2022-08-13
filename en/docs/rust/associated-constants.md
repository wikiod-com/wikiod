---
title: "Associated Constants"
slug: "associated-constants"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Syntax
 - #![feature(associated_consts)]
 - const ID: i32;

This feature is currently available only in nightly compiler. [Tracking issue #29646](https://github.com/rust-lang/rust/issues/29646)

## Using Associated Constants
    // Must enable the feature to use associated constants
    #![feature(associated_consts)]

    use std::mem;

    // Associated constants can be used to add constant attributes to types
    trait Foo {
        const ID: i32;
    }
    
    // All implementations of Foo must define associated constants
    // unless a default value is supplied in the definition.
    impl Foo for i32 {
        const ID: i32 = 1;
    }
    
    struct Bar;
    
    // Associated constants don't have to be bound to a trait to be defined
    impl Bar {
        const BAZ: u32 = 5;
    }
    
    fn main() {
        assert_eq!(1, i32::ID);

        // The defined constant value is only stored once, so the size of
        // instances of the defined types doesn't include the constants.
        assert_eq!(4, mem::size_of::<i32>());
        assert_eq!(0, mem::size_of::<Bar>());
    }

