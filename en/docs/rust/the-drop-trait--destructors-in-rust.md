---
title: "The Drop Trait - Destructors in Rust"
slug: "the-drop-trait---destructors-in-rust"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Using the Drop Trait does not mean that it will be run every time. While it will run when going out of scope or unwinding, it might not not always be the case, for example when `mem::forget` is called.

This is because a panic while unwinding causes the program to abort. It also might have been compiled with `Abort on Panic` switched on.

For more information check out the book: https://doc.rust-lang.org/book/drop.html

## Drop for Cleanup
    use std::ops::Drop;
    
    #[derive(Debug)]
    struct Bar(i32);
    
    impl Bar {
        fn get<'a>(&'a mut self) -> Foo<'a> {
            let temp = self.0; // Since we will also capture `self` we..
                               // ..will have to copy the value out first
            Foo(self, temp) // Let's take the i32
        }
    }
    
    struct Foo<'a>(&'a mut Bar, i32); // We specify that we want a mutable borrow..
                                      // ..so we can put it back later on
    
    impl<'a> Drop for Foo<'a> {
        fn drop(&mut self) {
            if self.1 < 10 { // This is just an example, you could also just put..
                             // ..it back as is
                (self.0).0 = self.1;
            }
        }
    }
    
    fn main() {
        let mut b = Bar(0);
        println!("{:?}", b);
        {
            let mut a : Foo = b.get(); // `a` now holds a reference to `b`..
            a.1 = 2;                   // .. and will hold it until end of scope
        }                              // .. here
            
        println!("{:?}", b);
        {
            let mut a : Foo = b.get();
            a.1 = 20;
        }
        println!("{:?}", b);
    }

Drop allows you to create simple and fail-safe Designs.

## Simple Drop Implementation
    use std::ops::Drop;
    
    struct Foo(usize);
    
    impl Drop for Foo {
        fn drop(&mut self) {
            println!("I had a {}", self.0);
        }
    }

## Drop Logging for Runtime Memory Management Debugging
Run-time memory management with Rc<T> can be very useful, but it can also be difficult to wrap one's head around, especially if your code is very complex and a single instance is referenced by tens or even hundreds of other types in many scopes.

Writing a Drop trait that includes `println!("Dropping StructName: {:?}", self);` can be immensely valuable for debugging, as it allows you to see precisely when the strong references to a struct instance run out.

