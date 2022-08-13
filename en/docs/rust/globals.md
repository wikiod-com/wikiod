---
title: "Globals"
slug: "globals"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

## Syntax
- const IDENTIFIER: type = constexpr;
- static [mut] IDENTIFIER: type = expr;
- lazy_static! { static ref IDENTIFIER: type = expr; }

- `const` values are always inlined and have no address in memory.
- `static` values are never inlined and have one instance with a fixed address.
- `static mut` values are not memory safe and thus can only be accessed in an `unsafe` block.
- Sometimes using global static mutable variables in multi-threaded code can be dangerous, so consider using [std::sync::Mutex](http://doc.rust-lang.org/std/sync/struct.Mutex.html) or other alternatives
- `lazy_static` objects are immutable, are initialized only once, are shared among all threads, and can be directly accessed (there are no wrapper types involved). In contrast, `thread_local` objects are meant to be mutable, are initialized once for each thread, and accesses are indirect (involving the wrapper type `LocalKey<T>`)


## Const
The `const` keyword declares a global constant binding.

    const DEADBEEF: u64 = 0xDEADBEEF;

    fn main() {
        println("{:X}", DEADBEEF);
    }

This outputs

    DEADBEEF

## Static
The `static` keyword declares a global static binding, which may be mutable.

    static HELLO_WORLD: &'static str = "Hello, world!";

    fn main() {
        println("{}", HELLO_WORLD);
    }

This outputs

    Hello, world!

## lazy_static!
Use the `lazy_static` crate to create global immutable variables which are initialized at runtime. We use `HashMap` as a demonstration.

In `Cargo.toml`:

    [dependencies]
    lazy_static = "0.1.*"

In `main.rs`:

    #[macro_use]
    extern crate lazy_static;

    lazy_static! {
        static ref HASHMAP: HashMap<u32, &'static str> = {
            let mut m = HashMap::new();
            m.insert(0, "hello");
            m.insert(1, ",");
            m.insert(2, " ");
            m.insert(3, "world");
            m
        };
        static ref COUNT: usize = HASHMAP.len();
    }

    fn main() {
        // We dereference COUNT because it's type is &usize
        println!("The map has {} entries.", *COUNT);

        // Here we don't dereference with * because of Deref coercions
        println!("The entry for `0` is \"{}\".", HASHMAP.get(&0).unwrap());
    }

## Thread-local Objects
A thread-local object gets initialized on its first use in a thread. And as the name suggests, each thread will get a fresh copy independent of other threads.

    use std::cell::RefCell;
    use std::thread;
    
    thread_local! {
        static FOO: RefCell<f32> = RefCell::new(1.0);
    }
    
    // When this macro expands, `FOO` gets type `thread::LocalKey<RefCell<f32>>`.
    //
    // Side note: One of its private member is a pointer to a function which is
    // responsible for returning the thread-local object. Having all its members
    // `Sync` [0], `LocalKey` is also implicitly `Sync`.
    //
    // [0]: As of writing this, `LocalKey` just has 2 function-pointers as members
    
    fn main() {
        FOO.with(|foo| {
            // `foo` is of type `&RefCell<f64>`
            *foo.borrow_mut() = 3.0;
        });
    
        thread::spawn(move|| {
            // Note that static objects do not move (`FOO` is the same everywhere),
            // but the `foo` you get inside the closure will of course be different.
            FOO.with(|foo| {
                println!("inner: {}", *foo.borrow());
            });
        }).join().unwrap();
    
        FOO.with(|foo| {
            println!("main: {}", *foo.borrow());
        });
    }

Outputs:

    inner: 1
    main: 3

## Safe static mut with mut_static
Mutable global items (called `static mut`, highlighting the inherent contradiction involved in their use) are unsafe because it is difficult for the compiler to ensure they are used appropriately.

However, the introduction of mutually exclusive locks around data allows memory-safe mutable globals. This does NOT mean that they are logically safe, though!

    #[macro_use]
    extern crate lazy_static;
    extern crate mut_static;
    
    use mut_static::MutStatic;
    
    pub struct MyStruct { value: usize }
    
    impl MyStruct {
        pub fn new(v: usize) -> Self{
            MyStruct { value: v }
        }
        pub fn getvalue(&self) -> usize { self.value }
        pub fn setvalue(&mut self, v: usize) { self.value = v }
    }
    
    lazy_static! {
        static ref MY_GLOBAL_STATE: MutStatic<MyStruct> = MutStatic::new();
    }
    
    fn main() {
        // Here, I call .set on the MutStatic to put data inside it.
        // This can fail.
        MY_GLOBAL_STATE.set(MyStruct::new(0)).unwrap();
        {
            // Using the global state immutably is easy...
            println!("Before mut: {}", 
                     MY_GLOBAL_STATE.read().unwrap().getvalue());
        }
        {
             // Using it mutably is too...
             let mut mut_handle = MY_GLOBAL_STATE.write().unwrap();
             mut_handle.setvalue(3);
             println!("Changed value to 3.");
        } 
        {
            // As long as there's a scope change we can get the 
            // immutable version again...
            println!("After mut: {}", 
                     MY_GLOBAL_STATE.read().unwrap().getvalue());
        }
        {
            // But beware! Anything can change global state!
            foo();
            println!("After foo: {}", 
                     MY_GLOBAL_STATE.read().unwrap().getvalue());
        }
     
    }
    
    // Note that foo takes no parameters
    fn foo() {
        let val;
        {
            val = MY_GLOBAL_STATE.read().unwrap().getvalue();
        }
        {
            let mut mut_handle = 
                MY_GLOBAL_STATE.write().unwrap();
            mut_handle.setvalue(val + 1);
        }
    }

This code produces the output:

    Before mut: 0
    Changed value to 3.
    After mut: 3
    After foo: 4

This is not something that should happen in Rust normally. `foo()` did not take a mutable reference to anything, so it should not have mutated anything, and yet it did so. This can lead to very hard to debug logic errors.

On the other hand, this is sometimes exactly what you want. For instance, many game engines require a global cache of images and other resources which is lazily loaded (or uses some other complex loading strategy) - MutStatic is perfect for that purpose.

