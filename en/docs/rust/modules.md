---
title: "Modules"
slug: "modules"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Syntax
* mod *modname*; // Search for the module in *modname*.rs or *modname*/mod.rs in the same directory
* mod *modname* { *block* }


## The #[path] attribute
Rust's `#[path]` attribute can be used to specify the path to search for a particular module if it is not in the standard location. This is [typically discouraged](https://github.com/rust-lang-nursery/fmt-rfcs/blob/e72f990b033eb6a2ed7836aa0199a2c3095372c8/guide/guide.md#modules-1), however, because it makes the module hierarchy fragile and makes it easy to break the build by moving a file in a completely different directory.

    #[path="../path/to/module.rs"]
    mod module;

## Modules tree
Files:
<!-- language: none -->
```
- example.rs (root of our modules tree, generally named lib.rs or main.rs when using Cargo)
- first.rs
- second/
  - mod.rs
  - sub.rs
```

Modules:
<!-- language: lang-none -->
```
- example        -> example
  - first        -> example::first
  - second       -> example::second
    - sub        -> example::second::sub
  - third        -> example::third
```

`example.rs`
```
pub mod first;
pub mod second;
pub mod third {
    ...
}
```
*The module `second` have to be declared in the `example.rs` file as its parent is `example` and not, for example, `first` and thus cannot be declared in the `first.rs`  or another file in the same directory level*

`second/mod.rs`
```
pub mod sub;
```

## Names in code vs names in `use`
The double-colon syntax of names in the `use` statement looks similar to names used elsewhere in the code, but meaning of these paths is different.

Names in the `use` statement by default are interpreted as absolute, starting at the crate root. Names elsewhere in the code are relative to the current module.

The statement:

    use std::fs::File;

has the same meaning in the main file of the crate as well as in modules. On the other hand, a function name such as `std::fs::File::open()` will refer to Rust's standard library only in the main file of the crate, because names in the code are interpreted relative to the current module.

    fn main() {
        std::fs::File::open("example"); // OK
    }

    mod my_module {
       fn my_fn() {
           // Error! It means my_module::std::fs::File::open()
           std::fs::File::open("example"); 

           // OK. `::` prefix makes it absolute 
           ::std::fs::File::open("example"); 

           // OK. `super::` reaches out to the parent module, where `std` is present
           super::std::fs::File::open("example"); 
       } 
    }

To make `std::…` names behave everywhere the same as in the root of the crate you could add:

    use std;


Conversely, you can make `use` paths relative by prefixing them with `self` or `super` keywords:

     use self::my_module::my_fn;

## Accessing the Parent Module
Sometimes, it can be useful to import functions and structs relatively without having to `use` something with its absolute path in your project. To achieve this, you can use the module `super`, like so:

    fn x() -> u8 {
        5
    }

    mod example {
        use super::x;

        fn foo() {
            println!("{}", x());
        }
    }

You can use `super` multiple times to reach the 'grandparent' of your current module, but you should be wary of introducing readability issues if you use `super` too many times in one import. 

## Exports and Visibility


## Basic Code Organization
Let's see how we can organize the code, when the codebase is getting larger.

**01. Functions**

```rust
fn main() {
  greet();
}

fn greet() {
  println!("Hello, world!");
}
```

**02. Modules - In the same file**

```rust
fn main() {
  greet::hello();
}

mod greet {
  // By default, everything inside a module is private
  pub fn hello() { // So function has to be public to access from outside
    println!("Hello, world!");
  }
}
```

**03. Modules - In a different file in the same directory**

When move some code to a new file, no need to wrap the code in a `mod` declaration. File itself acts as a module.

```rust
// ↳ main.rs
mod greet; // import greet module

fn main() {
  greet::hello();
}
```

```rust
// ↳ greet.rs
pub fn hello() { // function has to be public to access from outside
  println!("Hello, world!");
}
```


> When move some code to a new file, if that code has been wrapped from
> a `mod` declaration, that will be a sub module of the file.

```rust
// ↳ main.rs
mod greet;

fn main() {
  greet::hello::greet();
}
```

```rust
// ↳ greet.rs
pub mod hello { // module has to be public to access from outside
  pub fn greet() { // function has to be public to access from outside
    println!("Hello, world!");
  }
}
```


**04. Modules - In a different file in a different directory**

When move some code to a new file in a different directory, directory itself acts as a module. And `mod.rs` in the module root is the entry point to the directory module. All other files in that directory, acts as a sub module of that directory.

```rust
// ↳ main.rs
mod greet;

fn main() {
  greet::hello();
}
```

```rust
// ↳ greet/mod.rs
pub fn hello() {
  println!("Hello, world!");
}
```

> When you have multiple files in the module root,

```rust
// ↳ main.rs
mod greet;

fn main() {
  greet::hello_greet()
}
```

```rust
// ↳ greet/mod.rs
mod hello;

pub fn hello_greet() {
  hello::greet()
}
```

```rust
// ↳ greet/hello.rs
pub fn greet() {
  println!("Hello, world!");
}
```


**05. Modules - With `self`**


```rust
fn main() {
  greet::call_hello();
}

mod greet {
  pub fn call_hello() { 
    self::hello();
  }

  fn hello() {
    println!("Hello, world!");
  }
}
```

**06. Modules - With `super`**

01. When you want to access a root function from inside a module,

```rust
fn main() {
  dash::call_hello();
}

fn hello() {
  println!("Hello, world!");
}

mod dash {
  pub fn call_hello() {
    super::hello();
  }
}
```

02. When you want to access a function in outer/ parent module from inside a nested module,

```rust
fn main() {
  outer::inner::call_hello();
}

mod outer {

  pub fn hello() {
    println!("Hello, world!");
  }

  mod inner {
    pub fn call_hello() {
      super::hello();
    }
  }

}
```

**07. Modules - With `use`**

01. When you want to bind the full path to a new name,

```rust
use greet::hello::greet as greet_hello;

fn main() {
  greet_hello();
}

mod greet {
  pub mod hello {
    pub fn greet() {
      println!("Hello, world!");
    }
  }
}
```

02. When you want to use crate scope level content

```rust
fn main() {
  user::hello();
}

mod greet {
  pub mod hello {
    pub fn greet() {
      println!("Hello, world!");
    }
  }
}

mod user {
  use greet::hello::greet as call_hello;

  pub fn hello() {
    call_hello();
  }
}
```


