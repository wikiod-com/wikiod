---
title: "Custom derive Macros 1.1"
slug: "custom-derive-macros-11"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

Rust 1.15 added (stabilized) a new feature: Custom derive aka Macros 1.1.

Now apart from usual `PartialEq` or `Debug` you can have `#[deriving(MyOwnDerive)]`. Two primary users of the feature is [serde][1] and [diesel][2].

Rust Book link: https://doc.rust-lang.org/stable/book/procedural-macros.html

[1]:https://docs.serde.rs/serde/
[2]:http://docs.diesel.rs/diesel/index.html

## Getters and setters
Cargo.toml:

```ini
[package]
name = "gettersetter"
version = "0.1.0"
[lib]
proc-macro=true
[dependencies]
quote="^0.3.12"
syn="^0.11.4"
```

src/lib.rs:

```rust
#![crate_type = "proc-macro"]
extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;
#[macro_use]
extern crate quote;

#[proc_macro_derive(GetSet)]
pub fn qqq(input: TokenStream) -> TokenStream {
    let source = input.to_string();
    let ast = syn::parse_derive_input(&source).unwrap();
    
    let struct_name = &ast.ident;
    if let syn::Body::Struct(s) = ast.body {
        let field_names : Vec<_> = s.fields().iter().map(|ref x|
                x.ident.clone().unwrap()).collect();
        
        let field_getter_names = field_names.iter().map(|ref x|
                syn::Ident::new(format!("get_{}", x).as_str()));
        let field_setter_names = field_names.iter().map(|ref x|
                syn::Ident::new(format!("set_{}", x).as_str()));
        let field_types : Vec<_> = s.fields().iter().map(|ref x|
                x.ty.clone()).collect();
        let field_names2 = field_names.clone();
        let field_names3 = field_names.clone();
        let field_types2 = field_types.clone();
        
        let quoted_code = quote!{
            #[allow(dead_code)]
            impl #struct_name {
                #(
                    fn #field_getter_names(&self) -> &#field_types {
                        &self.#field_names2
                    }
                    fn #field_setter_names(&mut self, x : #field_types2) {
                        self.#field_names3 = x;
                    }
                )*
            }
        };
        return quoted_code.parse().unwrap();
    }
    // not a struct
    "".parse().unwrap()
}
```

examples/hello.rs:

```rust
#[macro_use]
extern crate gettersetter;

#[derive(GetSet)]
struct Qqq {
    x : i32,
    y : String,
}

fn main(){
    let mut a = Qqq { x: 3, y: "zzaaqq".to_string() };
    println!("{}", a.get_x());
    a.set_y("123213".to_string());
    println!("{}", a.get_y());
}
```

See also: https://github.com/emk/accessors

## Verbose dumpy helloworld
Cargo.toml:

```ini
[package]
name = "customderive"
version = "0.1.1"

[lib]
proc-macro=true

[dependencies]
quote="^0.3.12"
syn="^0.11.4"
```

src/lib.rs:

```rust
#![crate_type = "proc-macro"]
extern crate proc_macro;
use proc_macro::TokenStream;

extern crate syn;
#[macro_use]
extern crate quote;

#[proc_macro_derive(Hello)]
pub fn qqq(input: TokenStream) -> TokenStream {
    let source = input.to_string();
    println!("Normalized source code: {}", source);    
    let ast = syn::parse_derive_input(&source).unwrap();
    println!("Syn's AST: {:?}", ast); // {:#?} - pretty print
    let struct_name = &ast.ident;
    let quoted_code = quote!{
        fn hello() {
            println!("Hello, {}!", stringify!(#struct_name));
        }
    };
    println!("Quoted code: {:?}", quoted_code);
    quoted_code.parse().unwrap()
}
```

examples/hello.rs:

```rust
#[macro_use]
extern crate customderive;

#[derive(Hello)]
struct Qqq;

fn main(){
    hello();
}
```

output:

```
$ cargo run --example hello
   Compiling customderive v0.1.1 (file:///tmp/cd)
Normalized source code: struct Qqq;
Syn's AST: DeriveInput { ident: Ident("Qqq"), vis: Inherited, attrs: [], generics: Generics { lifetimes: [], ty_params: [], where_clause: WhereClause { predicates: [] } }, body: Struct(Unit) }
Quoted code: Tokens("fn hello ( ) { println ! ( \"Hello, {}!\" , stringify ! ( Qqq ) ) ; }")
warning: struct is never used: <snip>
    Finished dev [unoptimized + debuginfo] target(s) in 3.79 secs
     Running `target/x86_64-unknown-linux-gnu/debug/examples/hello`
Hello, Qqq!
```

## Minimal dummy custom derive
Cargo.toml:

```ini
[package]
name = "customderive"
version = "0.1.0"
[lib]
proc-macro=true
```

src/lib.rs:

```rust
#![crate_type = "proc-macro"]
extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_derive(Dummy)]
pub fn qqq(input: TokenStream) -> TokenStream {
    "".parse().unwrap()
}
```

examples/hello.rs

```rust
#[macro_use]
extern crate customderive;

#[derive(Dummy)]
struct Qqq;

fn main(){}
```

