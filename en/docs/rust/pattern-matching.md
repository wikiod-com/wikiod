---
title: "Pattern Matching"
slug: "pattern-matching"
draft: false
images: []
weight: 9893
type: docs
toc: true
---

## Syntax
* _ // wildcard pattern, matches anything¹
* *ident* // binding pattern, matches anything and binds it to *ident*¹
* *ident* @ *pat* // same as above, but allow to further match what is binded
* ref *ident* // binding pattern, matches anything and binds it to a reference *ident*¹
* ref mut *ident* // binding pattern, matches anything and binds it to a mutable reference *ident*¹
* &*pat* // matches a reference (*pat* is therefore not a reference but the referee)¹
* &mut *pat* // same as above with a mutable reference¹
* *CONST* // matches a named constant
* *Struct* { *field1*, *field2* } // matches and deconstructs a structure value, see below the note about fields¹
* *EnumVariant* // matches an enumeration variant
* *EnumVariant*(*pat1*, *pat2*) // matches a enumeration variant and the corresponding parameters
* *EnumVariant*(*pat1*, *pat2*, .., *patn*) // same as above but skips all but the first, second and last parameters
* (*pat1*, *pat2*) // matches a tuple and the corresponding elements¹
* (*pat1*, *pat2*, .., *patn*) // same as above but skips all but the first, second and last elements¹
* *lit* // matches a literal constant (char, numeric types, boolean and string)
* *pat1*...*pat2* // matches a value in that (inclusive) range (char and numeric types)


When deconstructing a structure value, the field should be either of the form *`field_name`* or <code>*field_name*: *pattern*</code>. If no pattern is specified, an implicit binding is done:

```rust
let Point { x, y } = p;
// equivalent to
let Point { x: x, y: y } = p;

let Point { ref x, ref y } = p;
// equivalent to
let Point { x: ref x, y: ref y } = p;
```

---

1: Irrefutable pattern

## Matching multiple patterns
It's possible to treat multiple, distinct values the same way, using `|`:
```
enum Colour {
    Red,
    Green,
    Blue,
    Cyan,
    Magenta,
    Yellow,
    Black
}

enum ColourModel {
    RGB,
    CMYK
}

// let's take an example colour
let colour = Colour::Red;

let model = match colour {
    // check if colour is any of the RGB colours
    Colour::Red | Colour::Green | Colour::Blue => ColourModel::RGB,
    // otherwise select CMYK
    _ => ColourModel::CMYK,
};
```


## Pattern matching with bindings
It is possible to bind values to names using `@`:
```

struct Badger {
    pub age: u8
}

fn main() {
    // Let's create a Badger instances
    let badger_john = Badger { age: 8 };

    // Now try to find out what John's favourite activity is, based on his age
    match badger_john.age {
        // we can bind value ranges to variables and use them in the matched branches
        baby_age @ 0...1 => println!("John is {} years old, he sleeps a lot", baby_age),
        young_age @ 2...4 => println!("John is {} years old, he plays all day", young_age),
        adult_age @ 5...10 => println!("John is {} years old, he eats honey most of the time", adult_age),
        old_age => println!("John is {} years old, he mostly reads newspapers", old_age),
    }
}
```
This will print:
```
John is 8 years old, he eats honey most of the time
```


## Basic pattern matching
```
// Create a boolean value
let a = true;

// The following expression will try and find a pattern for our value starting with
// the topmost pattern. 
// This is an exhaustive match expression because it checks for every possible value
match a {
  true => println!("a is true"),
  false => println!("a is false")
}
```

If we don't cover every case we will get a compiler error:

```
match a {
  true => println!("most important case")
}
// error: non-exhaustive patterns: `false` not covered [E0004]
```
We can use `_` as the default/wildcard case, it matches everything:

```
// Create an 32-bit unsigned integer
let b: u32 = 13;

match b {
  0 => println!("b is 0"),
  1 => println!("b is 1"),
  _ => println!("b is something other than 0 or 1")
}
```

This example will print:
```
a is true
b is something else than 0 or 1

## Conditional pattern matching with guards
Patterns can be matched based on values independent to the value being matched using `if` guards:

```
// Let's imagine a simplistic web app with the following pages:
enum Page {
  Login,
  Logout,
  About,
  Admin
}

// We are authenticated
let is_authenticated = true;

// But we aren't admins
let is_admin = false;

let accessed_page = Page::Admin;

match accessed_page {
    // Login is available for not yet authenticated users
    Page::Login if !is_authenticated => println!("Please provide a username and a password"),

    // Logout is available for authenticated users 
    Page::Logout if is_authenticated => println!("Good bye"),
    
    // About is a public page, anyone can access it
    Page::About => println!("About us"),

    // But the Admin page is restricted to administators
    Page::Admin if is_admin => println!("Welcome, dear administrator"),

    // For every other request, we display an error message
    _ => println!("Not available")
}
```
This will display _"Not available"_.


## if let / while let
---
## `if let`

Combines a pattern `match` and an `if` statement, and allows for brief non-exhaustive matches to be performed.

    if let Some(x) = option {
        do_something(x);
    }

This is equivalent to:

    match option {
        Some(x) => do_something(x),
        _ => {},
    }

---

These blocks can also have `else` statements.

    if let Some(x) = option {
        do_something(x);
    } else {
        panic!("option was None");
    }

This block is equivalent to:

    match option {
        Some(x) => do_something(x),
        None => panic!("option was None"),
    }
---
## `while let`

Combines a pattern match and a while loop.

    let mut cs = "Hello, world!".chars();
    while let Some(x) = cs.next() {
        print("{}+", x);
    }
    println!("");

This prints `H+e+l+l+o+,+ +w+o+r+l+d+!+`.

It's equivalent to using a `loop {}` and a `match` statement:
    
    let mut cs = "Hello, world!".chars();
    loop {
        match cs.next() {
            Some(x) => print("{}+", x),
            _ => break,
        }
    }
    println!("");

## Extracting references from patterns
Sometimes it's necessary to be able to extract values from an object using only references (ie. without transferring ownership). 

```
struct Token {
  pub id: u32
}

struct User {
  pub token: Option<Token>
}


fn main() {
    // Create a user with an arbitrary token
    let user = User { token: Some(Token { id: 3 }) };

    // Let's borrow user by getting a reference to it
    let user_ref = &user;

    // This match expression would not compile saying "cannot move out of borrowed
    // content" because user_ref is a borrowed value but token expects an owned value.
    match user_ref {
        &User { token } => println!("User token exists? {}", token.is_some())
    }

    // By adding 'ref' to our pattern we instruct the compiler to give us a reference
    // instead of an owned value.
    match user_ref {
        &User { ref token } => println!("User token exists? {}", token.is_some())
    }

    // We can also combine ref with destructuring
    match user_ref {
        // 'ref' will allow us to access the token inside of the Option by reference
        &User { token: Some(ref user_token) } => println!("Token value: {}", user_token.id ),
        &User { token: None } => println!("There was no token assigned to the user" )
    }

    // References can be mutable too, let's create another user to demonstrate this
    let mut other_user = User { token: Some(Token { id: 4 }) };

    // Take a mutable reference to the user
    let other_user_ref_mut = &mut other_user;

    match other_user_ref_mut {
        // 'ref mut' gets us a mutable reference allowing us to change the contained value directly.
        &mut User { token: Some(ref mut user_token) } => {
            user_token.id = 5;
            println!("New token value: {}", user_token.id )
        },
        &mut User { token: None } => println!("There was no token assigned to the user" )
    }
}
```
It will print this:
```
User token exists? true
Token value: 3
New token value: 5
```


