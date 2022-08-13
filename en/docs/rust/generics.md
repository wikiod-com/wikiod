---
title: "Generics"
slug: "generics"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Declaration
```
// Generic types are declared using the <T> annotation

struct GenericType<T> {
    pub item: T
}

enum QualityChecked<T> {
    Excellent(T),
    Good(T),
    // enum fields can be generics too
    Mediocre { product: T }
}

## Instantiation
```
// explicit type declaration
let some_value: Option<u32> = Some(13);

// implicit type declaration
let some_other_value = Some(66);

## Multiple type parameters
Generics types can have more than one type parameters, eg. `Result` is defined like this:
```
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}
```


## Bounded generic types
```
// Only accept T and U generic types that also implement Debug
fn print_objects<T: Debug, U: Debug>(a: T, b: U) {
    println!("A: {:?} B: {:?}", a, b);
}

print_objects(13, 44);
// or annotated explicitly
print_objects::<usize, u16>(13, 44);

```

The bounds must cover all uses of the type. Addition is done by the `std::ops::Add` trait, which has input and output parameters itself. `where T: std::ops::Add<u32,Output=U>` states that it's possible to `Add` `T` to `u32`, and this addition has to produce type `U`. 

```
fn try_add_one<T, U>(input_value: T) -> Result<U, String> 
    where T: std::ops::Add<u32,Output=U> 
{
    return Ok(input_value + 1);
}
```

`Sized` bound is implied by default. `?Sized` bound allows unsized types as well.


## Generic functions
Generic functions allow some or all of their arguments to be parameterised. 
```
fn convert_values<T, U>(input_value: T) -> Result<U, String> {
  // Try and convert the value.
  // Actual code will require bounds on the types T, U to be able to do something with them.
}
```
If the compiler can't infer the type parameter then it can be supplied manually upon call:
```
let result: Result<u32, String> = convert_value::<f64, u32>(13.5);

