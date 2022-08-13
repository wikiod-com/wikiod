---
title: "Raw Pointers"
slug: "raw-pointers"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## Syntax
* let raw_ptr = &pointee as *const *type* // create constant raw pointer to some data
* let raw_mut_ptr = &mut pointee as *mut *type* // create mutable raw pointer to some mutable data
* let deref = *raw_ptr // dereference a raw pointer (requires unsafe block)


* Raw pointers are not guaranteed to point to a valid memory address and as such, careless usage may lead to unexpected (and probably fatal) errors.
* Any normal Rust reference (eg. `&my_object` where the type of `my_object` is T) will coerce to `*const T`. Similarly, mutable references coerce to `*mut T`. 
* Raw pointers do not move ownership (in contrast to Box values that) 

## Creating and using constant raw pointers

```
// Let's take an arbitrary piece of data, a 4-byte integer in this case
let some_data: u32 = 14;

// Create a constant raw pointer pointing to the data above
let data_ptr: *const u32 = &some_data as *const u32;

// Note: creating a raw pointer is totally safe but dereferencing a raw pointer requires an
// unsafe block
unsafe {
    let deref_data: u32 = *data_ptr;
    println!("Dereferenced data: {}", deref_data);
}
```
The above code will output:
`Dereferenced data: 14`

## Creating and using mutable raw pointers
```
// Let's take a mutable piece of data, a 4-byte integer in this case
let mut some_data: u32 = 14;

// Create a mutable raw pointer pointing to the data above
let data_ptr: *mut u32 = &mut some_data as *mut u32;

// Note: creating a raw pointer is totally safe but dereferencing a raw pointer requires an
// unsafe block
unsafe {
    *data_ptr = 20;
    println!("Dereferenced data: {}", some_data);
}
```
The above code will output: `Dereferenced data: 20`

## Initialising a raw pointer to null
Unlike normal Rust references, raw pointers are allowed to take null values.
```
use std::ptr;

// Create a const NULL pointer
let null_ptr: *const u16 = ptr::null();

// Create a mutable NULL pointer
let mut_null_ptr: *mut u16 = ptr::null_mut();
```

## Chain-dereferencing
Just like in C, Rust raw pointers can point to other raw pointers (which in turn may point to further raw pointers). 
```
// Take a regular string slice
let planet: &str = "Earth";

// Create a constant pointer pointing to our string slice
let planet_ptr: *const &str = &planet as *const &str;

// Create a constant pointer pointing to the pointer
let planet_ptr_ptr: *const *const &str = &planet_ptr as *const *const &str;

// This can go on...
let planet_ptr_ptr_ptr = &planet_ptr_ptr as *const *const *const &str;

unsafe {
    // Direct usage
    println!("The name of our planet is: {}", planet);
    // Single dereference
    println!("The name of our planet is: {}", *planet_ptr);
    // Double dereference
    println!("The name of our planet is: {}", **planet_ptr_ptr);
    // Triple dereference
    println!("The name of our planet is: {}", ***planet_ptr_ptr_ptr);
}
```
This will output: `The name of our planet is: Earth` four times.


## Displaying raw pointers
Rust has a default formatter for pointer types that can be used for displaying pointers.

```
use std::ptr;

// Create some data, a raw pointer pointing to it and a null pointer
let data: u32 = 42;
let raw_ptr = &data as *const u32;
let null_ptr = ptr::null() as *const u32;

// the {:p} mapping shows pointer values as hexadecimal memory addresses
println!("Data address: {:p}", &data);
println!("Raw pointer address: {:p}", raw_ptr); 
println!("Null pointer address: {:p}", null_ptr);
```
This will output something like this:
```
Data address: 0x7fff59f6bcc0
Raw pointer address: 0x7fff59f6bcc0
Null pointer address: 0x0
```

