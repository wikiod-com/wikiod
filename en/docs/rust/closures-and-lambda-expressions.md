---
title: "Closures and lambda expressions"
slug: "closures-and-lambda-expressions"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Returning lambdas from functions
Returning lambdas (or closures) from functions can be tricky because they implement traits and thus their exact size is rarely known. 

```
// Box in the return type moves the function from the stack to the heap
fn curried_adder(a: i32) -> Box<Fn(i32) -> i32> {
    // 'move' applies move semantics to a, so it can outlive this function call
    Box::new(move |b| a + b)
}

println!("3 + 4 = {}", curried_adder(3)(4));
```
This displays: `3 + 4 = 7`

## Simple lambda expressions
```
// A simple adder function defined as a lambda expression. 
// Unlike with regular functions, parameter types often may be omitted because the
// compiler can infer their types
let adder = |a, b| a + b;
// Lambdas can span across multiple lines, like normal functions.
let multiplier = |a: i32, b: i32| {
    let c = b;
    let b = a;
    let a = c;
    a * b
};

// Since lambdas are anonymous functions, they can be called like other functions
println!("{}", adder(3, 5));
println!("{}", multiplier(3, 5));
```
This displays:
```
8
15
```

## Simple closures
Unlike regular functions, lambda expressions can capture their environments. Such lambdas are called closures.

```

// variable definition outside the lambda expression...
let lucky_number: usize = 663;

// but the our function can access it anyway, thanks to the closures
let print_lucky_number = || println!("{}", lucky_number);

// finally call the closure
print_lucky_number();
```
This will print:
```
663
```

## Lambdas with explicit return types
```
// lambda expressions can have explicitly annotated return types
let floor_func = |x: f64| -> i64 { x.floor() as i64 };
```

## Passing lambdas around
Since lambda functions are values themselves, you store them in collections, pass them to functions, etc like you would with other values.

```
// This function takes two integers and a function that performs some operation on the two arguments
fn apply_function<T>(a: i32, b: i32, func: T) -> i32 where T: Fn(i32, i32) -> i32 {
    // apply the passed function to arguments a and b
    func(a, b)
}

// let's define three lambdas, each operating on the same parameters
let sum = |a, b| a + b;
let product = |a, b| a * b;
let diff = |a, b| a - b;

// And now let's pass them to apply_function along with some arbitary values
println!("3 + 6 = {}", apply_function(3, 6, sum));
println!("-4 * 9 = {}", apply_function(-4, 9, product));
println!("7 - (-3) = {}", apply_function(7, -3, diff));

```
This will print:
```
3 + 6 = 9
-4 * 9 = -36
7 - (-3) = 10
```

