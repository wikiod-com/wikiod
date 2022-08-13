---
title: "Option"
slug: "option"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

The `Option<T>` type is Rust's equivalent of nullable types, without all the issues that come with it. The majority of C-like languages allow any variable to be `null` if there is no data present, but the `Option` type is inspired by functional languages which favour 'optionals' (e.g. Haskell's `Maybe` monad). Using `Option` types will allow you to express the idea that data may or may not be there (since Rust doesn't have nullable types).

## Destructuring an Option
    fn main() {
        let maybe_cake = Some("Chocolate cake");
        let not_cake = None;
    
        // The unwrap method retrieves the value from the Option
        // and panics if the value is None
        println!("{}", maybe_cake.unwrap());
    
        // The expect method works much like the unwrap method,
        // but panics with a custom, user provided message.
        println!("{}", not_cake.expect("The cake is a lie."));
    
        // The unwrap_or method can be used to provide a default value in case
        // the value contained within the option is None. This example would
        // print "Cheesecake".
        println!("{}", not_cake.unwrap_or("Cheesecake"));
    
        // The unwrap_or_else method works like the unwrap_or method,
        // but allows us to provide a function which will return the
        // fallback value. This example would print "Pumpkin Cake".
        println!("{}", not_cake.unwrap_or_else(|| { "Pumpkin Cake" }));
    
        // A match statement can be used to safely handle the possibility of none.
        match maybe_cake {
            Some(cake) => println!("{} was consumed.", cake),
            None       => println!("There was no cake.")
        }
    
        // The if let statement can also be used to destructure an Option.
        if let Some(cake) = maybe_cake {
            println!("{} was consumed.", cake);
        }
    }

## Creating an Option value and pattern match
    // The Option type can either contain Some value or None.
    fn find(value: i32, slice: &[i32]) -> Option<usize> {
        for (index, &element) in slice.iter().enumerate() {
            if element == value {
                // Return a value (wrapped in Some).
                return Some(index);
            }
        }
        // Return no value.
        None
    }

    fn main() {
        let array = [1, 2, 3, 4, 5];
        // Pattern match against the Option value.
        if let Some(index) = find(2, &array) {
            // Here, there is a value.
            println!("The element 2 is at index {}.", index);
        }

        // Check if the result is None (no value).
        if let None = find(12, &array) {
            // Here, there is no value.
            println!("The element 12 is not in the array.");
        }

        // You can also use `is_some` and `is_none` helpers
        if find(12, &array).is_none() {
            println!("The element 12 is not in the array.");
        }
    }

## Unwrapping a reference to an Option owning its contents
A reference to an option `&Option<T>` cannot be unwrapped if the type `T` is not copyable. The solution is to change the option to `&Option<&T>` using `as_ref()`.

Rust forbids transferring of ownership of objects while the objects are borrowed. When the Option itself is borrowed (`&Option<T>`), its contents is also — indirectly — borrowed. 

    #[derive(Debug)]
    struct Foo;
     
    fn main() {
        let wrapped = Some(Foo);
        let wrapped_ref = &wrapped;
        
        println!("{:?}", wrapped_ref.unwrap()); // Error!
    }

> cannot move out of borrowed content [--explain E0507]

However, it is possible to create a reference to the contents of `Option<T>`. Option's `as_ref()` method returns an option for `&T`, which can be unwrapped without transfer of ownership:

    println!("{:?}", wrapped_ref.as_ref().unwrap());




## Using Option with map and and_then
The `map` operation is a useful tool when working with arrays and vectors, but it can also be used to deal with `Option` values in a functional way. 

    fn main() {
    
        // We start with an Option value (Option<i32> in this case).
        let some_number = Some(9);
    
        // Let's do some consecutive calculations with our number.
        // The crucial point here is that we don't have to unwrap
        // the content of our Option type - instead, we're just
        // transforming its content. The result of the whole operation
        // will still be an Option<i32>. If the initial value of
        // 'some_number' was 'None' instead of 9, then the result
        //  would also be 'None'.
        let another_number = some_number
            .map(|n| n - 1) // => Some(8)
            .map(|n| n * n) // => Some(64)
            .and_then(|n| divide(n, 4)); // => Some(16)
    
        // In the last line above, we're doing a division using a helper
        // function (definition: see bottom).
        // 'and_then' is very similar to 'map', but allows us to pass a
        // function which returns an Option type itself. To ensure that we
        // don't end up with Option<Option<i32>>, 'and_then' flattens the
        // result (in other languages, 'and_then' is also known as 'flatmap').
    
        println!("{}", to_message(another_number));
        // => "16 is definitely a number!"
    
        // For the sake of completeness, let's check the result when
        // dividing by zero.
        let final_number = another_number
            .and_then(|n| divide(n, 0)); // => None
    
        println!("{}", to_message(final_number));
        // => "None!"
    }
    
    // Just a helper function for integer division. In case
    // the divisor is zero, we'll get 'None' as result.
    fn divide(number: i32, divisor: i32) -> Option<i32> {
        if divisor != 0  { Some(number/divisor) } else { None }
    }
    
    // Creates a message that tells us whether our
    // Option<i32> contains a number or not. There are other
    // ways to achieve the same result, but let's just use
    // map again!
    fn to_message(number: Option<i32>) -> String {
        number
            .map(|n| format!("{} is definitely a number!", n)) // => Some("...")
            .unwrap_or("None!".to_string()) // => "..."
    }



