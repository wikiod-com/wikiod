---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Anonymous functions
Since functions are ordinary values, there is a convenient syntax for creating functions without names:

    List.map (fun x -> x * x) [1; 2; 3; 4]
    (* - : int list = [1; 4; 9; 16] *)

This is handy, as we would otherwise have to name the function first (see [let][1]) to be able to use it:

    let square x = x * x
    (* val square : int -> int = <fun> *)

    List.map square [1; 2; 3; 4]
    (* - : int list = [1; 4; 9; 16] *)

  [1]: https://www.wikiod.com/ocaml/functions#Defining a Function with a let Binding

## Using the function keyword
The `function` keyword automatically has pattern matching when you define the
body of your function.  Observe it below:

```
# let foo = function
0 -> "zero"
| 1 -> "one"
| 2 -> "couple"
| 3 -> "few"
| _ -> "many";;
val foo : int -> bytes = <fun>

# foo 0;;
- : bytes = "zero"

# foo 3;;
- : bytes = "few"                                                                         

# foo 10;;
- : bytes = "many"                                                                        

# let bar = function
"a" | "i" | "e" | "o" | "u" -> "vowel"
| _ -> "consonant";;
val bar : bytes -> bytes = <fun>                                                          

# bar "a";;
- : bytes = "vowel"

# bar "k";;
- : bytes = "consonant" 
```



## Defining a Function with a let Binding
Values can be given names using `let`:

```
# let a = 1;;
val a : int = 1 
```

You can use similar syntax to define a function. Just provide additional parameters for the arguments.

```
# let add arg1 arg2 = arg1 + arg2;;
val add : int -> int -> int = <fun> 
```

We can call it like this:

```
# add 1 2;;
- : int = 3 
```

We can pass values in directly like that, or we can pass values bound to names:

```
# add a 2;;
- : int = 3 
```

The line that the interpreter gives us after we define something is the value of the object with its type signature.  When we gave it a simple value bound to `a`, it came back with:

```
val a : int = 1
```

Which means `a` is an `int`, and its value is `1`.

The type signature of our function is a little more complicated:

```
val add : int -> int -> int = <fun> 
```

The type signature of `add` looks like a bunch of ints and arrows.  This is because a function that takes two arguments is actually a function which just takes one argument, but returns another function that takes the next argument.  You could instead read it like this:

```
val add : int -> (int -> int) = <fun>
```

This is useful when we want to create different sorts of functions on the fly. For example, a function that adds 5 to everything:

```
# let add_five = add 5;;
val add_five : int -> int = <fun>
# add_five 5;;
- : int = 10
# add_five 10;;
- : int = 15 
```


## Recursive and Mutually Recursive Functions
You can define a function to be recursive with the `rec` keyword, so it can call itself.
    
    # let rec fact n = match n with
        | 0 -> 1
        | n -> n * fact (n - 1);;

    val fact : int -> int = <fun>

    # fact 0;;
    - : int = 1
    # fact 4;;
    - : int = 24

You can also define mutually recursive functions with the `and` keyword, so they can call each other.

    # let rec first x = match x with
        | 1 -> 1
        | x -> second (x mod 10)

      and second x = first (x + 1);;

    val first : int -> int = <fun>
    val second : int -> int = <fun>

    # first 20;;
    - : int = 1
    # first 12345;;
    - : int = 1

Notice that the second function does not have the `req` keyword.

