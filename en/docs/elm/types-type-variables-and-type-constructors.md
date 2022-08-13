---
title: "Types, Type Variables, and Type Constructors"
slug: "types-type-variables-and-type-constructors"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

Please play with these concepts yourself to really master them! The `elm-repl` (see the [Introduction to the REPL][1]) is probably a good place to play around with the code above. You can also play with [`elm-repl` online][2].

[1]: https://www.wikiod.com/elm/getting-started-with-elm-language#REPL

[2]: http://elmrepl.cuberoot.in/

## Improving Type-Safety Using New Types
Aliasing types cuts down on boilerplate and enhances readability, but it is no more type-safe than the aliased type itself is. Consider the following:

<!-- language: lang-elm -->
    type alias Email = String

    type alias Name = String

    someEmail = "holmes@private.com"

    someName = "Benedict"

    sendEmail : Email -> Cmd msg
    sendEmail email = ...

Using the above code, we can write `sendEmail someName`, and it will compile, even though it really shouldn't, because despite names and emails both being `String`s, they are entirely different things.

We can truly distinguish one `String` from another `String` on the type-level by creating a new **type**. Here's an example that rewrites `Email` as a `type` rather than a `type alias`:

<!-- language: lang-elm -->
    module Email exposing (Email, create, send)

    type Email = EmailAddress String

    isValid : String -> Bool
    isValid email = 
      -- ...validation logic

    create : String -> Maybe Email
    create email =
        if isValid email then
            Just (EmailAddress email)
        else
            Nothing

    send : Email -> Cmd msg
    send (EmailAddress email) = ...
 
Our `isValid` function does something to determine if a string is a valid email address. The `create` function checks if a given `String` is a valid email, returning a `Maybe`-wrapped `Email` to ensure that we only return validated addresses. While we can sidestep the validation check by constructing an `Email` directly by writing `EmailAddress "somestring"`, if our module declaration doesn't expose the `EmailAddress` constructor, as show here

<!-- language: lang-elm -->
    module Email exposing (Email, create, send)

then no other module will have access to the `EmailAddress` constructor, though they can still use the `Email` type in annotations. The **only** way to build a new `Email` outside of this module is by using the `create` function it provides, and that function ensures that it will only return valid email addresses in the first place. Hence, this API automatically guides the user down the correct path via its type safety: `send` only works with values constructed by `create`, which performs a validation, and enforces handling of invalid emails since it returns a `Maybe Email`.

If you'd like to export the `Email` constructor, you could write
<!-- language: lang-elm -->
    module Email exposing (Email(EmailAddress), create, send)

Now any file that imports `Email` can also import its constructor. In this case, doing so would allow users to sidestep validation and `send` invalid emails, but you're not always building an API like this, so exporting constructors can be useful. With a type that has several constructors, you may also only want to export some of them.

## Comparable data types
Comparable types are primitive types that can be compared using comparison operators from [Basics][1] module, like: `(<)`, `(>)`, `(<=)`, `(>=)`, `max`, `min`, `compare`

Comparable types in Elm are `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists of comparable types.

In documentation or type definitions they are referred as a special type variable `comparable`, eg. see type definition for `Basics.max` function:

    max : comparable -> comparable -> comparable


  [1]: http://package.elm-lang.org/packages/elm-lang/core/4.0.3/Basics

## Constructing Types
The `type alias` keyword combination gives a new name for a type, but the `type` keyword in isolation declares a new type. Let's examine one of the most fundamental of these types: [`Maybe`](https://github.com/elm-lang/core/blob/4.0.3/src/Maybe.elm)

<!-- language: lang-hs -->
    type Maybe a
        = Just a
        | Nothing

The first thing to note is that the `Maybe` type is declared with a [**type variable**](https://www.wikiod.com/elm/types-type-variables-and-type-constructors#Type Variables) of `a`. The second thing to note is the pipe character, `|`, which signifies "or". In other words, something of type `Maybe a` is *either* `Just a` *or* `Nothing`.

When you write the above code, `Just` and `Nothing` come into scope as *value-constructors*, and `Maybe` comes into scope as a *type-constructor*. These are their signatures:

<!-- language: lang-hs -->
    Just : a -> Maybe a
    
    Nothing : Maybe a

    Maybe : a -> Maybe a -- this can only be used in type signatures

Because of the *type variable* `a`, any type can be "wrapped inside" of the `Maybe` type. So, `Maybe Int`, `Maybe (List String)`, or `Maybe (Maybe (List Html))`, are all valid types.
When destructuring any `type` value with a `case` expression, you must account for each possible instantiation of that type. In the case of a value of type `Maybe a`, you have to account for both the `Just a` case, and the `Nothing` case:

<!-- language: lang-hs -->
    thing : Maybe Int
    thing = 
        Just 3

    blah : Int
    blah =
        case thing of
            Just n -> 
                n

            Nothing ->
                42

    -- blah = 3
            
Try writing the above code without the `Nothing` clause in the `case` expression: it won't compile. This is what makes the `Maybe` type-constructor a great pattern for expressing values that may not exist, as it forces you to handle the logic of when the value is `Nothing`.

## The Never type
The `Never` type cannot be constructed (the `Basics` module hasn't exported its **value constructor** and hasn't given you any other function that returns `Never` either). There is no value `never : Never` or a function `createNever : ?? -> Never`.

This has its benefits: you can encode in a type system a possibility that can't happen. This can be seen in types like `Task Never Int` which guarantees it will succeed with an `Int`; or `Program Never` that will not take any parameters when initializing the Elm code from JavaScript.

## Type Signatures
In Elm, values are declared by writing a name, an equals sign, and then the actual value:

```
someValue = 42
```

Functions are also values, with the addition of taking a value or values as arguments. They are usually written as follows:

```
double n = n * 2
```

Every value in Elm has a type. The types of the values above will be *inferred* by the compiler depending on how they are used. But it is best-practice to always explicitly declare the type of any top-level value, and to do so you write a *type signature* as follows:

```
someValue : Int
someValue = 
    42

someOtherValue : Float
someOtherValue =
    42
```
As we can see, `42` can be defined as *either* an `Int` or a `Float`. This makes intuitive sense, but see **Type Variables** for more information.

Type signatures are particularly valuable when used with functions. Here's the doubling function from before:
```
double : Int -> Int
double n =
    n * 2
```
This time, the signature has a `->`, an arrow, and we'd pronounce the signature as "int to int", or "takes an integer and returns an integer". `->` indicates that by giving `double` an `Int` value as an argument, `double` will return an `Int`. Hence, it takes an integer to an integer:

```
> double
<function> : Int -> Int

> double 3
6 : Int
```



## Basic Types
In `elm-repl`, type a piece of code to get its value and inferred type. Try the following to learn about the various types that exist:

```
> 42
42 : number

> 1.987
1.987 : Float

> 42 / 2
21 : Float

> 42 % 2
0 : Int

> 'e'
'e' : Char

> "e"
"e" : String

> "Hello Friend"
"Hello Friend" : String

> ['w', 'o', 'a', 'h']
['w', 'o', 'a', 'h'] : List Char

> ("hey", 42.42, ['n', 'o'])
("hey", 42.42, ['n', 'o']) : ( String, Float, List Char )

> (1, 2.1, 3, 4.3, 'c')
(1,2.1,3,4.3,'c') : ( number, Float, number', Float, Char )

> {}
{} : {}

> { hey = "Hi", someNumber = 43 }
{ hey = "Hi", someNumber = 43 } : { hey : String, someNumber : number }

> ()
() : ()
```

`{}` is the empty Record type, and `()` is the empty Tuple type. The latter is often used for the purposes of lazy evaluation. See the corresponding example in https://www.wikiod.com/elm/functions-and-partial-application

Note how `number` appears uncapitalized. This indicates that it is a **Type Variable**, and moreover the particular word `number` refers to a **Special Type Variable** that can either be an `Int` or a `Float` (see the corresponding sections for more). Types though are always upper-case, such as `Char`, `Float`, `List String`, et cetera.

## Special Type Variables
Elm defines the following special type variables that have a particular meaning to the compiler:

- **`comparable`**: Comprised of `Int`, `Float`, `Char`, `String` and tuples thereof. This allows the use of the `<` and `>` operators.

  *Example:* You could define a function to find the smallest and largest elements in a list (`extent`). You think what type signature to write. On one hand, you could write `extentInt : List Int -> Maybe (Int, Int)` and `extentChar : List Char -> Maybe (Char, Char)` and another for `Float` and `String`. The implementation of these would be the same:

      extentInt list =
        let
          helper x (minimum, maximum) = 
            ((min minimum x), (max maximum x))
        in 
          case list of 
            [] ->
              Nothing
            x :: xs ->
              Just <| List.foldr helper (x, x) xs

     You might be tempted to simply write `extent : List a -> Maybe (a, a)`, but the compiler will not let you do this, because the functions `min` and `max` are not defined for these types (NB: these are just simple wrappers around the `<` operator mentioned above). You can solve this by defining `extent : List comparable -> Maybe (comparable, comparable)`. This allows your solution to be *polymorphic*, which just means that it will work for more than one type.

- **`number`**: Comprised of `Int` and `Float`. Allows the use of arithmetic operators except division. You can then define for example `sum : List number -> number` and have it work for both ints and floats.
- **`appendable`**: Comprised of `String`, `List`. Allows the use of the `++` operator.
- **`compappend`**: This sometimes appears, but is an implementation detail of the compiler. Currently this can't be used in your own programs, but is sometimes mentioned.

Note that in a type annotation like this: `number -> number -> number` these all refer to the same type, so passing in `Int -> Float -> Int` would be a type error. You can solve this by adding a suffix to the type variable name: `number -> number' -> number''` would then compile fine.

There is no official name for these, they are sometimes called:

- Special Type Variables
- Typeclass-like Type Variables
- Pseudo-typeclasses

This is because they work like Haskell's [Type Classes](https://www.wikiod.com/haskell/type-classes), but without the ability for the user to define these.

## Type Variables
Type variables are uncapitalized names in type-signatures. Unlike their capitalized counterparts, such as `Int` and `String`, they do not represent a single type, but rather, any type. They are used to write generic functions that can operate on *any* type or types, and are particularly useful for writing operations over containers like `List` or `Dict`. The `List.reverse` function, for example, has the following signature:

```
reverse : List a -> List a
```

Which means it can work on a list of *any type value*, so `List Int`, `List (List String)`, both of those and any others can be `reversed` all the same. Hence, `a` is a type variable that can stand in for any type.

The `reverse` function could have used *any* uncapitalized variable name in its type signature, except for the handful of **special type variable** names, such as `number` (see the corresponding example on that for more information):

```
reverse : List lol -> List lol

reverse : List wakaFlaka -> List wakaFlaka
```

The names of type variables become meaningful only when there when there are *different* type variables within a single signature, exemplified by the `map` function on lists:

```
map : (a -> b) -> List a -> List b
```

`map` takes some function from any type `a` to any type `b`, along with a list with elements of some type `a`, and returns a list of elements of some type `b`, which it gets by applying the given function to every element of the list.

Let's make the signature concrete to better see this:

```
plusOne : Int -> Int
plusOne x = 
    x + 1

> List.map plusOne
<function> : List Int -> List Int
```

As we can see, both `a = Int` and `b = Int` in this case. But, if `map` had a type signature like `map : (a -> a) -> List a -> List a`, then it would *only* work on functions that operate on a single type, and you'd never be able to change the type of a list by using the `map` function. But since the type signature of `map` has multiple different type variables, `a` and `b`, we can use `map` to change the type of a list:

```
isOdd : Int -> Bool
isOdd x =
    x % 2 /= 0

> List.map isOdd
<function> : List Int -> List Bool
```

In this case, `a = Int` and `b = Bool`. Hence, to be able to use functions that can take and return *different* types, you must use different type variables.

## Type Aliases
Sometimes we want to give a type a more descriptive name. Let's say our app has a data type representing users:

<!-- language: lang-hs -->
    { name : String, age : Int, email : String }

And our functions on users have type signatures along the lines of:

<!-- language: lang-hs -->
    prettyPrintUser : { name : String, age : Int, email : String } -> String

This could become quite unwieldy with a larger record type for a user, so let's use a *type alias* to cut down on the size and give a more meaningful name to that data structure:

<!-- language: lang-hs -->
    type alias User =
        { name: String
        , age : Int
        , email : String
        }
    

    prettyPrintUser : User -> String

Type aliases make it much cleaner to define and use a model for an application:

<!-- language: lang-hs -->
    type alias Model =
        { count : Int
        , lastEditMade : Time
        }

Using `type alias` literally just aliases a type with the name you give it. Using the `Model` type above is exactly the same as using `{ count : Int, lastEditMade : Time }`. Here's an example showing how aliases are no different than the underlying types:

<!-- language: lang-hs -->
    type alias Bugatti = Int

    type alias Fugazi = Int

    unstoppableForceImmovableObject : Bugatti -> Fugazi -> Int
    unstoppableForceImmovableObject bug fug =
        bug + fug

    > unstoppableForceImmovableObject 09 87
    96 : Int

A type alias for a record type defines a constructor function with one argument for each field in declaration order.

    type alias Point = { x : Int, y : Int }

    Point 3 7
    { x = 3, y = 7 } : Point

    type alias Person = { last : String, middle : String, first : String }

    Person "McNameface" "M" "Namey"
    { last = "McNameface", middle = "M", first = "Namey" } : Person

Each record type alias has its own field order even for a compatible type.

    type alias Person = { last : String, middle : String, first : String }
    type alias Person2 = { first : String, last : String, middle : String }

    Person2 "Theodore" "Roosevelt" "-"
    { first = "Theodore", last = "Roosevelt", middle = "-" } : Person2

    a = [ Person "Last" "Middle" "First", Person2 "First" "Last" "Middle" ]
    [{ last = "Last", middle = "Middle", first = "First" },{ first = "First", last = "Last", middle = "Middle" }] : List Person2


    

