---
title: "Creating Custom Data Types"
slug: "creating-custom-data-types"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Creating a data type with value constructor parameters
Value constructors are functions that return a value of a data type. Because of this, just like any other function, they can take one or more parameters:

    data Foo = Bar String Int | Biz String

Let's check the type of the `Bar` value constructor.

    :t Bar

prints

    Bar :: String -> Int -> Foo

which proves that `Bar` is indeed a function.

Creating variables of our custom type
-

    let x = Bar "Hello" 10
    let y = Biz "Goodbye"

## Creating a data type with type parameters
Type constructors can take one or more type parameters:

    data Foo a b = Bar a b | Biz a b

Type parameters in Haskell must begin with a lowercase letter. Our custom data type is not a real type yet. In order to create values of our type, we must substitute all type parameters with actual types. Because `a` and `b` can be of any type, our value constructors are polymorphic functions.

Creating variables of our custom type
-

    let x = Bar "Hello" 10      -- x :: Foo [Char] Integer
    let y = Biz "Goodbye" 6.0   -- y :: Fractional b => Foo [Char] b
    let z = Biz True False      -- z :: Foo Bool Bool

## Creating a simple data type
The easiest way to create a custom data type in Haskell is to use the `data` keyword:

    data Foo = Bar | Biz

The name of the type is specified between `data` and `=`, and is called a **type constructor**. After `=` we specify all **value constructors** of our data type, delimited by the `|` sign. There is a rule in Haskell that all type and value constructors must begin with a capital letter. The above declaration can be read as follows:

> Define a type called `Foo`, which has two possible values: `Bar` and `Biz`.

Creating variables of our custom type
-

    let x = Bar

The above statement creates a variable named `x` of type `Foo`. Let's verify this by checking its type.

    :t x

prints

    x :: Foo

## Custom data type with record parameters
Assume we want to create a data type Person, which has a first and last name, an age, a phone number, a street, a zip code and a town.

We could write 

    data Person = Person String String Int Int String String String

If we want now to get the phone number, we need to make a function

    getPhone :: Person -> Int
    getPhone (Person _ _ _ phone _ _ _) = phone

Well, this is no fun.
We can do better using parameters:
    
    data Person' = Person' { firstName     :: String
                           , lastName      :: String
                           , age           :: Int
                           , phone         :: Int
                           , street        :: String
                           , code          :: String
                           , town          :: String }

Now we get the function `phone` where

    :t phone
    phone :: Person' -> Int

We can now do whatever we want, eg:

    printPhone :: Person' -> IO ()
    printPhone = putStrLn . show . phone

We can also bind the phone number by [Pattern Matching][1]:
    
    getPhone' :: Person' -> Int
    getPhone' (Person {phone = p}) = p

For easy use of the parameters see [RecordWildCards][2]


  [1]: https://www.wikiod.com/haskell/syntax-in-functions#Pattern Matching
  [2]: https://www.wikiod.com/haskell/common-ghc-language-extensions#RecordWildCards

