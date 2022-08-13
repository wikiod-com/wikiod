---
title: "Record Syntax"
slug: "record-syntax"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Basic Syntax
Records are an extension of sum algebraic `data` type that allow fields to be named:

    data StandardType = StandardType String Int Bool --standard way to create a sum type
    
    data RecordType = RecordType { -- the same sum type with record syntax
        aString :: String
      , aNumber :: Int
      , isTrue  :: Bool
      }

The field names can then be used to get the named field out of the record

    > let r = RecordType {aString = "Foobar", aNumber= 42, isTrue = True}
    > :t r
      r :: RecordType
    > :t aString
      aString :: RecordType -> String
    > aString r
      "Foobar"

Records can be pattern matched against

    case r of
      RecordType{aNumber = x, aString=str} -> ... -- x = 42, str = "Foobar"

Notice that not all fields need be named

Records are created by naming their fields, but can also be created as ordinary sum types (often useful when the number of fields is small and not likely to change)

    r  = RecordType {aString = "Foobar", aNumber= 42, isTrue = True}
    r' = RecordType  "Foobar" 42 True

If a record is created without a named field, the compiler will issue a warning, and the resulting value will be `undefined`.

    > let r = RecordType {aString = "Foobar", aNumber= 42}
      <interactive>:1:9: Warning:
         Fields of RecordType not initialized: isTrue
    > isTrue r
      Error 'undefined'

A field of a record can be updated by setting its value. Unmentioned fields do not change.

    > let r = RecordType {aString = "Foobar", aNumber= 42, isTrue = True}
    > let r' = r{aNumber=117}
        -- r'{aString = "Foobar", aNumber= 117, isTrue = True}

It is often useful to create [lenses][1] for complicated record types.

[1]:  https://www.wikiod.com/haskell/lens#Lenses for records

## Defining a data type with field labels


## Copying Records while Changing Field Values
Suppose you have this type:

    data Person = Person { name :: String, age:: Int } deriving (Show, Eq)

and two values:

    alex = Person { name = "Alex", age = 21 }
    jenny = Person { name = "Jenny", age = 36 }

a new value of type `Person` can be created by copying from `alex`, specifying which values to change:

    anotherAlex = alex { age = 31 }

The values of `alex` and `anotherAlex` will now be:

    Person {name = "Alex", age = 21}

    Person {name = "Alex", age = 31}


## Records with newtype
Record syntax can be used with `newtype` with the restriction that there is exactly one constructor with exactly one field.  The benefit here is the automatic creation of a function to unwrap the newtype.  These fields are often named starting with `run` for monads, `get` for monoids, and `un` for other types.

    newtype State s a = State { runState :: s -> (s, a) }

    newtype Product a = Product { getProduct :: a }

    newtype Fancy = Fancy { unfancy :: String } 
      -- a fancy string that wants to avoid concatenation with ordinary strings

It is important to note that the record syntax is typically never used to form values and the field name is used strictly for unwrapping

    getProduct $ mconcat [Product 7, Product 9, Product 12]
    -- > 756

## RecordWildCards
    {-# LANGUAGE RecordWildCards #-}

    data Client = Client { firstName     :: String
                         , lastName      :: String
                         , clientID      :: String 
                         } deriving (Show)

    printClientName :: Client -> IO ()
    printClientName Client{..} = do
        putStrLn firstName
        putStrLn lastName
        putStrLn clientID

The pattern `Client{..}` brings in scope all the fields of the constructor `Client`, and is equivalent to the pattern

    Client{ firstName = firstName, lastName = lastName, clientID = clientID }

It can also be combined with other field matchers like so:

    Client { firstName = "Joe", .. }

This is equivalent to 

    Client{ firstName = "Joe", lastName = lastName, clientID = clientID }


