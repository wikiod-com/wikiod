---
title: "Collecting Data Tuples, Records and Dictionaries"
slug: "collecting-data-tuples-records-and-dictionaries"
draft: false
images: []
weight: 9926
type: docs
toc: true
---

## Dictionaries
Dictionaries are implemented in a [Dict][1] core library.

> A dictionary mapping unique keys to values. The keys can be any
> comparable type. This includes Int, Float, Time, Char, String, and
> tuples or lists of comparable types.
> 
> Insert, remove, and query operations all take O(log n) time.

Unlike Tuples and Records, Dictionaries can change their structure, in other words it is possible to add and remove keys.

It is possible to update a value by a key.

It is also possible to access or update a value using dynamic keys.

# Accessing values #
You can retrieve a value from a Dictionary by using a `Dict.get` function.

Type definition of `Dict.get`:

    get : comparable -> Dict comparable v -> Maybe v

It will always return `Maybe v`, because it is possible to try to get a value by an non-existent key.

    import Dict
    
    initialUsers =
          Dict.fromList [ (1, "John"), (2, "Brad") ]
    
    getUserName id =
      initialUsers
      |> Dict.get id
      |> Maybe.withDefault "Anonymous"
      
    getUserName 2 -- "Brad"
    getUserName 0 -- "Anonymous"

# Updating values #
Update operation on a Dictionary is performed by using `Maybe.map`, since the requested key might be absent.

    import Dict
    
    initialUsers =
      Dict.fromList [ (1, "John"), (2, "Brad") ]
    
    updatedUsers =
      Dict.update 1 (Maybe.map (\name -> name ++ " Johnson")) initialUsers
    
    Maybe.withDefault "No user" (Dict.get 1 updatedUsers) -- "John Johnson"

  [1]: http://package.elm-lang.org/packages/elm-lang/core/4.0.3/Dict

## Records
Record is a set of key-value pairs.

    greeter =
        { isMorning: True
        , greeting: "Good morning!"
        }

It is impossible to access a value by an non-existent key.

It is impossible to dynamically modify Record's structure.

Records only allow you to update values by constant keys.

# Accessing values #

Values can not be accessed using a dynamic key to prevent possible run-time errors:

    isMorningKeyName =
        "isMorning "


    greeter[isMorningKeyName] -- Compiler error
    greeter.isMorning -- True

The alternative syntax for accessing the value allows you to extract the value, while piping through the Record:

    greeter
        |> .greeting
        |> (++) " Have a nice day!" -- "Good morning! Have a nice day!"

# Extending Types

Sometimes you'd want the signature of a parameter to constrain the record types you pass into functions. Extending record types makes the idea of supertypes unnecessary. The following example shows how this concept can be implemented:

    type alias Person =
        { name : String
        }
    
    
    type alias Animal =
        { name : String
        }
    
    
    peter : Person
    peter =
        { name = "Peter" }
    
    
    dog : Animal
    dog =
        { name = "Dog" }
    
    
    getName : { a | name : String } -> String
    getName livingThing =
        livingThing.name
    
    
    bothNames : String
    bothNames =
        getName peter ++ " " ++ getName dog

We could even take extending records a step further and do something like:

    type alias Named a = { a | name : String }
    type alias Totalled a = { a | total : Int }
    
    
    totallyNamed : Named ( Totalled { age : Int })
    totallyNamed =
      { name = "Peter Pan"
      , total = 1337
      , age = 14
      }

We now have ways to pass these partial types around in functions:

    changeName : Named a -> String -> Named a
    changeName a newName =
      { a | name = newName }
      
    cptHook = changeName totallyNamed "Cpt. Hook" |> Debug.log "who?"



# Updating values #

Elm has a special syntax for Record updates:

    model =
        { id: 1
        , score: 0
        , name: "John Doe"
        }
    

    update model =
        { model
           | score = model.score + 100
           | name = "John Johnson"
        }


## Tuples
Tuples are ordered lists of values of any type.

    (True, "Hello!", 42)

It is impossible to change the structure of a Tuple or update the value.

Tuples in Elm are considered a primitive data type, which means that you don't need to import any modules to use Tuples.

# Accessing values #
[Basics][1] module has two helper functions for accessing values of a Tuple with a length of two `( a, b )` without using pattern matching:

    fst (True, "Hello!") -- True
    snd (True, "Hello!") -- "Hello!"

Access values of tuples with a bigger length is done through pattern matching.

# Pattern matching #
Tuples are extremely useful in combination with pattern matching:
    
    toggleFlag: (Sting, Bool) -> (Sting, Bool)
    toggleFlag (name, flag) =
        (name, not flag)

# Remarks on Tuples #
Tuples contain less than 7 values of `comparable` data type


  [1]: http://package.elm-lang.org/packages/elm-lang/core/4.0.3/Basics

