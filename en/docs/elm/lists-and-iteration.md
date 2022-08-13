---
title: "Lists and Iteration"
slug: "lists-and-iteration"
draft: false
images: []
weight: 9701
type: docs
toc: true
---

The `List` ([linked list](https://en.wikipedia.org/wiki/Linked_list)) shines in **sequential access**:

 - accessing the first element
 - prepending to the front of the list
 - deleting from the front of the list

On the other hand, it's not ideal for **random access** (ie. getting nth element) and **traversation in reverse order**, and you might have better luck (and performance) with the `Array` data structure.

## Creating a list by range
<!-- if version [lt 0.18.0] -->
Prior to **0.18.0** you can create ranges like this:

    > range = [1..5]
    [1,2,3,4,5] : List number
    >
    > negative = [-5..3]
    [-5,-4,-3,-2,-1,0,1,2,3] : List number
<!-- end version if -->
<!-- if version [gte 0.18.0] -->
In **0.18.0** The `[1..5]` syntax [has been removed][1].

    > range = List.range 1 5
    [1,2,3,4,5] : List number
    >
    > negative = List.range -5 3
    [-5,-4,-3,-2,-1,0,1,2,3] : List number
<!-- end version if -->

Ranges created by this syntax are always **inclusive** and the **step** is always **1**.


  [1]: https://github.com/elm-lang/elm-platform/blob/master/upgrade-docs/0.18.md#list-ranges "Upgrading to 0.18"

## Getting nth element from the list
`List` doesn't support "random access", which means it takes more work to get, say, the fifth element from the list than the first element, and as a result there's no `List.get nth list` function. One has to go all the way from the beginning (`1 -> 2 -> 3 -> 4 -> 5`).

**If you need random access,** you might get better results (and performance) with random access data structures, like `Array`, where taking the first element takes the same amount of work as taking, say, the 1000th. (complexity O(1)).

Nevertheless, it's possible **(but discouraged)** to get nth element:

<!-- language: lang-elm -->

    get : Int -> List a -> Maybe a
    get nth list =
        list
            |> List.drop (nth - 1)
            |> List.head

    fifth : Maybe Int
    fifth = get 5 [1..10]
    --    = Just 5

    nonexistent : Maybe Int
    nonexistent = get 5 [1..3]
    --          = Nothing

Again, this takes significantly more work the bigger the `nth` argument is.

## Creating a list

    > listOfNumbers = [1,4,99]
    [1,4,99] : List number
    >
    > listOfStrings = ["Hello","World"]
    ["Hello","World"] : List String
    >
    > emptyList = []   -- can be anything, we don't know yet
    [] : List a
    >

Under the hood, `List` ([linked list](https://en.wikipedia.org/wiki/Linked_list)) is constructed by the `::` function (called "cons"), which takes two arguments: an element, known as the head, and a (possibly empty) list the head is prepended to.

    > withoutSyntaxSugar = 1 :: []
    [1] : List number
    >
    > longerOne = 1 :: 2 :: 3 :: []
    [1,2,3] : List number
    >
    > nonemptyTail = 1 :: [2]
    [1,2] : List number
    >

`List` can only take values of one type, so something like `[1,"abc"]` is not possible. If you need this, use tuples.

    > notAllowed = [1,"abc"]
    ==================================== ERRORS ====================================

    -- TYPE MISMATCH --------------------------------------------- repl-temp-000.elm

    The 1st and 2nd elements are different types of values.

    8|              [1,"abc"]
                   ^^^^^
    The 1st element has this type:

        number

    But the 2nd is:

        String

    Hint: All elements should be the same type of value so that we can iterate
    through the list without running into unexpected values.


    > 

## Reducing a list to a single value
In Elm, reducing functions are called "folds", and there are two standard methods to "fold" values up: from the left, `foldl`, and from the right, `foldr`.

<!-- language: lang-hs -->

    > List.foldl (+) 0 [1,2,3]
    6 : number

The arguments to `foldl` and `foldr` are:

 - **reducing function**: `newValue -> accumulator -> accumulator`
 - **accumulator** starting value
 - **list** to reduce

One more example with custom function:

<!-- language: lang-elm -->

    type alias Counts =
        { odd : Int
        , even : Int
        }

    addCount : Int -> Counts -> Counts
    addCount num counts =
        let
            (incOdd, incEven) =
                if num `rem` 2 == 0
                    then (0,1)
                    else (1,0)
        in
            { counts
                | odd = counts.odd + incOdd
                , even = counts.even + incEven
            }

    > List.foldl
          addCount
          { odd = 0, even = 0 }
          [1,2,3,4,5]
    { odd = 3, even = 2 } : Counts

In the first example above the program goes like this:

<!-- language: lang-hs -->

    List.foldl (+) 0 [1,2,3]
    3 + (2 + (1 + 0))
    3 + (2 + 1)
    3 + 3
    6

<!-- language: lang-hs -->

    List.foldr (+) 0 [1,2,3]
    1 + (2 + (3 + 0))
    1 + (2 + 3)
    1 + 5
    6

In the case of a [commutative](https://en.wikipedia.org/wiki/Commutative_property) function like `(+)` there's not really a difference.

But see what happens with `(::)`:

<!-- language: lang-hs -->

    List.foldl (::) [] [1,2,3]
    3 :: (2 :: (1 :: []))
    3 :: (2 :: [1])
    3 :: [2,1]
    [3,2,1]

<!-- language: lang-hs -->

    List.foldr (::) [] [1,2,3]
    1 :: (2 :: (3 :: []))
    1 :: (2 :: [3])
    1 :: [2,3]
    [1,2,3]

## Sorting a list in descending order
By default `List.sort` sorts in ascending order, with the `compare` function.

There are two ways to sort in descending order: one efficient and one inefficient.

 1. **The efficient way**: `List.sortWith` and a descending comparison function.

<!-- language: lang-elm -->

    descending a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

    > List.sortWith descending [1,5,9,7,3]
    [9,7,5,3,1] : List number

2. The inefficient way **(discouraged!)**: `List.sort` and then `List.reverse`.
<!-- language: lang-hs -->
    > List.reverse (List.sort [1,5,9,7,3])
    [9,7,5,3,1] : List number

## Getting elements

    > ourList = [1,2,3,4,5]
    [1,2,3,4,5] : List number
    >
    > firstElement = List.head ourList
    Just 1 : Maybe Int
    >
    > allButFirst = List.tail ourList
    Just [2,3,4,5] : Maybe (List Int)
    
This wrapping into `Maybe` type happens because of the following scenario:

**What should `List.head` return for an empty list?** (Remember, Elm doesn't have exceptions or nulls.)

    > headOfEmpty = List.head []
    Nothing : Maybe Int
    >
    > tailOfEmpty = List.tail []
    Nothing : Maybe (List Int)
    >
    > tailOfAlmostEmpty = List.tail [1] -- warning ... List is a *linked list* :)
    Just [] : Maybe (List Int)

## Transforming every element of a list
`List.map : (a -> b) -> List a -> List b` is a higher-order function that applies a one-parameter function to each element of a list, returning a new list with the modified values.

<!-- language: lang-elm -->

    import String

    ourList : List String
    ourList = 
        ["wubba", "lubba", "dub", "dub"]

    lengths : List Int
    lengths = 
        List.map String.length ourList
    -- [5,5,3,3]

    slices : List String
    slices =
        List.map (String.slice 1 3) ourList
    -- ["ub", "ub", "ub", "ub"]

If you need to know the index of the elements you can use `List.indexedMap : (Int -> a -> b) -> List a -> List b`:

<!-- language: lang-elm -->

    newList : List String
    newList =
        List.indexedMap (\index element -> String.concat [toString index, ": ", element]) ourList
    -- ["0: wubba","1: lubba","2: dub","3: dub"] 



    

## Filtering a list
`List.filter : (a -> Bool) -> List a -> List a` is a higher-order function which takes a one-parameter function from any value to a boolean, and applies that function to every element of a given list, keeping only those elements for which the function returns `True` on. The function that `List.filter` takes as its first parameter is often referred to as a [predicate](http://stackoverflow.com/questions/3230944/what-does-predicate-mean-in-the-context-of-computer-science).

<!-- language: lang-elm -->

    import String

    catStory : List String
    catStory = 
        ["a", "crazy", "cat", "walked", "into", "a", "bar"]

    -- Any word with more than 3 characters is so long!
    isLongWord : String -> Bool
    isLongWord string =
        String.length string > 3

    longWordsFromCatStory : List String
    longWordsFromCatStory = 
        List.filter isLongWord catStory

Evaluate this in `elm-repl`:

    > longWordsFromCatStory
    ["crazy", "walked", "into"] : List String
    >
    > List.filter (String.startsWith "w") longWordsFromCatStory
    ["walked"] : List String


## Pattern Matching on a list
We can match on lists like any other data type, though they are somewhat unique, in that the constructor for building up lists is the infix function `::`. (See the example [Creating a list][1] for more on how that works.)

<!-- language: lang-elm -->

    matchMyList : List SomeType -> SomeOtherType
    matchMyList myList = 
        case myList of
            [] -> 
                emptyCase
    
            (theHead :: theRest) ->
                doSomethingWith theHead theRest

We can match as many elements in the list as we want:

<!-- language: lang-elm -->

    hasAtLeast2Elems : List a -> Bool
    hasAtLeast2Elems myList =
        case myList of
            (e1 :: e2 :: rest) -> 
                True
        
            _ -> 
                False

    hasAtLeast3Elems : List a -> Bool
    hasAtLeast3Elems myList =
        case myList of
            (e1 :: e2 :: e3 :: rest) -> 
                True
        
            _ -> 
                False


  [1]: https://www.wikiod.com/elm/lists-and-iteration#Creating a list

## Creating a list by repeating a value
<!-- language: lang-hs -->

    > List.repeat 3 "abc"
    ["abc","abc","abc"] : List String

You can give `List.repeat` any value:
<!-- language: lang-hs -->

    > List.repeat 2 {a = 1, b = (2,True)}
    [{a = 1, b = (2,True)}, {a = 1, b = (2,True)}]
      : List {a : Int, b : (Int, Bool)}

## Sorting a list
By default, `List.sort` sorts in ascending order.

<!-- language: lang-hs -->

    > List.sort [3,1,5]
    [1,3,5] : List number

`List.sort` needs the list elements to be [`comparable`][1]. That means: `String`, `Char`, `number` (`Int` and `Float`), `List` of `comparable` or [tuple][2] of `comparable`.

<!-- language: lang-hs -->

    > List.sort [(5,"ddd"),(4,"zzz"),(5,"aaa")]
    [(4,"zzz"),(5,"aaa"),(5,"ddd")] : List ( number, String )

    > List.sort [[3,4],[2,3],[4,5],[1,2]]
    [[1,2],[2,3],[3,4],[4,5]] : List (List number)

You can't sort lists of `Bool` or objects with `List.sort`. For that see Sorting a list with custom comparator.

<!-- language: lang-hs -->

    > List.sort [True, False]
    -- error, can't compare Bools

  [1]: https://www.wikiod.com/elm/types-type-variables-and-type-constructors#Comparable data types
  [2]: https://www.wikiod.com/elm/collecting-data-tuples-records-and-dictionaries#Tuples

## Sorting a list with custom comparator
`List.sortWith` allows you to sort lists with data of any shape - you supply it with a comparison function.

<!-- language: lang-elm -->

    compareBools : Bool -> Bool -> Order
    compareBools a b =
        case (a,b) of
            (False, True) ->
                LT

            (True, False) ->
                GT

            _ ->
                EQ
            
    > List.sortWith compareBools [False, True, False, True]
    [False, False, True, True] : List Bool

## Reversing a list
Note: this is not very efficient due to the nature of `List` (see Remarks below). It will be better to **construct the list the "right" way from the beginning** than to construct it and then reverse it.

<!-- language: lang-hs -->

    > List.reverse [1,3,5,7,9]
    [9,7,5,3,1] : List number

## Sorting a list by a derived value
`List.sortBy` allows to use a function on the elements and use its result for the comparison.

<!-- language: lang-elm -->

    > List.sortBy String.length ["longest","short","medium"]
    ["short","medium","longest"] : List String
    -- because the lengths are: [7,5,6]

It also nicely works with record accessors:

<!-- language: lang-elm -->

    people =
        [ { name = "John", age = 43 }
        , { name = "Alice", age = 30 }
        , { name = "Rupert", age = 12 }
        ]

    > List.sortBy .age people
    [ { name = "Rupert", age = 12 }
    , { name = "Alice", age = 30 }
    , { name = "John", age = 43 }
    ] : List {name: String, age: number}

    > List.sortBy .name people
    [ { name = "Alice", age = 30 }
    , { name = "John", age = 43 }
    , { name = "Rupert", age = 12 }
    ] : List {name: String, age: number}

