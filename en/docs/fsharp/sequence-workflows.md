---
title: "Sequence Workflows"
slug: "sequence-workflows"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## yield and yield!
In sequence workflows, `yield` adds a single item into the sequence being built. (In monadic terminology, it is `return`.)

    > seq { yield 1; yield 2; yield 3 }
    val it: seq<int> = seq [1; 2; 3]

    > let homogenousTup2ToSeq (a, b) = seq { yield a; yield b }
    > tup2Seq ("foo", "bar")
    val homogenousTup2ToSeq: 'a * 'a -> seq<'a>
    val it: seq<string> = seq ["foo"; "bar"]

`yield!` (pronounced _yield bang_) inserts all the items of another sequence into this sequence being built. Or, in other words, it appends a sequence. (In relation to monads, it is `bind`.)

    > seq { yield 1; yield! [10;11;12]; yield 20 }
    val it: seq<int> = seq [1; 10; 11; 12; 20]
    
    // Creates a sequence containing the items of seq1 and seq2 in order
    > let concat seq1 seq2 = seq { yield! seq1; yield! seq2 }
    > concat ['a'..'c'] ['x'..'z']
    val concat: seq<'a> -> seq<'a> -> seq<'a>
    val it: seq<int> = seq ['a'; 'b'; 'c'; 'x'; 'y'; 'z']

Sequences created by sequence workflows are also lazy, meaning that items of the sequence don't actually get evaluated until they're needed. A few ways to force items include calling `Seq.take` (pulls the first n items into a sequence), `Seq.iter` (applies a function to each item for executing side effects), or `Seq.toList` (converts a sequence to a list). Combining this with recursion is where `yield!` really starts to shine.

    > let rec numbersFrom n = seq { yield n; yield! numbersFrom (n + 1) }
    > let naturals = numbersFrom 0
    val numbersFrom: int -> seq<int>
    val naturals: seq<int> = seq [0; 1; 2; ...]
    
    // Just like Seq.map: applies a mapping function to each item in a sequence to build a new sequence
    > let rec map f seq1 =
          if Seq.isEmpty seq1 then Seq.empty
          else seq { yield f (Seq.head seq1); yield! map f (Seq.tail seq1) }
    > map (fun x -> x * x) [1..10]
    val map: ('a -> 'b) -> seq<'a> -> 'b
    val it: seq<int> = seq [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

## for
`for` sequence expression is designed to look just like its more famous cousin, the imperative for-loop. It "loops" through a sequence and evaluates the body of each iteration into the sequence it is generating. Just like everything sequence related, it is NOT mutable.

    > let oneToTen = seq { for x in 1..10 -> x }
    val oneToTen: seq<int> = seq [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    // Or, equivalently:
    > let oneToTen = seq { for x in 1..10 do yield x }
    val oneToTen: seq<int> = seq [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    
    // Just like Seq.map: applies a mapping function to each item in a sequence to build a new sequence
    > let map mapping seq1 = seq { for x in seq1 do yield mapping x }
    > map (fun x -> x * x) [1..10]
    val map: ('a -> 'b) -> seq<'a> -> seq<'b>
    val it: seq<int> = seq [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
    
    // An infinite sequence of consecutive integers starting at 0
    > let naturals =
          let numbersFrom n = seq { yield n; yield! numbersFrom (n + 1) }
          numbersFrom 0
    // Just like Seq.filter: returns a sequence consisting only of items from the input sequence that satisfy the predicate
    > let filter predicate seq1 = seq { for x in seq1 do if predicate x then yield x }
    > let evenNaturals = naturals |> filter (fun x -> x % 2 = 0)
    val naturals: seq<int> = seq [1; 2; 3; ...]
    val filter: ('a -> bool) -> seq<'a> -> seq<'a>
    val evenNaturals: seq<int> = seq [2; 4; 6; ...]
    
    // Just like Seq.concat: concatenates a collection of sequences together
    > let concat seqSeq = seq { for seq in seqSeq do yield! seq }
    > concat [[1;2;3];[10;20;30]]
    val concat: seq<#seq<'b>> -> seq<'b>
    val it: seq<int> = seq [1; 2; 3; 10; 20; 30]

