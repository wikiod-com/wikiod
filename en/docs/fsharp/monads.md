---
title: "Monads"
slug: "monads"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Understanding Monads comes from practice
*This topic is intended for intermediate to advanced F# developers*

"What are Monads?" is a common question. This is [easy to answer](https://wiki.haskell.org/Monad) but like in [Hitchhikers guide to galaxy](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life.2C_the_Universe.2C_and_Everything_.2842.29) we realize we don't understand the answer because we didn't know what we were asking after.

[Many](https://two-wrongs.com/the-what-are-monads-fallacy) believe the way to understanding Monads is by practicing them. As programmers we typically don't care for the mathematical foundation for what Liskov's Substitution Principle, sub-types or sub-classes are. By using these ideas we have acquired an intuition for what they represent. The same is true for Monads.

In order to help you get started with Monads this example demonstrates how to build a [Monadic Parser Combinator](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) library. This might help you get started but understanding will come from writing your own Monadic library.

**Enough prose, time for code**

The Parser type:

    // A Parser<'T> is a function that takes a string and position
    //  and returns an optionally parsed value and a position
    //  A parsed value means the position points to the character following the parsed value
    //  No parsed value indicates a parse failure at the position
    type Parser<'T> = Parser of (string*int -> 'T option*int)

Using this definition of a Parser we define some fundamental parser functions

    // Runs a parser 't' on the input string 's'
    let run t s =
      let (Parser tps) = t
      tps (s, 0)

    // Different ways to create parser result
    let succeedWith v p = Some v, p
    let failAt        p = None  , p

    // The 'satisfy' parser succeeds if the character at the current position 
    //  passes the 'sat' function
    let satisfy sat : Parser<char> = Parser <| fun (s, p) ->
      if p < s.Length && sat s.[p] then succeedWith s.[p] (p + 1)
      else failAt p

    // 'eos' succeeds if the position is beyond the last character.
    //  Useful when testing if the parser have consumed the full string
    let eos : Parser<unit> = Parser <| fun (s, p) ->
      if p < s.Length then failAt p
      else succeedWith () p

    let anyChar       = satisfy (fun _ -> true)
    let char ch       = satisfy ((=) ch)
    let digit         = satisfy System.Char.IsDigit
    let letter        = satisfy System.Char.IsLetter

`satisfy` is a function that given a `sat` function produces a parser that succeeds if we haven't passed `EOS` and the character at the current position passes the `sat` function. Using `satisfy` we create a number of useful character parsers.

Running this in FSI:

    > run digit "";;
    val it : char option * int = (null, 0)
    > run digit "123";;
    val it : char option * int = (Some '1', 1)
    > run digit "hello";;
    val it : char option * int = (null, 0)

We have some fundamental parsers into place. We will combine them into more powerful parsers using parser combinator functions

    // 'fail' is a parser that always fails
    let fail<'T>      = Parser <| fun (s, p) -> failAt p
    // 'return_' is a parser that always succeed with value 'v'
    let return_ v     = Parser <| fun (s, p) -> succeedWith v p

    // 'bind' let us combine two parser into a more complex parser
    let bind t uf     = Parser <| fun (s, p) ->
      let (Parser tps) = t
      let tov, tp = tps (s, p)
      match tov with
      | None    -> None, tp
      | Some tv ->
        let u = uf tv
        let (Parser ups) = u
        ups (s, tp)

The names and signatures are [not arbitrarily chosen](https://wiki.haskell.org/Monad#Monad_class) but we will not delve on this, instead let's see how we use `bind` to combine parser into more complex ones:

    > run (bind digit (fun v -> digit)) "123";;
    val it : char option * int = (Some '2', 2)
    > run (bind digit (fun v -> bind digit (fun u -> return_ (v,u)))) "123";;
    val it : (char * char) option * int = (Some ('1', '2'), 2)
    > run (bind digit (fun v -> bind digit (fun u -> return_ (v,u)))) "1";;
    val it : (char * char) option * int = (null, 1)

What this shows us is that `bind` allows us to combine two parsers into a more complex parser. As the result of `bind` is a parser that in turn can be combined again.

    > run (bind digit (fun v -> bind digit (fun w -> bind digit (fun u -> return_ (v,w,u))))) "123";;
    val it : (char * char * char) option * int = (Some ('1', '2', '3'), 3)

`bind` will be the fundamental way we combine parsers although we will define helper functions to simplify the syntax.

One of the things that can simplify syntax are [computation expressions](https://msdn.microsoft.com/visualfsharpdocs/conceptual/computation-expressions-%5bfsharp%5d). They are easy to define:

    type ParserBuilder() =
      member x.Bind       (t, uf) = bind      t   uf
      member x.Return     v       = return_   v
      member x.ReturnFrom t       = t

    // 'parser' enables us to combine parsers using 'parser { ... }' syntax
    let parser = ParserBuilder()

FSI

    let p = parser {
              let! v = digit
              let! u = digit
              return v,u
            }
    run p "123"
    val p : Parser<char * char> = Parser <fun:bind@49-1>
    val it : (char * char) option * int = (Some ('1', '2'), 2)

This is equivalent to:

    > let p = bind digit (fun v -> bind digit (fun u -> return_ (v,u)))
    run p "123";;
    val p : Parser<char * char> = Parser <fun:bind@49-1>
    val it : (char * char) option * int = (Some ('1', '2'), 2)

Another fundamental parser combinator we are going to use alot is `orElse`:

    // 'orElse' creates a parser that runs parser 't' first, if that is successful
    //  the result is returned otherwise the result of parser 'u' is returned
    let orElse t u    = Parser <| fun (s, p) ->
      let (Parser tps) = t
      let tov, tp = tps (s, p)
      match tov with
      | None    -> 
        let (Parser ups) = u
        ups (s, p)
      | Some tv -> succeedWith tv tp

This allows us to define `letterOrDigit` like this:

    > let letterOrDigit = orElse letter digit;;
    val letterOrDigit : Parser<char> = Parser <fun:orElse@70-1>
    > run letterOrDigit "123";;
    val it : char option * int = (Some '1', 1)
    > run letterOrDigit "hello";;
    val it : char option * int = (Some 'h', 1)
    > run letterOrDigit "!!!";;
    val it : char option * int = (null, 0)
 
**A note on Infix operators**

A common concern over FP is the use of unusual infix operators like `>>=`, `>=>`, `<-` and so on. However, most aren't concerned over the use of `+`, `-`, `*`, `/` and `%`, these are well known operators used to compose values. However, a big part in FP is about composing not just values but functions as well. To an intermediate FP developer the infix operators `>>=`, `>=>`, `<-` are well-known and should have specific signatures as well as semantics.

For the functions we have defined so far we would define the following infix operators used to combine parsers:

    let (>>=)   t   uf  = bind t uf
    let (<|>)   t   u   = orElse t u

So `>>=` means `bind` and `<|>` means `orElse`.

This allows us combine parsers more succinct:

    let letterOrDigit = letter <|> digit
    let p = digit >>= fun v -> digit >>= fun u -> return_ (v,u)

In order to define some advanced parser combinators that will allow us to parse more complex expression we define a few more simple parser combinators:

    // 'map' runs parser 't' and maps the result using 'm'
    let map m t       = t >>= (m >> return_)
    let (>>!) t m     = map m t
    let (>>%) t v     = t >>! (fun _ -> v)

    // 'opt' takes a parser 't' and creates a parser that always succeed but
    //  if parser 't' fails the new parser will produce the value 'None'
    let opt t         = (t >>! Some) <|> (return_ None)

    // 'pair' runs parser 't' and 'u' and returns a pair of 't' and 'u' results
    let pair t u      = 
      parser {
        let! tv = t
        let! tu = u
        return tv, tu
      }

We are ready to define `many` and `sepBy` which are more advanced as they apply the input parsers until they fail. Then `many` and `sepBy` returns the aggregated result:

    // 'many' applies parser 't' until it fails and returns all successful
    //  parser results as a list
    let many t =
      let ot = opt t
      let rec loop vs = ot >>= function Some v -> loop (v::vs) | None -> return_ (List.rev vs)
      loop []

    // 'sepBy' applies parser 't' separated by 'sep'. 
    //  The values are reduced with the function 'sep' returns
    let sepBy t sep     =
      let ots = opt (pair sep t)
      let rec loop v = ots >>= function Some (s, n) -> loop (s v n) | None -> return_ v
      t >>= loop

**Creating a simple expression parser**

With the tools we created we can now define a parser for simple expressions like `1+2*3`

We start from the bottom by defining a parser for integers `pint`

    // 'pint' parses an integer
    let pint = 
      let f s v = 10*s + int v - int '0'
      parser {
        let! digits = many digit
        return! 
          match digits with
          | [] -> fail
          | vs -> return_ (List.fold f 0 vs)
      }

We try to parse as many digits as we can, the result is `char list`. If the list is empty we `fail`, otherwise we fold the characters into an integer.

Testing `pint` in FSI:

    > run pint "123";;
    val it : int option * int = (Some 123, 3)


In addition we need to parse the different kind of operators used to combine integer values:

    // operator parsers, note that the parser result is the operator function 
    let padd      = char '+' >>% (+)
    let psubtract = char '-' >>% (-)
    let pmultiply = char '*' >>% (*)
    let pdivide   = char '/' >>% (/)
    let pmodulus  = char '%' >>% (%)

FSI:

    > run padd "+";;
    val it : (int -> int -> int) option * int = (Some <fun:padd@121-1>, 1)

Tying it all together:

    // 'pmullike' parsers integers separated by operators with same precedence as multiply
    let pmullike  = sepBy pint (pmultiply <|> pdivide <|> pmodulus)
    // 'paddlike' parsers sub expressions separated by operators with same precedence as add
    let paddlike  = sepBy pmullike (padd <|> psubtract)
    // 'pexpr' is the full expression
    let pexpr     =
      parser {
        let! v = paddlike
        let! _ = eos      // To make sure the full string is consumed
        return v
      }

Running it all in FSI:

    > run pexpr "2+123*2-3";;
    val it : int option * int = (Some 245, 9)

**Conclusion**

By defining `Parser<'T>`, `return_`, `bind` and making sure they obey the [monadic laws](https://wiki.haskell.org/Monad_laws) we have built a simple but powerful Monadic Parser Combinator framework.

Monads and Parsers go together because Parsers are executed on a parser state. Monads allows us to combine parsers while hiding the parser state thus reducing clutter and improving composability.

The framework we have created is slow and produces no error messages, this in order to keep the code succinct. [FParsec](http://www.quanttec.com/fparsec/) provide both acceptable performance as well as excellent error messages.

However, an example alone cannot give understanding of Monads. One has to practice Monads.

Here are some examples on Monads you can try to implement in order to reach your won understanding:

 1. State Monad - Allows hidden environment state to be carried implicitly
 2. Tracer Monad - Allows trace state to be carried implicitly. A variant of State Monad
 3. Turtle Monad - A Monad for creating Turtle (Logos) programs. A variant of State Monad
 4. Continuation Monad - A coroutine Monad. An example of this is `async` in F#

The best thing in order to learn would be to come up with an application for Monads in a domain you are comfortable with. For me that was parsers.

Full source code:

    // A Parser<'T> is a function that takes a string and position
    //  and returns an optionally parsed value and a position
    //  A parsed value means the position points to the character following the parsed value
    //  No parsed value indicates a parse failure at the position
    type Parser<'T> = Parser of (string*int -> 'T option*int)

    // Runs a parser 't' on the input string 's'
    let run t s =
      let (Parser tps) = t
      tps (s, 0)

    // Different ways to create parser result
    let succeedWith v p = Some v, p
    let failAt        p = None  , p

    // The 'satisfy' parser succeeds if the character at the current position 
    //  passes the 'sat' function
    let satisfy sat : Parser<char> = Parser <| fun (s, p) ->
      if p < s.Length && sat s.[p] then succeedWith s.[p] (p + 1)
      else failAt p

    // 'eos' succeeds if the position is beyond the last character.
    //  Useful when testing if the parser have consumed the full string
    let eos : Parser<unit> = Parser <| fun (s, p) ->
      if p < s.Length then failAt p
      else succeedWith () p

    let anyChar       = satisfy (fun _ -> true)
    let char ch       = satisfy ((=) ch)
    let digit         = satisfy System.Char.IsDigit
    let letter        = satisfy System.Char.IsLetter

    // 'fail' is a parser that always fails
    let fail<'T>      = Parser <| fun (s, p) -> failAt p
    // 'return_' is a parser that always succeed with value 'v'
    let return_ v     = Parser <| fun (s, p) -> succeedWith v p

    // 'bind' let us combine two parser into a more complex parser
    let bind t uf     = Parser <| fun (s, p) ->
      let (Parser tps) = t
      let tov, tp = tps (s, p)
      match tov with
      | None    -> None, tp
      | Some tv ->
        let u = uf tv
        let (Parser ups) = u
        ups (s, tp)

    type ParserBuilder() =
      member x.Bind       (t, uf) = bind      t   uf
      member x.Return     v       = return_   v
      member x.ReturnFrom t       = t

    // 'parser' enables us to combine parsers using 'parser { ... }' syntax
    let parser = ParserBuilder()

    // 'orElse' creates a parser that runs parser 't' first, if that is successful
    //  the result is returned otherwise the result of parser 'u' is returned
    let orElse t u    = Parser <| fun (s, p) ->
      let (Parser tps) = t
      let tov, tp = tps (s, p)
      match tov with
      | None    -> 
        let (Parser ups) = u
        ups (s, p)
      | Some tv -> succeedWith tv tp

    let (>>=) t uf    = bind t uf
    let (<|>) t u     = orElse t u

    // 'map' runs parser 't' and maps the result using 'm'
    let map m t       = t >>= (m >> return_)
    let (>>!) t m     = map m t
    let (>>%) t v     = t >>! (fun _ -> v)

    // 'opt' takes a parser 't' and creates a parser that always succeed but
    //  if parser 't' fails the new parser will produce the value 'None'
    let opt t         = (t >>! Some) <|> (return_ None)

    // 'pair' runs parser 't' and 'u' and returns a pair of 't' and 'u' results
    let pair t u      = 
      parser {
        let! tv = t
        let! tu = u
        return tv, tu
      }

    // 'many' applies parser 't' until it fails and returns all successful
    //  parser results as a list
    let many t =
      let ot = opt t
      let rec loop vs = ot >>= function Some v -> loop (v::vs) | None -> return_ (List.rev vs)
      loop []

    // 'sepBy' applies parser 't' separated by 'sep'. 
    //  The values are reduced with the function 'sep' returns
    let sepBy t sep     =
      let ots = opt (pair sep t)
      let rec loop v = ots >>= function Some (s, n) -> loop (s v n) | None -> return_ v
      t >>= loop

    // A simplistic integer expression parser

    // 'pint' parses an integer
    let pint = 
      let f s v = 10*s + int v - int '0'
      parser {
        let! digits = many digit
        return! 
          match digits with
          | [] -> fail
          | vs -> return_ (List.fold f 0 vs)
      }

    // operator parsers, note that the parser result is the operator function 
    let padd      = char '+' >>% (+)
    let psubtract = char '-' >>% (-)
    let pmultiply = char '*' >>% (*)
    let pdivide   = char '/' >>% (/)
    let pmodulus  = char '%' >>% (%)

    // 'pmullike' parsers integers separated by operators with same precedence as multiply
    let pmullike  = sepBy pint (pmultiply <|> pdivide <|> pmodulus)
    // 'paddlike' parsers sub expressions separated by operators with same precedence as add
    let paddlike  = sepBy pmullike (padd <|> psubtract)
    // 'pexpr' is the full expression
    let pexpr     =
      parser {
        let! v = paddlike
        let! _ = eos      // To make sure the full string is consumed
        return v
      }


## Computation Expressions provide an alternative syntax to chain Monads
Related to Monads are `F#` [computation expressions](https://msdn.microsoft.com/visualfsharpdocs/conceptual/computation-expressions-%5bfsharp%5d)
(`CE`). A programmer typically implements a `CE` to provide an alternative approach to chaining Monads,
ie instead of this:

    let v = m >>= fun x -> n >>= fun y -> return_ (x, y)

You can write this:

    let v = ce {
        let! x = m
        let! y = n
        return x, y
      }

Both styles are equivalent and it's up to developer preference which one to pick.

In order to demonstrate how to implement a `CE` imagine you like all traces to
include a correlation id. This correlation id will help correlating traces
that belong to the same call. This is very useful when have log files that
contains traces from concurrent calls.

The problem is that it's cumbersome to include the correlation id as an argument to all functions.
As Monads [allows carrying implicit state](https://wiki.haskell.org/Monad) we will
define a Log Monad to hide the log context (ie the correlation id).

We begin by defining a log context and the type of a function that traces with log context:

    type Context =
      {
        CorrelationId : Guid
      }
      static member New () : Context = { CorrelationId = Guid.NewGuid () }

    type Function<'T> = Context -> 'T

    // Runs a Function<'T> with a new log context
    let run t = t (Context.New ())

We also define two trace functions that will log with the correlation id
from the log context:

    let trace v   : Function<_> = fun ctx -> printfn "CorrelationId: %A - %A" ctx.CorrelationId v
    let tracef fmt              = kprintf trace fmt

`trace` is a `Function<unit>` which means it will be passed a log context when invoked.
From the log context we pick up the correlation id and traces it together with `v`

In addition we define `bind` and `return_` and as they follow the
[Monad Laws](https://wiki.haskell.org/Monad_laws) this forms our Log Monad.

    let bind t uf : Function<_> = fun ctx ->
      let tv = t ctx  // Invoke t with the log context
      let u  = uf tv  // Create u function using result of t
      u ctx           // Invoke u with the log context
    
    // >>= is the common infix operator for bind
    let inline (>>=) (t, uf) = bind t uf
    
    let return_ v : Function<_> = fun ctx -> v

Finally we define `LogBuilder` that will enable us to use `CE` syntax to chain
`Log` Monads.

    type LogBuilder() =
      member x.Bind   (t, uf) = bind t uf
      member x.Return v       = return_ v

    // This enables us to write function like: let f = log { ... }
    let log = Log.LogBuilder ()


We can now define our functions that should have the implicit log context:

    let f x y =
      log {
        do! Log.tracef "f: called with: x = %d, y = %d" x y
        return x + y
      }

    let g =
      log {
        do! Log.trace "g: starting..."
        let! v = f 1 2
        do! Log.tracef "g: f produced %d" v
        return v
      }

We execute g with:

    printfn "g produced %A" (Log.run g)

Which prints:

    CorrelationId: 33342765-2f96-42da-8b57-6fa9cdaf060f - "g: starting..."
    CorrelationId: 33342765-2f96-42da-8b57-6fa9cdaf060f - "f: called with: x = 1, y = 2"
    CorrelationId: 33342765-2f96-42da-8b57-6fa9cdaf060f - "g: f produced 3"
    g produced 3

Notice that the CorrelationId is implicitly carried from `run` to `g` to `f` which
allows us the correlate the log entries during trouble shooting.

`CE` has [lot more features](https://msdn.microsoft.com/visualfsharpdocs/conceptual/computation-expressions-%5bfsharp%5d)
but this should help you get started defining your own `CE`:s.

Full code:

    module Log =
      open System
      open FSharp.Core.Printf

      type Context =
        {
          CorrelationId : Guid
        }
        static member New () : Context = { CorrelationId = Guid.NewGuid () }

      type Function<'T> = Context -> 'T

      // Runs a Function<'T> with a new log context
      let run t = t (Context.New ())

      let trace v   : Function<_> = fun ctx -> printfn "CorrelationId: %A - %A" ctx.CorrelationId v
      let tracef fmt              = kprintf trace fmt

      let bind t uf : Function<_> = fun ctx ->
        let tv = t ctx  // Invoke t with the log context
        let u  = uf tv  // Create u function using result of t
        u ctx           // Invoke u with the log context

      // >>= is the common infix operator for bind
      let inline (>>=) (t, uf) = bind t uf

      let return_ v : Function<_> = fun ctx -> v

      type LogBuilder() =
        member x.Bind   (t, uf) = bind t uf
        member x.Return v       = return_ v

    // This enables us to write function like: let f = log { ... }
    let log = Log.LogBuilder ()

    let f x y =
      log {
        do! Log.tracef "f: called with: x = %d, y = %d" x y
        return x + y
      }

    let g =
      log {
        do! Log.trace "g: starting..."
        let! v = f 1 2
        do! Log.tracef "g: f produced %d" v
        return v
      }

    [<EntryPoint>]
    let main argv =
      printfn "g produced %A" (Log.run g)
      0


