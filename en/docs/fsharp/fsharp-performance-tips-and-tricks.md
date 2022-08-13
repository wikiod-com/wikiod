---
title: "F# Performance Tips and Tricks"
slug: "f-performance-tips-and-tricks"
draft: false
images: []
weight: 9930
type: docs
toc: true
---

## Measure and Verify your performance assumptions
*This example is written with `F#` in mind but the ideas are applicable in all
environments*

The first rule when optimizing for performance is to not to rely assumption;
always Measure and Verify your assumptions.

As we are not writing machine code directly it is hard to predict how the compiler
and JIT:er transform your program to machine code. That's why we need to Measure
the execution time to see that we get the performance improvement we expect and
Verify that the actual program doesn't contain any hidden overhead.

Verification is the quite simple process that involves reverse engineering the
executable using for example tools like [ILSpy](https://github.com/icsharpcode/ILSpy).
The JIT:er complicates Verification in that seeing the actual machine code is
tricky but doable. However, usually examining the `IL-code` gives the big gains.

The harder problem is Measuring; harder because it's tricky to setup realistic
situations that allows to measure improvements in code. Still Measuring is
invaluable.

**Analyzing simple F# functions**

Let's examine some simple `F#` functions that accumulates all integers in `1..n`
written in various different ways. As the range is a simple Arithmetic Series
the result can be computed directly but for the purpose of this example we
iterate over the range.

First we define some useful functions for measuring the time a function takes:

    // now () returns current time in milliseconds since start
    let now : unit -> int64 =
      let sw = System.Diagnostics.Stopwatch ()
      sw.Start ()
      fun () -> sw.ElapsedMilliseconds

    // time estimates the time 'action' repeated a number of times
    let time repeat action : int64*'T =
      let v = action ()  // Warm-up and compute value

      let b = now ()
      for i = 1 to repeat do
        action () |> ignore
      let e = now ()

      e - b, v

`time` runs an action repeatedly we need to run the tests for a few hundred
milliseconds to reduce variance.

Then we define a few functions that accumulates all integers in `1..n` in
different ways.

    // Accumulates all integers 1..n using 'List'
    let accumulateUsingList n =
      List.init (n + 1) id
      |> List.sum

    // Accumulates all integers 1..n using 'Seq'
    let accumulateUsingSeq n =
      Seq.init (n + 1) id
      |> Seq.sum

    // Accumulates all integers 1..n using 'for-expression'
    let accumulateUsingFor n =
      let mutable sum = 0
      for i = 1 to n do
        sum <- sum + i
      sum

    // Accumulates all integers 1..n using 'foreach-expression' over range
    let accumulateUsingForEach n =
      let mutable sum = 0
      for i in 1..n do
        sum <- sum + i
      sum

    // Accumulates all integers 1..n using 'foreach-expression' over list range
    let accumulateUsingForEachOverList n =
      let mutable sum = 0
      for i in [1..n] do
        sum <- sum + i
      sum

    // Accumulates every second integer 1..n using 'foreach-expression' over range
    let accumulateUsingForEachStep2 n =
      let mutable sum = 0
      for i in 1..2..n do
        sum <- sum + i
      sum

    // Accumulates all 64 bit integers 1..n using 'foreach-expression' over range
    let accumulateUsingForEach64 n =
      let mutable sum = 0L
      for i in 1L..int64 n do
        sum <- sum + i
      sum |> int

    // Accumulates all integers n..1 using 'for-expression' in reverse order
    let accumulateUsingReverseFor n =
      let mutable sum = 0
      for i = n downto 1 do
        sum <- sum + i
      sum

    // Accumulates all 64 integers n..1 using 'tail-recursion' in reverse order
    let accumulateUsingReverseTailRecursion n =
      let rec loop sum i =
        if i > 0 then
          loop (sum + i) (i - 1)
        else
          sum
      loop 0 n

We assume the result to be the same (except for one of the functions that uses
increment of `2`) but is there difference in performance. To Measure this the
following function is defined:

    let testRun (path : string) =
      use testResult = new System.IO.StreamWriter (path)
      let write   (l : string)  = testResult.WriteLine l
      let writef  fmt           = FSharp.Core.Printf.kprintf write fmt

      write "Name\tTotal\tOuter\tInner\tCC0\tCC1\tCC2\tTime\tResult"

      // total is the total number of iterations being executed
      let total   = 10000000
      // outers let us variate the relation between the inner and outer loop
      //  this is often useful when the algorithm allocates different amount of memory
      //  depending on the input size. This can affect cache locality
      let outers  = [| 1000; 10000; 100000 |]
      for outer in outers do
        let inner = total / outer

        // multiplier is used to increase resolution of certain tests that are significantly
        //  faster than the slower ones

        let testCases =
          [|
        //   Name of test                         multiplier    action
            "List"                              , 1           , accumulateUsingList
            "Seq"                               , 1           , accumulateUsingSeq
            "for-expression"                    , 100         , accumulateUsingFor
            "foreach-expression"                , 100         , accumulateUsingForEach
            "foreach-expression over List"      , 1           , accumulateUsingForEachOverList
            "foreach-expression increment of 2" , 1           , accumulateUsingForEachStep2
            "foreach-expression over 64 bit"    , 1           , accumulateUsingForEach64
            "reverse for-expression"            , 100         , accumulateUsingReverseFor
            "reverse tail-recursion"            , 100         , accumulateUsingReverseTailRecursion
          |]
        for name, multiplier, a in testCases do
          System.GC.Collect (2, System.GCCollectionMode.Forced, true)
          let cc g = System.GC.CollectionCount g

          printfn "Accumulate using %s with outer=%d and inner=%d ..." name outer inner

          // Collect collection counters before test run
          let pcc0, pcc1, pcc2 = cc 0, cc 1, cc 2

          let ms, result       = time (outer*multiplier) (fun () -> a inner)
          let ms               = (float ms / float multiplier)

          // Collect collection counters after test run
          let acc0, acc1, acc2 = cc 0, cc 1, cc 2
          let cc0, cc1, cc2    = acc0 - pcc0, acc1 - pcc1, acc1 - pcc1
          printfn "  ... took: %f ms, GC collection count %d,%d,%d and produced %A" ms cc0 cc1 cc2 result

          writef "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%f\t%d" name total outer inner cc0 cc1 cc2 ms result

The test result while running on .NET 4.5.2 x64:

[![Test results on .NET 4.5.2 x64][1]][1]

We see dramatic difference and some of the results are unexpectedly bad.

Let's look at the bad cases:

**List**

    // Accumulates all integers 1..n using 'List'
    let accumulateUsingList n =
      List.init (n + 1) id
      |> List.sum

What happens here is a full list containing all integers `1..n` is created and
reduced using a sum. This should be more expensive than just iterating and
accumulating over the range, it seems about ~42x slower than the for loop.

In addition, we can see that the GC ran about 100x during the test run because
the code allocated a lot of objects. This also costs CPU.

**Seq**

    // Accumulates all integers 1..n using 'Seq'
    let accumulateUsingSeq n =
      Seq.init (n + 1) id
      |> Seq.sum

The `Seq` version doesn't allocate a full `List` so it's a bit suprising that
this ~270x slower than the for loop. In addition, we see that the GC has executed
661x.

`Seq` is inefficient when the amount of work per item is very small
(in this case aggregating two integers).

The point is not to never use `Seq`. The point is to Measure.

(**manofstick edit:** `Seq.init` is the culprit of this severe performance issue. It is much more efficent to use the expression `{ 0 .. n }` instead of `Seq.init (n+1) id`. This will become much more efficient still when [this PR](https://github.com/Microsoft/visualfsharp/pull/2587) is merged and released. Even after the release though, the original `Seq.init ... |> Seq.sum` will still be slow, but somewhat counter-intuitively, `Seq.init ... |> Seq.map id |> Seq.sum` will be quite fast. This was to maintain backward compatibility with `Seq.init`s implementation, which doesn't calculate `Current` initially, but rather wraps them in a `Lazy` object - although this too should perform a little better due to [this PR](https://github.com/dotnet/coreclr/pull/8963). Note to editor: sorry this is kind of rambling notes, but I don't want people to be put off Seq when improvement is just around the corner... _When that times does come it would be good to update the charts that are on this page._)

**foreach-expression over List**

    // Accumulates all integers 1..n using 'foreach-expression' over range
    let accumulateUsingForEach n =
      let mutable sum = 0
      for i in 1..n do
        sum <- sum + i
      sum

    // Accumulates all integers 1..n using 'foreach-expression' over list range
    let accumulateUsingForEachOverList n =
      let mutable sum = 0
      for i in [1..n] do
        sum <- sum + i
      sum

The difference between these two function is very subtle but the performance
difference is not, roughly ~76x. Why? Let's reverse engineer the bad code:

    public static int accumulateUsingForEach(int n)
    {
      int sum = 0;
      int i = 1;
      if (n >= i)
      {
        do
        {
          sum += i;
          i++;
        }
        while (i != n + 1);
      }
      return sum;
    }

    public static int accumulateUsingForEachOverList(int n)
    {
      int sum = 0;
      FSharpList<int> fSharpList = SeqModule.ToList<int>(Operators.CreateSequence<int>(Operators.OperatorIntrinsics.RangeInt32(1, 1, n)));
      for (FSharpList<int> tailOrNull = fSharpList.TailOrNull; tailOrNull != null; tailOrNull = fSharpList.TailOrNull)
      {
        int i = fSharpList.HeadOrDefault;
        sum += i;
        fSharpList = tailOrNull;
      }
      return sum;
    }

`accumulateUsingForEach` is implemented as an efficient `while` loop but
`for i in [1..n]` is converted into:

    FSharpList<int> fSharpList =
      SeqModule.ToList<int>(
        Operators.CreateSequence<int>(
          Operators.OperatorIntrinsics.RangeInt32(1, 1, n)));

This means first we create a `Seq` over `1..n` and finally calls `ToList`.

Expensive.

**foreach-expression increment of 2**

    // Accumulates all integers 1..n using 'foreach-expression' over range
    let accumulateUsingForEach n =
      let mutable sum = 0
      for i in 1..n do
        sum <- sum + i
      sum

    // Accumulates every second integer 1..n using 'foreach-expression' over range
    let accumulateUsingForEachStep2 n =
      let mutable sum = 0
      for i in 1..2..n do
        sum <- sum + i
      sum

Once again the difference between these two functions are subtle but the performance
difference is brutal: ~25x

Once again let's run `ILSpy`:

    public static int accumulateUsingForEachStep2(int n)
    {
      int sum = 0;
      IEnumerable<int> enumerable = Operators.OperatorIntrinsics.RangeInt32(1, 2, n);
      foreach (int i in enumerable)
      {
        sum += i;
      }
      return sum;
    }

A `Seq` is created over `1..2..n` and then we iterate over `Seq` using the
enumerator.

We were expecting `F#` to create something like this:

    public static int accumulateUsingForEachStep2(int n)
    {
      int sum = 0;
      for (int i = 1; i < n; i += 2)
      {
        sum += i;
      }
      return sum;
    }

However, `F#` compiler only supports efficient for loops over int32 ranges that
increment by one. For all other cases it falls back on
`Operators.OperatorIntrinsics.RangeInt32`. Which will explain the next suprising
result

**foreach-expression over 64 bit**

    // Accumulates all 64 bit integers 1..n using 'foreach-expression' over range
    let accumulateUsingForEach64 n =
      let mutable sum = 0L
      for i in 1L..int64 n do
        sum <- sum + i
      sum |> int

This performs ~47x slower than the for loop, the only difference is that we iterate
over 64 bit integers. `ILSpy` shows us why:

    public static int accumulateUsingForEach64(int n)
    {
      long sum = 0L;
      IEnumerable<long> enumerable = Operators.OperatorIntrinsics.RangeInt64(1L, 1L, (long)n);
      foreach (long i in enumerable)
      {
        sum += i;
      }
      return (int)sum;
    }

`F#` only supports efficient for loops for `int32` numbers it has to use the
fallback `Operators.OperatorIntrinsics.RangeInt64`.

The other cases performs roughly similar:

[![Test results on .NET 4.5.2 x64][2]][2]

The reason the performance degrades for larger test runs is that the overhead of
invoking the `action` is growing as we doing less and less work in `action`.

Looping towards `0` can sometimes give performance benefits as it might save a
CPU register but in this case the CPU has registers to spare so it doesn't seem
to make a difference.

**Conclusion**

Measuring is important because otherwise we might think all these alternatives
are equivalent but some alternatives are ~270x slower than others.

The Verification step involves reverse engineering the executable helps us
explain *why* we did or did not get performance we expected. In addition,
Verification can help us predict performance in the cases it's too difficult to
do a proper Measurement.

It's hard to predict performance there always Measure, always Verify your
performance assumptions.

  [1]: http://i.stack.imgur.com/f67LD.png
  [2]: http://i.stack.imgur.com/3VG4p.png

## Using tail-recursion for efficient iteration
Coming from imperative languages many developers wonder how to write a `for-loop` that exits early as `F#` doesn't support `break`, `continue` or `return`. 
The answer in `F#` is to use [tail-recursion](http://stackoverflow.com/questions/33923/what-is-tail-recursion) which is a flexible 
and idiomatic way to iterate while still providing excellent performance.

Say we want to implement `tryFind` for `List`. If `F#` supported `return` we would write `tryFind` a bit like this:

    let tryFind predicate vs =
      for v in vs do
        if predicate v then
          return Some v
      None

This doesn't work in `F#`. Instead we write the function using tail-recursion:

    let tryFind predicate vs =
      let rec loop = function
        | v::vs -> if predicate v then 
                       Some v 
                   else 
                       loop vs
        | _ -> None
      loop vs

Tail-recursion is performant in `F#` because when the `F#` compiler detects that a function is tail-recursive it rewrites 
the recursion into an efficient `while-loop`. Using `ILSpy` we can see that this is true for our function `loop`:

    internal static FSharpOption<a> loop@3-10<a>(FSharpFunc<a, bool> predicate, FSharpList<a> _arg1)
    {
      while (_arg1.TailOrNull != null)
      {
        FSharpList<a> fSharpList = _arg1;
        FSharpList<a> vs = fSharpList.TailOrNull;
        a v = fSharpList.HeadOrDefault;
        if (predicate.Invoke(v))
        {
          return FSharpOption<a>.Some(v);
        }
        FSharpFunc<a, bool> arg_2D_0 = predicate;
        _arg1 = vs;
        predicate = arg_2D_0;
      }
      return null;
    }

Apart from some unnecessary assignments (which hopefully the JIT-er eliminates) this is essentially an efficient loop.

In addition, tail-recursion is idiomatic for `F#` as it allows us to avoid mutable state. 
Consider a `sum` function that sums all elements in a `List`. An obvious first try would be this:

    let sum vs =
      let mutable s = LanguagePrimitives.GenericZero
      for v in vs do
        s <- s + v
      s

If we rewrite the loop into tail-recursion we can avoid the mutable state:

    let sum vs =
      let rec loop s = function
        | v::vs -> loop (s + v) vs
        | _ -> s
      loop LanguagePrimitives.GenericZero vs

For efficiency the `F#` compiler transforms this into a `while-loop` that uses mutable state.

## Comparison of different F# data pipelines
In `F#` there are many options for creating data pipelines, for example:
`List`, `Seq` and `Array`.

**What data pipeline is preferable from memory usage and performance perspective?**

In order to answer this we'll compare performance and memory usage using
different pipelines.

**Data Pipeline**

In order to measure the overhead, we will use a data pipeline with low cpu-cost
per items processed:

    let seqTest n =
      Seq.init (n + 1) id
      |> Seq.map    int64
      |> Seq.filter (fun v -> v % 2L = 0L)
      |> Seq.map    ((+) 1L)
      |> Seq.sum

We will create equivalent pipelines for all alternatives and compare them.

We will variate the size of `n` but let the total number of work be the same.

**Data Pipeline Alternatives**

We will compare the following alternatives:

  1. Imperative code
  2. Array (Non-lazy)
  3. List (Non-lazy)
  4. LINQ (Lazy pull stream)
  5. Seq (Lazy pull stream)
  6. Nessos (Lazy pull/push stream)
  7. PullStream (Simplistic pull stream)
  8. PushStream (Simplistic push stream)

Although not a data pipeline we will compare against `Imperative` code since that
most closely match how the CPU executes code. That should be that fastest possible
way to compute the result allowing us to measure the performance overhead of data pipelines.

`Array` and `List` compute a full `Array`/`List` in each step so we expect
memory overhead.

`LINQ` and `Seq` are both based around `IEnumerable<'T>` which is lazy pull stream
(pull means that the consumer stream is pulling data out of the producer stream). We
therefore expect the performance and memory usage to be identical.

`Nessos` is a high-performance stream library that supports both push & pull
(like Java `Stream`).

PullStream and PushStream are simplistic implementations of `Pull` & `Push` streams.

**Performance Results from running on: F# 4.0 - .NET 4.6.1 - x64**

[![Performance Results from running on: F# 4.0 - .NET 4.6.1 - x64][1]][1]

The bars show the elapsed time, lower is better. The total amount of
useful work is the same for all tests so the results are comparable.
This also means that few runs implies larger datasets.

As usual when Measuring one see interesting results.

  1. `List` performance poor is compared to other alternatives for large data sets. This can be because of `GC` or poor cache locality.
  2. `Array` performance better than expected.
  3. `LINQ` performs better than `Seq`, this is unexpected because both are based around `IEnumerable<'T>`. However, `Seq` internally is based around a generic impementation for all algorithms while `LINQ` uses specialized algorithms.
  4. `Push` performs better than `Pull`. This is expected since the push data pipeline has fewer checks
  5. The simplistic `Push` data pipelines performs comparable to `Nessos`. However, `Nessos` supports pull and parallelism.
  6. For small data pipelines the performance of `Nessos` degrades possible because pipelines setup overhead.
  7. As expected the `Imperative` code performed the best

**GC Collection count from running on: F# 4.0 - .NET 4.6.1 - x64**

[![GC Collection count from running on: F# 4.0 - .NET 4.6.1 - x64][2]][2]

The bars shows the total number of `GC` collection counts during the test, lower is better.
This is a measurement of how many objects are created by the data pipeline.

As usual when Measuring one see interesting results.

  1. `List` is expectedly creating more objects than `Array` because a `List` is essentially a single linked list of nodes. An array is a continous memory area.
  2. Looking at the underlying numbers both `List` & `Array` forces 2 generation collections. These kind of collection are expensive.
  3. `Seq` is triggering a surprising amount of collections. It's surprisingly even worse than `List` in this regard.
  4. `LINQ`, `Nessos`, `Push` and `Pull` triggers no collections for few runs. However, objects are allocated so the `GC` eventually will have to run.
  5. As expected since the `Imperative` code allocate no objects no `GC` collections were triggered.

**Conclusion**

All data pipelines do the same amount of useful work in all test cases but we see
significant differences in performance and memory usage between the different pipelines.

In addition, we notice that the overhead of data pipelines differ depending on
the size of data processed. For example, for small sizes `Array` is performing quite well.

One should keep in mind the amount of work performed in each step in the pipeline
is very small in order to measure the overhead. In "real" situations the overhead
of `Seq` might not matter because the actual work is more time consuming.

Of more concern is the memory usage differences. `GC` isn't free and it is
beneficial for long running applications to keep `GC` pressure down.

For `F#` developers concerned about performance and memory usage it's recommended to check
out [Nessos Streams](https://github.com/nessos/Streams).

If you need top-notch performance strategically placed `Imperative` code is worth considering.

Finally, when it comes to performance don't make assumptions. Measure and Verify.

Full source code:

    module PushStream =
      type Receiver<'T> = 'T -> bool
      type Stream<'T>   = Receiver<'T> -> unit

      let inline filter (f : 'T -> bool) (s : Stream<'T>) : Stream<'T> =
        fun r -> s (fun v -> if f v then r v else true)

      let inline map (m : 'T -> 'U) (s : Stream<'T>) : Stream<'U> =
        fun r -> s (fun v -> r (m v))

      let inline range b e : Stream<int> =
        fun r ->
          let rec loop i = if i <= e && r i then loop (i + 1)
          loop b

      let inline sum (s : Stream<'T>) : 'T =
        let mutable state = LanguagePrimitives.GenericZero<'T>
        s (fun v -> state <- state + v; true)
        state

    module PullStream =

      [<Struct>]
      [<NoComparison>]
      [<NoEqualityAttribute>]
      type Maybe<'T>(v : 'T, hasValue : bool) =
        member    x.Value        = v
        member    x.HasValue     = hasValue
        override  x.ToString ()  =
          if hasValue then
            sprintf "Just %A" v
          else
            "Nothing"

      let Nothing<'T>     = Maybe<'T> (Unchecked.defaultof<'T>, false)
      let inline Just v   = Maybe<'T> (v, true)

      type Iterator<'T> = unit -> Maybe<'T>
      type Stream<'T>   = unit -> Iterator<'T>

      let filter (f : 'T -> bool) (s : Stream<'T>) : Stream<'T> =
        fun () ->
          let i = s ()
          let rec pop () =
            let mv = i ()
            if mv.HasValue then
              let v = mv.Value
              if f v then Just v else pop ()
            else
              Nothing
          pop

      let map (m : 'T -> 'U) (s : Stream<'T>) : Stream<'U> =
        fun () ->
          let i = s ()
          let pop () =
            let mv = i ()
            if mv.HasValue then
              Just (m mv.Value)
            else
              Nothing
          pop

      let range b e : Stream<int> =
        fun () ->
          let mutable i = b
          fun () ->
            if i <= e then
              let p = i
              i <- i + 1
              Just p
            else
              Nothing

      let inline sum (s : Stream<'T>) : 'T =
        let i = s ()
        let rec loop state =
          let mv = i ()
          if mv.HasValue then
            loop (state + mv.Value)
          else
            state
        loop LanguagePrimitives.GenericZero<'T>

    module PerfTest =

      open System.Linq
    #if USE_NESSOS
      open Nessos.Streams
    #endif

      let now =
        let sw = System.Diagnostics.Stopwatch ()
        sw.Start ()
        fun () -> sw.ElapsedMilliseconds

      let time n a =
        let inline cc i       = System.GC.CollectionCount i

        let v                 = a ()

        System.GC.Collect (2, System.GCCollectionMode.Forced, true)

        let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
        let b                 = now ()

        for i in 1..n do
          a () |> ignore

        let e = now ()
        let ecc0, ecc1, ecc2  = cc 0, cc 1, cc 2

        v, (e - b), ecc0 - bcc0, ecc1 - bcc1, ecc2 - bcc2

      let arrayTest n =
        Array.init (n + 1) id
        |> Array.map    int64
        |> Array.filter (fun v -> v % 2L = 0L)
        |> Array.map    ((+) 1L)
        |> Array.sum

      let imperativeTest n =
        let rec loop s i =
          if i >= 0L then
            if i % 2L = 0L then
              loop (s + i + 1L) (i - 1L)
            else
              loop s (i - 1L)
          else
            s
        loop 0L (int64 n)

      let linqTest n =
        (((Enumerable.Range(0, n + 1)).Select int64).Where(fun v -> v % 2L = 0L)).Select((+) 1L).Sum()

      let listTest n =
        List.init (n + 1) id
        |> List.map     int64
        |> List.filter  (fun v -> v % 2L = 0L)
        |> List.map     ((+) 1L)
        |> List.sum

    #if USE_NESSOS
      let nessosTest n =
        Stream.initInfinite id
        |> Stream.take    (n + 1)
        |> Stream.map     int64
        |> Stream.filter  (fun v -> v % 2L = 0L)
        |> Stream.map     ((+) 1L)
        |> Stream.sum
    #endif

      let pullTest n =
        PullStream.range 0 n
        |> PullStream.map     int64
        |> PullStream.filter  (fun v -> v % 2L = 0L)
        |> PullStream.map     ((+) 1L)
        |> PullStream.sum

      let pushTest n =
        PushStream.range 0 n
        |> PushStream.map     int64
        |> PushStream.filter  (fun v -> v % 2L = 0L)
        |> PushStream.map     ((+) 1L)
        |> PushStream.sum

      let seqTest n =
        Seq.init (n + 1) id
        |> Seq.map    int64
        |> Seq.filter (fun v -> v % 2L = 0L)
        |> Seq.map    ((+) 1L)
        |> Seq.sum

      let perfTest (path : string) =
        let testCases =
          [|
            "array"       , arrayTest       
            "imperative"  , imperativeTest  
            "linq"        , linqTest        
            "list"        , listTest        
            "seq"         , seqTest         
    #if USE_NESSOS
            "nessos"      , nessosTest      
    #endif
            "pull"        , pullTest        
            "push"        , pushTest        
          |]
        use out                   = new System.IO.StreamWriter (path)
        let write (msg : string)  = out.WriteLine msg
        let writef fmt            = FSharp.Core.Printf.kprintf write fmt

        write "Name\tTotal\tOuter\tInner\tElapsed\tCC0\tCC1\tCC2\tResult"

        let total   = 10000000
        let outers = [| 10; 1000; 1000000 |]
        for outer in outers do
          let inner = total / outer
          for name, a in testCases do
            printfn "Running %s with total=%d, outer=%d, inner=%d ..." name total outer inner
            let v, ms, cc0, cc1, cc2 = time outer (fun () -> a inner)
            printfn "  ... %d ms, cc0=%d, cc1=%d, cc2=%d, result=%A" ms cc0 cc1 cc2 v
            writef "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d" name total outer inner ms cc0 cc1 cc2 v

    [<EntryPoint>]
    let main argv =
      System.Environment.CurrentDirectory <- System.AppDomain.CurrentDomain.BaseDirectory
      PerfTest.perfTest "perf.tsv"
      0

  [1]: http://i.stack.imgur.com/444aS.png
  [2]: http://i.stack.imgur.com/dCb3r.png


