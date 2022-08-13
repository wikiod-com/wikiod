---
title: "Optimization"
slug: "optimization"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Compiling your Program for Profiling
The GHC compiler has [mature support][1] for compiling with profiling annotations. 

Using the `-prof` and `-fprof-auto` flags when compiling will add support to your binary for profiling flags for use at runtime.

Suppose we have this program:

    main = print (fib 30)
    fib n = if n < 2 then 1 else fib (n-1) + fib (n-2)

Compiled it like so:

    ghc -prof -fprof-auto -rtsopts Main.hs

Then ran it with runtime system options for profiling:

    ./Main +RTS -p

We will see a `main.prof` file created post execution (once the program has exited), and this will give us all sorts of profiling information such as cost centers which gives us a breakdown of the cost associated with running the various parts of the code:

    

        Wed Oct 12 16:14 2011 Time and Allocation Profiling Report  (Final)
    
               Main +RTS -p -RTS
    
            total time  =        0.68 secs   (34 ticks @ 20 ms)
            total alloc = 204,677,844 bytes  (excludes profiling overheads)
    
    COST CENTRE MODULE  %time %alloc
    
    fib         Main    100.0  100.0
    
    
                                                          individual     inherited
    COST CENTRE MODULE                  no.     entries  %time %alloc   %time %alloc
    
    MAIN        MAIN                    102           0    0.0    0.0   100.0  100.0
     CAF        GHC.IO.Handle.FD        128           0    0.0    0.0     0.0    0.0
     CAF        GHC.IO.Encoding.Iconv   120           0    0.0    0.0     0.0    0.0
     CAF        GHC.Conc.Signal         110           0    0.0    0.0     0.0    0.0
     CAF        Main                    108           0    0.0    0.0   100.0  100.0
      main      Main                    204           1    0.0    0.0   100.0  100.0
       fib      Main                    205     2692537  100.0  100.0   100.0  100.0


  [1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html

## Cost Centers
Cost centers are annotations on a Haskell program which can be added automatically by the GHC compiler -- using `-fprot-auto` -- or by a programmer using `{-# SCC "name" #-} <expression>`, where "name" is any name you wish and `<expression>` is any valid Haskell expression:

    -- Main.hs
    main :: IO ()
    main = do let l = [1..9999999]
              print $ {-# SCC "print_list" #-} (length l)

Compiling with `-fprof` and running with `+RTS -p` e.g. `ghc -prof -rtsopts Main.hs && ./Main.hs +RTS -p` would produce `Main.prof` once the program's exited.

