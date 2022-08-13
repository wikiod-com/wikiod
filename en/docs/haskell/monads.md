---
title: "Monads"
slug: "monads"
draft: false
images: []
weight: 9870
type: docs
toc: true
---

A monad is a data type of composable actions. `Monad` is the class of type constructors whose values represent such actions. Perhaps `IO` is the most recognizable one: a value of `IO a` is a "recipe for retrieving an `a` value from the real world".

We say a type constructor `m` (such as `[]` or `Maybe`) *forms a monad* if there is an `instance Monad m` satisfying certain laws about composition of actions. We can then reason about `m a` as an "action whose result has type `a`".

## No general way to extract value from a monadic computation
You can wrap values into actions and pipe the result of one computation into another:

    return :: Monad m => a -> m a
    (>>=)  :: Monad m => m a -> (a -> m b) -> m b

However, the definition of a Monad doesn’t guarantee the existence of a function of type `Monad m => m a -> a`.

That means there is, in general, **no way to extract a value from a computation** (i.e. “unwrap” it). This is the case for many instances:

```
extract :: Maybe a -> a
extract (Just x) = x          -- Sure, this works, but...
extract Nothing  = undefined  -- We can’t extract a value from failure.
```

Specifically, there is no function `IO a -> a`, which often confuses beginners; see [this example](https://www.wikiod.com/haskell/monads#IO monad).

## Monad as a Subclass of Applicative
As of GHC 7.10, `Applicative` is a superclass of `Monad` (i.e., every type which is a `Monad` must also be an `Applicative`). All the methods of `Applicative` (`pure`, `<*>`) can be implemented in terms of  methods of `Monad` (`return`, `>>=`). 

It is obvious that `pure` and `return` serve equivalent purposes, so `pure = return`. The definition for `<*>` is too relatively clear: 

    mf <*> mx = do { f <- mf; x <- mx; return (f x) }                 
           -- = mf >>= (\f -> mx >>= (\x -> return (f x)))
           -- = [r   | f <- mf, x <- mx, r <- return (f x)]   -- with MonadComprehensions
           -- = [f x | f <- mf, x <- mx]                   

This function is defined as `ap` in the standard libraries.

Thus if you have already defined an instance of `Monad` for a type, you effectively can get an instance of `Applicative` for it "for free" by defining

    instance Applicative < type > where
        pure  = return
        (<*>) = ap

As with the monad laws, these equivalencies are not enforced, but developers should ensure that they are always upheld.

## Definition of Monad
    class Monad m where
        return :: a -> m a
        (>>=) :: m a -> (a -> m b) -> m b

The most important function for dealing with monads is the **bind operator `>>=`**:

    (>>=) :: m a -> (a -> m b) -> m b

* Think of `m a` as *"an action with an `a` result"*.
* Think of `a -> m b` as *“an action (depending on an `a` parameter) with a `b` result.”*.

`>>=` **sequences two actions together by piping the result from the first action to the second.**

The other function defined by `Monad` is:

    return :: a -> m a

Its name is unfortunate: this `return` has nothing to do with the `return` keyword found in imperative programming languages.

`return x` **is the trivial action yielding `x` as its result.** (It is trivial in the [following sense](https://www.wikiod.com/haskell/monads#The Maybe monad):)

    return x >>= f       ≡  f x     --  “left identity” monad law
           x >>= return  ≡  x       -- “right identity” monad law

## The Maybe monad
`Maybe` is used to represent possibly empty values - similar to `null` in other languages. Usually it is used as the output type of functions that can fail in some way.

Consider the following function:

    halve :: Int -> Maybe Int
    halve x
      | even x = Just (x `div` 2)
      | odd x  = Nothing

Think of `halve` as an action, depending on an `Int`, that tries to halve the integer, failing if it is odd.

How do we `halve` an integer three times?

    takeOneEighth :: Int -> Maybe Int            -- (after you read the 'do' sub-section:)
    takeOneEighth x =                
      case halve x of                               --  do {
        Nothing -> Nothing
        Just oneHalf ->                             --     oneHalf    <- halve x
          case halve oneHalf of
            Nothing -> Nothing
            Just oneQuarter ->                      --     oneQuarter <- halve oneHalf
              case halve oneQuarter of
                Nothing -> Nothing                  --     oneEighth  <- halve oneQuarter
                Just oneEighth ->                         
                  Just oneEighth                    --     return oneEighth }


* `takeOneEighth` is a *sequence* of three `halve` steps chained together.
* If a `halve` step fails, we want the whole composition `takeOneEighth` to fail.
* If a `halve` step succeeds, we want to pipe its result forward.
<p></p>

    instance Monad Maybe where
      -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
      Nothing >>= f  = Nothing                            -- infixl 1 >>=
      Just x  >>= f  = Just (f x)                         -- also, f =<< m = m >>= f
      
      -- return :: a -> Maybe a
      return x       = Just x

and now we can write:

    takeOneEighth :: Int -> Maybe Int
    takeOneEighth x = halve x >>= halve >>= halve             -- or,
        -- return x >>= halve >>= halve >>= halve             -- which is parsed as
        -- (((return x) >>= halve) >>= halve) >>= halve       -- which can also be written as
        -- (halve =<<) . (halve =<<) . (halve =<<) $ return x    -- or, equivalently, as
        --  halve <=<     halve <=<     halve      $        x

*Kleisli composition* `<=<` is defined as `(g <=< f) x = g =<< f x`, or equivalently as `(f >=> g) x = f x >>= g`. With it the above definition becomes just

    takeOneEighth :: Int -> Maybe Int
    takeOneEighth = halve <=< halve <=< halve               -- infixr 1 <=<
            -- or, equivalently,                    
            --      halve >=> halve >=> halve               -- infixr 1 >=>    

----------


There are three monad laws that should be obeyed by every monad, that is every type which is an instance of the `Monad` typeclass:

    1.  return x >>= f  =  f x
    2.    m >>= return  =  m
    3. (m >>= g) >>= h  =  m >>= (\y -> g y >>= h)

where `m` is a monad, `f` has type `a -> m b` and `g` has type `b -> m c`.

Or equivalently, using the `>=>` Kleisli composition operator defined above:

    1.    return >=> g  =  g                    -- do { y <- return x ; g y } == g x
    2.    f >=> return  =  f                    -- do { y <- f x ; return y } == f x
    3. (f >=> g) >=> h  =  f >=> (g >=> h)      -- do { z <- do { y <- f x; g y } ; h z }
                                                --  == do { y <- f x ; do { z <- g y; h z } }


Obeying these laws makes it a lot easier to reason about the monad, because it guarantees that using monadic functions and composing them behaves in a reasonable way, similar to other monads.

Let's check if the `Maybe` monad obeys the three monad laws.

1. **The left identity law** - `return x >>= f = f x`

    
    return z >>= f 
    = (Just z) >>= f 
    = f z

2. **The right identity law** - `m >>= return = m`

- `Just` data constructor


    Just z >>= return
    = return z
    = Just z  

- `Nothing` data constructor


    Nothing >>= return
    = Nothing 

3. **The associativity law** - `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`

- `Just` data constructor

    
    -- Left-hand side
    ((Just z) >>= f) >>= g
    = f z >>= g
    
    -- Right-hand side
    (Just z) >>= (\x -> f x >>= g)
    (\x -> f x >>= g) z
    = f z >>= g

- `Nothing` data constructor


    -- Left-hand side
    (Nothing >>= f) >>= g
    = Nothing >>= g
    = Nothing

    -- Right-hand side
    Nothing >>= (\x -> f x >>= g)
    = Nothing

## IO monad
There is no way to get a value of type `a` out of an expression of type `IO a` and there shouldn't be. This is actually a large part of why monads are used to model `IO`.

An expression of type `IO a` can be thought of as representing an action that can interact with the real world and, if executed, would result in something of type `a`. For example, the function `getLine :: IO String` from the prelude doesn't mean that underneath `getLine` there is some specific string that I can extract - it means that `getLine` represents the action of getting a line from standard input. 

Not surprisingly, `main :: IO ()` since a Haskell program does represent a computation/action that interacts with the real world.

The things you _can_ do to expressions of type `IO a` because `IO` is a monad:

 - Sequence two actions using `(>>)` to produce a new action that executes the first action, discards whatever value it produced, and then executes the second action. 

         -- print the lines "Hello" then "World" to stdout
         putStrLn "Hello" >> putStrLn "World"

 - Sometimes you don't want to discard the value that was produced in the first action - you'd actually like it to be fed into a second action. For that, we have `>>=`. For `IO`, it has type `(>>=) :: IO a -> (a -> IO b) -> IO b`.

        -- get a line from stdin and print it back out
        getLine >>= putStrLn

 - Take a normal value and convert it into an action which just immediately returns the value you gave it. This function is less obviously useful until you start using `do` notation.

        -- make an action that just returns 5
        return 5
        
More from the Haskell Wiki on the IO monad [here][1].

[1]: https://wiki.haskell.org/IO_inside

## List Monad
The lists form a monad. They have a monad instantiation equivalent to this one:
``` Haskell
instance Monad [] where 
  return x = [x]
  xs >>= f = concat (map f xs)               
```

We can use them to emulate non-determinism in our computations. When we use `xs >>= f`, the function `f :: a -> [b]` is mapped over the list `xs`, obtaining a list of lists of results of each application of `f` over each element of `xs`, and all the lists of results are then concatenated into one list of all the results. As an example, we compute a sum of two non-deterministic numbers using **do-notation**, the sum being represented by list of sums of all pairs of integers from two lists, each list representing all possible values of a non-deterministic number:
```
sumnd xs ys = do
  x <- xs
  y <- ys
  return (x + y)
```
Or equivalently, using `liftM2` in `Control.Monad`:
```
sumnd = liftM2 (+)
```
we obtain:
```
> sumnd [1,2,3] [0,10]
[1,11,2,12,3,13]
```

## do-notation
`do`-notation is syntactic sugar for monads. Here are the rules:

<pre>
do x &lt;- mx                               do x &lt;- mx
   y &lt;- my       is equivalent to           do y &lt;- my
   ...                                         ...
</pre>
<pre>
do let a = b                             let a = b in
   ...           is equivalent to          do ...
</pre>
<pre>
do m                                     m >> (
   e             is equivalent to          e)
</pre>
<pre>
do x &lt;- m                                m >>= (\x ->
   e             is equivalent to          e)
</pre>
<pre>
do m             is equivalent to        m
</pre>

For example, these definitions are equivalent:

    example :: IO Integer
    example =
      putStrLn "What's your name?" >> (
        getLine >>= (\name ->
          putStrLn ("Hello, " ++ name ++ ".") >> (
            putStrLn "What should we return?" >> (
              getLine >>= (\line ->
                let n = (read line :: Integer) in
                  return (n + n))))))

---

    example :: IO Integer
    example = do
      putStrLn "What's your name?"
      name <- getLine
      putStrLn ("Hello, " ++ name ++ ".")
      putStrLn "What should we return?"
      line <- getLine
      let n = (read line :: Integer)
      return (n + n)



