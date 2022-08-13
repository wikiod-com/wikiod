---
title: "Free Monads"
slug: "free-monads"
draft: false
images: []
weight: 9847
type: docs
toc: true
---

## Free monads split monadic computations into data structures and interpreters
For instance, a computation involving commands to read and write from the prompt:

First we describe the "commands" of our computation as a Functor data type

```
{-# LANGUAGE DeriveFunctor #-}

data TeletypeF next
    = PrintLine String next
    | ReadLine (String -> next)
    deriving Functor
```

Then we use `Free` to create the "Free Monad over `TeletypeF`" and build some basic operations.

```
import Control.Monad.Free (Free, liftF, iterM)

type Teletype = Free TeletypeF

printLine :: String -> Teletype ()
printLine str = liftF (PrintLine str ())

readLine :: Teletype String
readLine = liftF (ReadLine id)
```

Since `Free f` is a `Monad` whenever `f` is a `Functor`, we can use the standard `Monad` combinators (including `do` notation) to build `Teletype` computations.

```
import Control.Monad -- we can use the standard combinators

echo :: Teletype ()
echo = readLine >>= printLine

mockingbird :: Teletype a
mockingbird = forever echo
```

Finally, we write an "interpreter" turning `Teletype a` values into something we know how to work with like `IO a`

```
interpretTeletype :: Teletype a -> IO a
interpretTeletype = foldFree run where
  run :: TeletypeF a -> IO a
  run (PrintLine str x) = putStrLn *> return x
  run (ReadLine f) = fmap f getLine
```

Which we can use to "run" the `Teletype a` computation in `IO`

```
> interpretTeletype mockingbird
hello
hello
goodbye
goodbye
this will go on forever
this will go on forever
```

## The Freer monad
There's an alternative formulation of the free monad called the Freer (or Prompt, or Operational) monad. The Freer monad doesn't require a Functor instance for its underlying instruction set, and it has a more recognisably list-like structure than the standard free monad.

The Freer monad represents programs as a sequence of atomic _instructions_ belonging to the instruction set `i :: * -> *`. Each instruction uses its parameter to declare its return type. For example, the set of base instructions for the `State` monad are as follows:

    data StateI s a where
        Get :: StateI s s  -- the Get instruction returns a value of type 's'
        Put :: s -> StateI s ()  -- the Put instruction contains an 's' as an argument and returns ()

Sequencing these instructions takes place with the `:>>=` constructor. `:>>=` takes a single instruction returning an `a` and prepends it to the rest of the program, piping its return value into the continuation. In other words, given an instruction returning an `a`, and a function to turn an `a` into a program returning a `b`, `:>>=` will produce a program returning a `b`.

    data Freer i a where
        Return :: a -> Freer i a
        (:>>=) :: i a -> (a -> Freer i b) -> Freer i b

Note that `a` is existentially quantified in the `:>>=` constructor. The only way for an interpreter to learn what `a` is is by pattern matching on the GADT `i`.

> **Aside**: The co-Yoneda lemma tells us that `Freer` is isomorphic to `Free`.
> Recall the definition of the `CoYoneda` functor:
> 
>     data CoYoneda i b where
>       CoYoneda :: i a -> (a -> b) -> CoYoneda i b
> 
> `Freer i` is equivalent to `Free (CoYoneda i)`. If you take [`Free`'s constructors](https://www.wikiod.com/haskell/free-monads#Free Monads are like fixed points) and set `f ~ CoYoneda i`, you get:
>     
>     Pure :: a -> Free (CoYoneda i) a
>     Free :: CoYoneda i (Free (CoYoneda i) b) -> Free (CoYonda i) b ~
>             i a -> (a -> Free (CoYoneda i) b) -> Free (CoYoneda i) b
> 
> from which we can recover `Freer i`'s constructors by just setting
> `Freer i ~ Free (CoYoneda i)`.

Because `CoYoneda i` is a `Functor` for any `i`, `Freer` is a `Monad` for any `i`, even if `i` isn't a `Functor`.

    instance Monad (Freer i) where
        return = Return
        Return x >>= f = f x
        (i :>>= g) >>= f = i :>>= fmap (>>= f) g  -- using `(->) r`'s instance of Functor, so fmap = (.)

Interpreters can be built for `Freer` by mapping instructions to some handler monad.

    foldFreer :: Monad m => (forall x. i x -> m x) -> Freer i a -> m a
    foldFreer eta (Return x) = return x
    foldFreer eta (i :>>= f) = eta i >>= (foldFreer eta . f)

For example, we can interpret the `Freer (StateI s)` monad using the regular `State s` monad as a handler:

    runFreerState :: Freer (StateI s) a -> s -> (a, s)
    runFreerState = State.runState . foldFreer toState
        where toState :: StateI s a -> State s a
              toState Get = State.get
              toState (Put x) = State.put x

## Free Monads are like fixed points
Compare the definition of `Free` to that of [`Fix`](https://www.wikiod.com/haskell/recursion-schemes#Fixed points):

    data Free f a = Return a
                  | Free (f (Free f a))

    newtype Fix f = Fix { unFix :: f (Fix f) }

In particular, compare the type of the `Free` constructor with the type of the `Fix` constructor. `Free` layers up a functor just like `Fix`, except that `Free` has an additional `Return a` case.

## How do foldFree and iterM work?
There are some functions to help tear down `Free` computations by interpreting them into another monad `m`: `iterM :: (Functor f, Monad m) => (f (m a) -> m a) -> (Free f a -> m a)` and `foldFree :: Monad m => (forall x. f x -> m x) -> (Free f a -> m a)`. What are they doing?

First let's see what it would take to tear down an interpret a `Teletype a` function into `IO` manually. We can see `Free f a` as being defined

```
data Free f a 
    = Pure a 
    | Free (f (Free f a))
```

The `Pure` case is easy:

```
interpretTeletype :: Teletype a -> IO a
interpretTeletype (Pure x) = return x
interpretTeletype (Free teletypeF) = _
```

Now, how to interpret a `Teletype` computation that was built with the `Free` constructor? We'd like to arrive at a value of type `IO a` by examining `teletypeF :: TeletypeF (Teletype a)`. To start with, we'll write a function `runIO :: TeletypeF a -> IO a` which maps a single layer of the free monad to an `IO` action:

    runIO :: TeletypeF a -> IO a
    runIO (PrintLine msg x) = putStrLn msg *> return x
    runIO (ReadLine k) = fmap k getLine

Now we can use `runIO` to fill in the rest of `interpretTeletype`. Recall that `teletypeF :: TeletypeF (Teletype a)` is a layer of the `TeletypeF` functor which contains the rest of the `Free` computation. We'll use `runIO` to interpret the outermost layer (so we have `runIO teletypeF :: IO (Teletype a)`) and then use the `IO` monad's `>>=` combinator to interpret the returned `Teletype a`.

    interpretTeletype :: Teletype a -> IO a
    interpretTeletype (Pure x) = return x
    interpretTeletype (Free teletypeF) = runIO teletypeF >>= interpretTeletype

The definition of `foldFree` is just that of `interpretTeletype`, except that the `runIO` function has been factored out. As a result, `foldFree` works independently of any particular base functor and of any target monad.

    foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
    foldFree eta (Pure x) = return x
    foldFree eta (Free fa) = eta fa >>= foldFree eta

`foldFree` has a rank-2 type: `eta` is a natural transformation. We could have given `foldFree` a type of `Monad m => (f (Free f a) -> m (Free f a)) -> Free f a -> m a`, but that gives `eta` the liberty of inspecting the `Free` computation inside the `f` layer. Giving `foldFree` this more restrictive type ensures that `eta` can only process a single layer at a time.

`iterM` does give the folding function the ability to examine the subcomputation. The (monadic) result of the previous iteration is available to the next, inside `f`'s parameter. `iterM` is analogous to a [_paramorphism_](https://www.wikiod.com/haskell/recursion-schemes#Primitive recursion) whereas `foldFree` is like a [_catamorphism_](https://www.wikiod.com/haskell/recursion-schemes#Folding up a structure one layer at a time).

```
iterM :: (Monad m, Functor f) => (f (m a) -> m a) -> Free f a -> m a
iterM phi (Pure x) = return x
iterM phi (Free fa) = phi (fmap (iterM phi) fa)
```

