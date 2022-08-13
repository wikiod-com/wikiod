---
title: "Reactive-banana"
slug: "reactive-banana"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Injecting external events into the library
This example is not tied to any concrete GUI toolkit, like reactive-banana-wx does, for instance. Instead it shows how to inject arbitary `IO` actions into FRP machinery.

The `Control.Event.Handler` module provides an `addHandler` function which creates a pair of `AddHandler a` and `a -> IO ()` values. The former is used by reactive-banana itself to obtain an `Event a` value, while the latter is a plain function that is used to trigger the corresponding event.

``` haskell
import Data.Char (toUpper)

import Control.Event.Handler
import Reactive.Banana

main = do
    (inputHandler, inputFire) <- newAddHandler
```

In our case the `a` parameter of the handler is of type `String`, but the code that lets compiler infer that will be written later.

Now we define the `EventNetwork` that describes our FRP-driven system. This is done using `compile` function:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        inputEvent <- fromAddHandler inputHandler
```

The `fromAddHandler` function transforms `AddHandler a` value into a `Event a`, which is covered in the next example.

Finally, we launch our "event loop", that would fire events on user input:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        ...
    forever $ do
        input <- getLine
        inputFire input
```

## Event type
In reactive-banana the `Event` type represents a stream of some events in time. An `Event` is similar to an analog impulse signal in the sense that it is not continuous in time. As a result, `Event` is an instance of the `Functor` typeclass only. You can't combine two `Event`s together because they may fire at different times. You can do something with an `Event`'s [current] value and react to it with some `IO` action.

Transformations on `Event`s value are done using `fmap`:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        inputEvent <- fromAddHandler inputHandler
        -- turn all characters in the signal to upper case
        let inputEvent' = fmap (map toUpper) inputEvent
```

Reacting to an `Event` is done the same way. First you `fmap` it with an action of type `a -> IO ()` and then pass it to `reactimate` function:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        inputEvent <- fromAddHandler inputHandler
        -- turn all characters in the signal to upper case
        let inputEvent' = fmap (map toUpper) inputEvent
        let inputEventReaction = fmap putStrLn inputEvent' -- this has type `Event (IO ())
        reactimate inputEventReaction
```

Now whenever `inputFire "something"` is called, `"SOMETHING"` would be printed.

## Behavior type
To represent continious signals, reactive-banana features `Behavior a` type. Unlike `Event`, a `Behavior` is an `Applicative`, which lets you combine n `Behavior`s using an n-ary pure function (using `<$>` and `<*>`).

To obtain a `Behavior a` from the `Event a` there is `accumE` function:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        ...
        inputBehavior <- accumE "" $ fmap (\oldValue newValue -> newValue) inputEvent
```

`accumE` takes `Behavior`'s initial value and an `Event`, containing a function that would set it to the new value.

As with `Event`s, you can use `fmap` to work with current `Behavior`s value, but you can also combine them with `(<*>)`.

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        ...
        inputBehavior  <- accumE "" $ fmap (\oldValue newValue -> newValue) inputEvent
        inputBehavior' <- accumE "" $ fmap (\oldValue newValue -> newValue) inputEvent
        let constantTrueBehavior = (==) <$> inputBehavior <*> inputBehavior'
```

To react on `Behavior` changes there is a `changes` function:

```haskell
main = do
    (inputHandler, inputFire) <- newAddHandler
    compile $ do
        ...
        inputBehavior  <- accumE "" $ fmap (\oldValue newValue -> newValue) inputEvent
        inputBehavior' <- accumE "" $ fmap (\oldValue newValue -> newValue) inputEvent
        let constantTrueBehavior = (==) <$> inputBehavior <*> inputBehavior'
        inputChanged <- changes inputBehavior
```

The only thing that should be noted is that `changes` return `Event (Future a)` instead of `Event a`. Because of this, `reactimate'` should be used instead of `reactimate`. The rationale behind this can be obtained from the documentation.

## Actuating EventNetworks
`EventNetwork`s returned by `compile` must be actuated before reactimated events  have an effect.

    main = do
        (inputHandler, inputFire) <- newAddHandler

        eventNetwork <- compile $ do
            inputEvent <- fromAddHandler inputHandler
            let inputEventReaction = fmap putStrLn inputEvent
            reactimate inputEventReaction
    
        inputFire "This will NOT be printed to the console!"
        actuate eventNetwork
        inputFire "This WILL be printed to the console!"

