---
title: "core.async"
slug: "coreasync"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## basic channel operations: creating, putting, taking, closing, and buffers.
`core.async` is about making *processes* that take values from and put values into *channels*. 

    (require [clojure.core.async :as a])

## Creating channels with `chan`
You create a channel using the `chan` function:

    (def chan-0 (a/chan)) ;; unbuffered channel: acts as a rendez-vous point.
    (def chan-1 (a/chan 3)) ;; channel with a buffer of size 3. 
    (def chan-2 (a/chan (a/dropping-buffer 3)) ;; channel with a *dropping* buffer of size 3
    (def chan-3 (a/chan (a/sliding-buffer 3)) ;; channel with a *sliding* buffer of size 3

## Putting values into channels with `>!!` and `>!`

You put values into a channel with `>!!`:

    (a/>!! my-channel :an-item)

You can put any value (Strings, numbers, maps, collections, objects, even other channels, etc.) into a channel, except `nil`:

    ;; WON'T WORK
    (a/>!! my-channel nil)
    => IllegalArgumentException Can't put nil on channel

Depending on the channel's buffer, `>!!` may block the current thread.

    (let [ch (a/chan)] ;; unbuffered channel
      (a/>!! ch :item) 
      ;; the above call blocks, until another process 
      ;; takes the item from the channel.
      )
    (let [ch (a/chan 3)] ;; channel with 3-size buffer
      (a/>!! ch :item-1) ;; => true
      (a/>!! ch :item-2) ;; => true
      (a/>!! ch :item-3) ;; => true
      (a/>!! ch :item-4) 
      ;; now the buffer is full; blocks until :item-1 is taken from ch.
      )

From inside a `(go ...)` block, you can - and should - use `a/>!` instead of `a/>!!`:

     (a/go (a/>! ch :item))

The logical behaviour will be the same as `a/>!!`, but only the logical process of the goroutine will block instead of the actual OS thread.

Using `a/>!!` inside of a `(go ...)` block is an anti-pattern:

    ;; NEVER DO THIS
    (a/go 
      (a/>!! ch :item))

## Taking values from channels with `<!!`

You take a value from a channel using `<!!`:

    ;; creating a channel
    (def ch (a/chan 3))
    ;; putting some items in it
    (do 
      (a/>!! ch :item-1)
      (a/>!! ch :item-2)
      (a/>!! ch :item-3))
    ;; taking a value
    (a/<!! ch) ;; => :item-1
    (a/<!! ch) ;; => :item-2

If no item is available in the channel, `a/<!!` will block the current Thread until a value is put in the channel (or the channel is closed, see later):

    (def ch (a/chan))
    (a/<!! ch) ;; blocks until another process puts something into ch or closes it

From inside a `(go ...)` block, you can - and should - use `a/<!` instead of `a/<!!`:

     (a/go (let [x (a/<! ch)] ...))

The logical behaviour will be the same as `a/<!!`, but only the logical process of the goroutine will block instead of the actual OS thread.

Using `a/<!!` inside of a `(go ...)` block is an anti-pattern:

    ;; NEVER DO THIS
    (a/go 
      (a/<!! ch))


## Closing channels

You *close* a channel with `a/close!`:

    (a/close! ch)

Once a channel is closed, and the all data in the channel has been exhausted, takes will always return `nil`:

    (def ch (a/chan 5))

    ;; putting 2 values in the channel, then closing it
    (a/>!! ch :item-1)
    (a/>!! ch :item-2)
    (a/close! ch)

    ;; taking from ch will return the items that were put in it, then nil
    (a/<!! ch) ;; => :item-1
    (a/<!! ch) ;; => :item-2
    (a/<!! ch) ;; => nil
    (a/<!! ch) ;; => nil
    (a/<!! ch) ;; => nil

    ;; once the channel is closed, >!! will have no effect on the channel:
    (a/>!! ch :item-3)
    => false ;; false means the put did not succeed
    (a/<!! ch) ;; => nil

## Asynchronous puts with `put!`

As an alternative to `a/>!!` (which may block), you can call `a/put!` to put a value in a channel in another thread, with an optional callback. 

    (a/put! ch :item)
    (a/put! ch :item (fn once-put [closed?] ...)) ;; callback function, will receive 

In ClojureScript, since blocking the current Thread is not possible, `a/>!!` is not supported, and `put!` is the only way to put data into a channel from outside of a `(go)` block.

## Asynchronous takes with `take!`

As an alternative to `a/<!!` (which may block the current thread), you may use `a/take!` to take a value from a channel asynchronously, passing it to a callback.

    (a/take! ch (fn [x] (do something with x)))

## Using dropping and sliding buffers

With dropping and sliding buffers, puts never block, however, when the buffer is full, you lose data. Dropping buffer lose the last data added, whereas sliding buffers lose the first data added.


**Dropping buffer example:**

    (def ch (a/chan (a/dropping-buffer 2)))

    ;; putting more items than buffer size
    (a/>!! ch :item-1)
    => true ;; put succeeded
    (a/>!! ch :item-2)
    => true
    (a/>!! ch :item-3)
    => false ;; put failed

    ;; no we take from the channel
    (a/<!! ch)
    => :item-1
    (a/<!! ch)
    => :item-2
    (a/<!! ch)
    ;; blocks! :item-3 is lost


**Sliding buffer example:**

    (def ch (a/chan (a/sliding-buffer 2)))

    ;; putting more items than buffer size
    (a/>!! ch :item-1)
    => true
    (a/>!! ch :item-2)
    => true
    (a/>!! ch :item-3)
    => true

    ;; no when we take from the channel:
    (a/<!! ch)
    => :item-2
    (a/<!! ch)
    => :item-3
    ;; :item-1 was lost


