---
title: "Mailbox Processor"
slug: "mailbox-processor"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

`MailboxProcessor` maintains an internal message queue, where multiple producers can post messages using various `Post` method variants. These messages are then retrieved and processed by a single consumer (unless you implement it otherwise) using `Retrieve` and `Scan` variants. By default both producing and consuming the messages is thread-safe.

By default there is no provided error handling. If an uncaught exception is thrown inside the processor's body, the body function will end, all messages in the queue will be lost, no more messages can be posted and the reply channel (if available) will get an exception instead of a response. You have to provide all error handling yourself in case this behavior does not suit your use case.

## Basic Hello World
Let's first create a simple "Hello world!" `MailboxProcessor` which processes one type of message and prints greetings.

You'll need the message type. It can be anything, but [Discriminated Unions](https://www.wikiod.com/docs/f%23/1025/discriminated-unions#t=201703110747364524357) are a natural choice here as they list all the possible cases on one place and you can easily use pattern matching when processing them.

    // In this example there is only a single case, it could technically be just a string
    type GreeterMessage = SayHelloTo of string

Now define the processor itself. This can be done with `MailboxProcessor<'message>.Start` static method which returns a started processor ready to do its job. You can also use the constructor, but then you need to make sure to start the processor later.

    let processor = MailboxProcessor<GreeterMessage>.Start(fun inbox ->
        let rec innerLoop () = async {
            // This way you retrieve message from the mailbox queue
            // or await them in case the queue empty.
            // You can think of the `inbox` parameter as a reference to self.
            let! message = inbox.Receive()
            // Now you can process the retrieved message.
            match message with
            | SayHelloTo name ->
                printfn "Hi, %s! This is mailbox processor's inner loop!" name
            // After that's done, don't forget to recurse so you can process the next messages!
            innerLoop()
        }
        innerLoop ())

The parameter to `Start` is a function which takes a reference to the `MailboxProcessor` itself (which doesn't exist yet as you are just creating it, but will be available once the function executes). That gives you access to its various `Receive` and `Scan` methods to access the messages from the mailbox. Inside this function, you can do whatever processing you need, but a usual approach is an infinite loop that reads the messages one by one and calls itself after each one.

Now the processor is ready, but it doesn't to anything! Why? You need to send it a message to process. This is done with the `Post` method variants - let's use the most basic, fire-and-forget one.

    processor.Post(SayHelloTo "Alice")

This puts a message to `processor`'s internal queue, the mailbox, and immediately returns so that the calling code can continue. Once the processor retrieves the message, it will process it, but that will be done asynchronously to posting it, and it will be most likely done on a separate thread.

Very soon afterwards you should see the message `"Hi, Alice! This is mailbox processor's inner loop!"` printed to the output and you're ready for more complicated samples.

## Mutable State Management
Mailbox processors can be used to manage mutable state in a transparent and thread-safe way. Let's build a simple counter.

    // Increment or decrement by one.
    type CounterMessage =
        | Increment
        | Decrement

    let createProcessor initialState =
        MailboxProcessor<CounterMessage>.Start(fun inbox ->
            // You can represent the processor's internal mutable state
            // as an immutable parameter to the inner loop function
            let rec innerLoop state = async {
                printfn "Waiting for message, the current state is: %i" state
                let! message = inbox.Receive()
                // In each call you use the current state to produce a new
                // value, which will be passed to the next call, so that
                // next message sees only the new value as its local state
                match message with
                | Increment ->
                    let state' = state + 1
                    printfn "Counter incremented, the new state is: %i" state'
                    innerLoop state'
                | Decrement ->
                    let state' = state - 1
                    printfn "Counter decremented, the new state is: %i" state'
                    innerLoop state'
            }
            // We pass the initialState to the first call to innerLoop
            innerLoop initialState)
    
    // Let's pick an initial value and create the processor
    let processor = createProcessor 10

Now let's generate some operations

    processor.Post(Increment)
    processor.Post(Increment)
    processor.Post(Decrement)
    processor.Post(Increment)

And you will see the following log

    Waiting for message, the current state is: 10
    Counter incremented, the new state is: 11
    Waiting for message, the current state is: 11
    Counter incremented, the new state is: 12
    Waiting for message, the current state is: 12
    Counter decremented, the new state is: 11
    Waiting for message, the current state is: 11
    Counter incremented, the new state is: 12
    Waiting for message, the current state is: 12

## Concurrency

Since mailbox processor processes the messages one by one and there is no interleaving, you can also produce the messages from multiple threads and you will not see the the typical problems of lost or duplicated operations. There is no way for a message to use the old state of other messages, unless you specifically implement the processor so.

    let processor = createProcessor 0

    [ async { processor.Post(Increment) }
      async { processor.Post(Increment) }
      async { processor.Post(Decrement) }
      async { processor.Post(Decrement) } ]
    |> Async.Parallel
    |> Async.RunSynchronously

All messages are posted from different threads. The order in which messages are posted to the mailbox is not deterministic, so the order of processing them is not deterministic, but since the overall number of increments and decrements is balanced, you will see the final state being 0, no matter in what order and from which threads the messages were sent.

## True mutable state

In the previous example we've only simulated mutable state by passing the recursive loop parameter, but mailbox processor has all these properties even for a truly mutable state. This is important when you maintain large state and immutability is impractical for performance reasons.

We can rewrite our counter to the following implementation

    let createProcessor initialState =
        MailboxProcessor<CounterMessage>.Start(fun inbox ->
            // In this case we represent the state as a mutable binding
            // local to this function. innerLoop will close over it and
            // change its value in each iteration instead of passing it around
            let mutable state = initialState

            let rec innerLoop () = async {
                printfn "Waiting for message, the current state is: %i" state
                let! message = inbox.Receive()
                match message with
                | Increment ->
                    let state <- state + 1
                    printfn "Counter incremented, the new state is: %i" state'
                    innerLoop ()
                | Decrement ->
                    let state <- state - 1
                    printfn "Counter decremented, the new state is: %i" state'
                    innerLoop ()
            }
            innerLoop ())

Even though this would definitely not be thread safe if the counter state was modified directly from multiple threads, you can see by using the parallel message Posts from previous section that mailbox processor processes the messages one after another with no interleaving, so each message uses the most current value.

## Return Values
You can asynchronously return a value for each processed message if you send an `AsyncReplyChannel<'a>` as part of the message.

    type MessageWithResponse = Message of InputData * AsyncReplyChannel<OutputData>

Then the mailbox processor can use this channel when processing the message to send a value back to the caller.

    let! message = inbox.Receive()
    match message with
    | MessageWithResponse (data, r) ->
        // ...process the data
        let output = ...
        r.Reply(output)

Now to create a message, you need the `AsyncReplyChannel<'a>` - what is is and how do you create a working instance? The best way is to let MailboxProcessor provide it for you and extract the response to a more common `Async<'a>`. This can be done by using for example the `PostAndAsynReply` method, where you don't post the complete message, but instead a function of type (in our case) `AsyncReplyChannel<OutputData> -> MessageWithResponse`:

    let! output = processor.PostAndAsyncReply(r -> MessageWithResponse(input, r))

This will post the message in a queue and await the reply, which will arrive once the processor gets to this message and replies using the channel.

There is also a synchronous variant `PostAndReply` which blocks the calling thread until the processor replies.

## Out-of-Order Message Processing
You can use `Scan` or `TryScan` methods to look for specific messages in the queue and process them regardless of how many messages are before them. Both methods look at the messages in the queue in the order they arrived and will look for a specified message (up until optional timeout). In case there is no such message, `TryScan` will return None, while `Scan` will keep waiting until such message arrives or the operation times out.

Let's see it in practice. We want the processor to process `RegularOperations` when it can, but whenever there is a `PriorityOperation`, it should be processed as soon as possible, no matter how many other `RegularOperations` are in the queue.

    type Message =
        | RegularOperation of string
        | PriorityOperation of string

    let processor = MailboxProcessor<Message>.Start(fun inbox ->
        let rec innerLoop () = async {
            let! priorityData = inbox.TryScan(fun msg ->
                // If there is a PriorityOperation, retrieve its data.
                match msg with
                | PriorityOperation data -> Some data
                | _ -> None)

            match priorityData with
            | Some data ->
                // Process the data from PriorityOperation.
            | None ->
                // No PriorityOperation was in the queue at the time, so
                // let's fall back to processing all possible messages
                let! message = inbox.Receive()
                match message with
                | RegularOperation data ->
                    // We have free time, let's process the RegularOperation.
                | PriorityOperation data ->
                    // We did scan the queue, but it might have been empty
                    // so it is possible that in the meantime a producer
                    // posted a new message and it is a PriorityOperation.
            // And never forget to process next messages.
            innerLoop ()
        }
        innerLoop())


