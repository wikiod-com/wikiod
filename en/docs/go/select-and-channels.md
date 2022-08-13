---
title: "Select and Channels"
slug: "select-and-channels"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

The `select` keyword provides an easy method to work with channels and perform more advanced tasks. It is frequently used for a number of purposes:

 - Handling timeouts.
 - When there are multiple channels to read from, the select will randomly read from one channel which has data.
 - Providing an easy way to define what happens if no data is available on a channel. 

## Syntax
- select {}
- select { case true: }
- select { case incomingData := <-someChannel: }
- select { default: }


## Using select with timeouts
So here, I have removed the `for` loops, and made a **timeout** by adding a second `case` to the `select` that returns after 3 seconds. Because the `select` just waits until ANY case is true, the second `case` fires, and then our script ends, and `chatter()` never even gets a chance to finish.

```
// Use of the select statement with channels, for timeouts, etc.
package main

import (
    "fmt"
    "time"
)

// Function that is "chatty"
//Takes a single parameter a channel to send messages down
func chatter(chatChannel chan<- string) {
    // loop ten times and die
    time.Sleep(5 * time.Second) // sleep for 5 seconds
    chatChannel<- fmt.Sprintf("This is pass number %d of chatter", 1)
}

// out main function
func main() {
    // Create the channel, it will be taking only strings, no need for a buffer on this project
    chatChannel := make(chan string)
    // Clean up our channel when we are done
    defer close(chatChannel)

    // start a go routine with chatter (separate, no blocking)
    go chatter(chatChannel)

    // select statement will block this thread until one of the two conditions below is met
    // because we have a default, we will hit default any time the chatter isn't chatting
    select {
    // anytime the chatter chats, we'll catch it and output it
    case spam := <-chatChannel:
        fmt.Println(spam)
    // if the chatter takes more than 3 seconds to chat, stop waiting
    case <-time.After(3 * time.Second):
        fmt.Println("Ain't no time for that!")
    }
}
```

## Simple Select Working with Channels
In this example we create a goroutine (a function running in a separate thread) that accepts a `chan` parameter, and simply loops, sending information into the channel each time.

In the `main` we have a `for` loop and a `select`. The `select` will block processing until one of the `case` statements becomes true. Here we have declared two cases; the first is when information comes through the channel, and the other is if no other case occurs, which is known as `default`.

    // Use of the select statement with channels (no timeouts)
    package main
    
    import (
        "fmt"
        "time"
    )
    
    // Function that is "chatty"
    // Takes a single parameter a channel to send messages down
    func chatter(chatChannel chan<- string) {
        // Clean up our channel when we are done.
        // The channel writer should always be the one to close a channel.
        defer close(chatChannel)
    
        // loop five times and die
        for i := 1; i <= 5; i++ {
            time.Sleep(2 * time.Second) // sleep for 2 seconds
            chatChannel <- fmt.Sprintf("This is pass number %d of chatter", i)
        }
    }
    
    // Our main function
    func main() {
        // Create the channel
        chatChannel := make(chan string, 1)
    
        // start a go routine with chatter (separate, non blocking)
        go chatter(chatChannel)
    
        // This for loop keeps things going while the chatter is sleeping
        for {
            // select statement will block this thread until one of the two conditions below is met
            // because we have a default, we will hit default any time the chatter isn't chatting
            select {
            // anytime the chatter chats, we'll catch it and output it
            case spam, ok := <-chatChannel:
                // Print the string from the channel, unless the channel is closed
                // and we're out of data, in which case exit.
                if ok {
                    fmt.Println(spam)
                } else {
                    fmt.Println("Channel closed, exiting!")
                    return
                }
            default:
                // print a line, then sleep for 1 second.
                fmt.Println("Nothing happened this second.")
                time.Sleep(1 * time.Second)
            }
        }
    }

[Try it on the Go Playground!](https://play.golang.org/p/jMeu32yIUJ)

