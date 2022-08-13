---
title: "Protocols and Delegates"
slug: "protocols-and-delegates"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

*Protocols* and *Delegates* are two related but different concept:

A *Protocol* is a interface a class can conforms to, meaning that class implements the listed methods.

A *Delegate* is typically an anonymous object that conforms to a protocol.

The application of *Delegate* called *Delegation* is a design pattern.

At one end we have the concept of *Inheritance* which creates a tight coupling between the subclass and its superclass whereas *Delegation* design pattern provides an alternative to avoid this tight coupling using which we can create a much looser relationship based on anonymous *Delegate* objects.

## Implementation of Protocols and Delegation mechanism.
Suppose you have two views `ViewA` and `ViewB`

Instance of `ViewB` is created inside `ViewA`, so `ViewA` can send message to `ViewB's` instance, but for the reverse to happen we need to implement delegation (so that using delegate `ViewB's` instance could send message to `ViewA`)

Follow these steps to implement the delegation

1. In `ViewB` create protocol as

        @protocol ViewBDelegate 
    
       -(void) exampleDelegateMethod;
    
        @end

2. Declare the delegate in the sender class

        @interface ViewB : UIView
        @property (nonatomic, weak) id< ViewBDelegate > delegate;
        @end

3. Adopt the protocol in Class ViewA

   `@interfac ViewA: UIView < ViewBDelegate >`

4. Set the delegate
    
       -(void) anyFunction   
       {
           // create Class ViewB's instance and set the delegate
           [viewB setDelegate:self];
       }

5. Implement the delegate method in class `ViewA`

       -(void) exampleDelegateMethod
       {
           // will be called by Class ViewB's instance
       }

6. Use the method in class `ViewB` to call the delegate method as

       -(void) callDelegateMethod
       {
           [delegate exampleDelegateMethod];
           //assuming the delegate is assigned otherwise error
       }

