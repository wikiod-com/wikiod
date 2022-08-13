---
title: "Grand Central Dispatch"
slug: "grand-central-dispatch"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

**Grand Central Dispatch (GCD)**


In iOS, Apple provides two ways to do multitasking: The Grand Central Dispatch (GCD) and NSOperationQueue frameworks.We will
discuss here about GCD.


GCD is a lightweight way to represent units of work that are going to be executed concurrently
You don’t schedule these units of work; the system takes care of scheduling for you. Adding dependency among blocks can be a headache. Canceling or suspending a block creates extra work for you as a developer!

## What is Grand central dispatch.
**What is Concurrency?**

    

 - Doing multiple things at the same time.

   

 - Taking advantage of number of cores available in multicore CPUs.

  

 - Running multiple programs in parallel.

**Objectives of Concurrency**

  

 - Running program in background without hogging CPU.

   

 - Define Tasks, Define Rules and let the system take the responsibility
   of performing them.

   

 - Improve responsiveness by ensuring that the main thread is free to
   respond to user events.

**DISPATCH QUEUES**

Grand central dispatch – dispatch queues allows us to execute arbitrary blocks of code either asynchronously or synchronously
All Dispatch Queues are first in – first out
All the tasks added to dispatch queue are started in the order they were added to the dispatch queue.

