---
title: "AFNetworking"
slug: "afnetworking"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Dispatching completion block on a custom thread
Whenever AFNetworking is used the call is dispatched on a custom thread provided by AFNetworking. When the call returns to the completion block, it gets executed on the main thread.

This example sets a custom thread that dispatch to the completion block:

**AFNetworking 2.xx:**

    // Create dispatch_queue_t with your name and DISPATCH_QUEUE_SERIAL as for the flag
    dispatch_queue_t myQueue = dispatch_queue_create("com.CompanyName.AppName.methodTest",
                      DISPATCH_QUEUE_SERIAL);

    // init AFHTTPRequestOperation of AFNetworking
    operation = [[AFHTTPRequestOperation alloc] initWithRequest:request];

    // Set the FMDB property to run off the main thread
    [operation setCompletionQueue:myQueue];
**AFNetworking 3.xx:**

    AFHTTPSessionManager *manager = [[AFHTTPSessionManager alloc] init];
    [self setCompletionQueue:myQueue];

