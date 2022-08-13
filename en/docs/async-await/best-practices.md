---
title: "Best practices"
slug: "best-practices"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Avoid async void
 - The only place where you can safely use `async void` is in event handlers. Consider the following code: 
   
       private async Task<bool> SomeFuncAsync() {
         ...
         await ...
       }
       public void button1_Click(object sender,  EventArgs e) {
         var result = SomeFuncAsync().Result;
         SomeOtherFunc();
       }
   
   Once the `async` call completes, it waits for the `SynchronizationContext` to become available. However, the event handler holds on to the `SynchronizationContext` while it is waiting for `SomeFuncAsync` method to complete; thus causing a circular wait (deadlock).
   
   To fix this we need to modify the event handler to:
   
       public async void button1_Click(object sender,  EventArgs e) {
         var result = await SomeFuncAsync();
         SomeOtherFunc();
       }

 - Any exception thrown out of an `async void` method will be raised directly on the `SynchronizationContext` that was active when the `async void` method started.
   
       private async void SomeFuncAsync() {
         throw new InvalidOperationException();
       }
       public void SomeOtherFunc() {
         try {
           SomeFuncAsync();
         }
         catch (Exception ex) {
           Console.WriteLine(ex);
           throw;
         }
       }
    the exception is never caught by the catch block in `SomeOtherFunc`. 

 - `async void` methods don’t provide an easy way to notify the calling code that they’ve completed

 - `async void` methods are difficult to test. MSTest asynchronous testing support only works for `async` methods returning `Task` or `Task<T>`.

