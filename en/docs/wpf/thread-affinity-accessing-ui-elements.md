---
title: "Thread Affinity Accessing UI Elements"
slug: "thread-affinity-accessing-ui-elements"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Accessing a UI Element From Within a Task
All UI elements created and reside in the main thread of a program. Accessing these from another thread is forbidden by the .net framework runtime. Basically it is because all UI elements are **thread sensitive resources** and accessing a resource in a multi-threaded environment requires to be thread-safe. If this cross thread object access is allowed then consistency would be affected in the first place. 

Consider this scenario: 

We have a calculation happening inside a task. Tasks are run in another thread than the main thread. While calculation goes on we need to update a progressbar. To do this:

    //Prepare the action
    Action taskAction = new Action( () => {   
        int progress = 0;
        Action invokeAction = new Action( () => { progressBar.Value = progress; });
        while (progress <= 100) {
            progress = CalculateSomething();
            progressBar.Dispatcher.Invoke( invokeAction );
        }
    } );

    //After .net 4.5
    Task.Run( taskAction );
    
    //Before .net 4.5
    Task.Factory.StartNew( taskAction ,
        CancellationToken.None, 
        TaskCreationOptions.DenyChildAttach, 
        TaskScheduler.Default);

Every UI element has a Dispatcher object that comes from its `DispatcherObject` ancestor (inside `System.Windows.Threading` namespace). Dispatcher executes the specified delegate synchronously at the specified priority on the thread on which the Dispatcher is associated with. Since execution is syncronised, caller task should wait for its result. This gives us the opportunity to use `int progress` also inside a dispatcher delegate.

We may want to update a UI element asynchronously then `invokeAction` definition changes:

    //Prepare the action
    Action taskAction = new Action( () => {   
        int progress = 0;
        Action<int> invokeAction = new Action<int>( (i) => { progressBar.Value = i; } )
        while (progress <= 100) {
            progress = CalculateSomething();
            progressBar.Dispatcher.BeginInvoke( 
                invokeAction,
                progress );
        }
    } );

    //After .net 4.5
    Task.Run( taskAction );
    
    //Before .net 4.5
    Task.Factory.StartNew( taskAction ,
        CancellationToken.None, 
        TaskCreationOptions.DenyChildAttach, 
        TaskScheduler.Default);

This time we packed `int progress` and use it as a parameter for delegate.


