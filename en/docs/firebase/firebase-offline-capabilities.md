---
title: "Firebase Offline Capabilities"
slug: "firebase-offline-capabilities"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

In this post you will find the different ways to implement offline capabilities when using`Firebase` , information about when and why could be a good idea enable offline capabilities and examples of it with Android platform.


**What should I use? Disk persistence or keepSynced calls?**


----------

From my experience I can say that it always depends of what your app is working, and how you manage the transactions and database of your application. If for example you have an application where the user is just writing and reading data but he is not able to delete or edit it, use `DiskPersistence` would be the right choice.

Also, `DiskPersistence` will store data in cache, which means that your app will use more space in the user's devices, which maybe is not the best idea in your case.

In other hand, if your application manages a lot of complex transactions and your data is updated really often, possibly you should avoid `DiskPersistence` and use `keepSynced` in the references that you want to keep fresh.

**Why?**


----------

`DiskPersistence` stores the data retrieved in local, which sometimes can cause lot of desynchronization showing your data if you don't use it together with continous `ListenerValueEvents`. For example:

 1. User A writes a message "Hello World" on your app, which is recieved for user B
 2. User B download message from User A in his phone and see the message "Hello World"
 3. User A edit's his message to "Firebase is cool".
 4. User B will still watching "Hello World" message even if he updates the data cause the snapshot ref is the same when Firebase filter for it.

To avoid this the best idea is keep continous listeners in the references that you want to track all time.

**Can I use both together?** 


----------

Of course you can, and in most of the apps possibly is the best idea to avoid download a lot of data and give the user the possibility to work with your app even if he has no connection.

If you don't care about use cache space in the user device, I recommend you to enable `diskPersistence` in your `FirebaseDatabase`object and also add a `keepSync` flags to each reference that can have a lot of times in a short space time or you want to keep fresh all time.




## Enable disk persistence (Android / iOS only)
To enable disk persistence you should enable the flag `persistenceEnabled` in the `FirebaseDatabaseInstance` object of your application:

**Android**

    FirebaseDatabase.getInstance().setPersistenceEnabled(true);
    
**iOS**

    Database.database().isPersistenceEnabled = true //Swift
    [FIRDatabase database].persistenceEnabled = YES; //Objetive-C

If you want to disable the persistence in some moment of your app lifecycle you should remember to disable it in the same way:

**Android**

    FirebaseDatabase.getInstance().setPersistenceEnabled(false);
    
**iOS**

    Database.database().isPersistenceEnabled = false //Swift
    [FIRDatabase database].persistenceEnabled = NO; //Objetive-C

## Keeping data fresh (Android/iOs Only)
Firebase synchronizes and stores a local copy of the data for active listeners when used on mobile devices. In addition, you can keep specific locations in sync.

**Android :**

```
DatabaseReference workoutsRef = FirebaseDatabase.getInstance().getReference("workouts");
scoresRef.keepSynced(true);
```

**iOs:**

```
//Objetive-c
FIRDatabaseReference *scoresRef = [[FIRDatabase database] referenceWithPath:@"scores"];
[scoresRef keepSynced:YES];
//Swift
let scoresRef = Database.database().reference(withPath: "scores")
scoresRef.keepSynced(true)
```

Firebase client automatically downloads the data at these locations and keeps it updated even if the reference has no active listeners. You disable synchronization with the following line of code.

**Android :**
```
scoresRef.keepSynced(false);
```
**iOS:**
```
[scoresRef keepSynced:NO]; //Objetive-C
scoresRef.keepSynced(false) //Swift
```


