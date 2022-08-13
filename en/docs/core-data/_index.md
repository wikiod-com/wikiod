---
title : core-data Tutorial
slug : core-data-tutorial
weight : 9973
draft : false
images : []
type : docs
---

Core Data is a framework in Apple’s various OS SDK including, but not limited to iOS and OS X.  It has two major roles a model layer and a persistence layer. The model layer is used in the management of model objects and persist data. Simply you can store and manage data in an object-oriented interface. Primary features include filtering, querying, sorting, persisting data and creating relationships between data. Other subjects of interest to Core Data projects are NSPredicate, threading, and among others.  

An example application of Core Data could a Catalog app for your local library. In the Catalog app a librarian could add or remove books. They could also filter books by genre, sort books by publication date, or search for a specific authors work. An entity “Book” would have various attributes such as title, author, publication date, isbn, call number, etc. Core Data including the above example can also store data gathered from a server. 

Major components of the framework include:

 - Data Models (entities, attributes, and relationships) 
 - Core Data Stack (NSPersistentStoreCoordinator,NSManagedObjectModel, NSManagedObjectContext)  
 - NSFetchRequest
 - NSFetchedResultsController

Sources: 

[Framework Documentation][1] 

[Programming Guide][2]

[Core Data Release Notes 2016][3]


  [1]: https://developer.apple.com/reference/coredata
  [2]: https://developer.apple.com/library/ios/documentation/Cocoa/Conceptual/CoreData/index.html#//apple_ref/doc/uid/TP40001075-CH2-SW1
  [3]: https://developer.apple.com/library/prerelease/content/releasenotes/General/WhatNewCoreData2016/ReleaseNotes.html


**CoreData & Concurrency**

It's important to remember that CoreData is **NOT** thread-safe, which means that if it's necessary to use for example a background-thread to work on ManagedObjects, there are new things to consider, like *PrivateQueue- / MainQueue*-ManagedObjectContexts.

From Apples documentary: *Core Data expects to be run on a single thread. You should never share managed object contexts between threads. This is a hard rule you should not break.*

