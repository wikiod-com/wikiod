---
title: "Core Data Stack"
slug: "core-data-stack"
draft: false
images: []
weight: 9875
type: docs
toc: true
---

This is an implementation of the Core Data Stack which is initially placed in the `AppDelegate` file if the project is created with Core Data when project is created. These functions can also implemented in separate class for `CoreDataStack.swift`. One of the major functions is to get the NSManagedObjectContext. 

## Objective-C ##

    - (NSManagedObjectContext *)managedObjectContext {...}

## Swift 2 ##

    lazy var managedObjectContext: NSManagedObjectContext = {...}

## Swift 3 ##

    lazy var persistentContainer: NSPersistentContainer = {...)
    let managedObjectContext = persistentContainer.viewContext


The Core Data stack that communicates between the objects in your application and external data stores. The Core Data stack handles all of the interactions with the external data stores so that your application can focus on its business logic. The stack consists of three primary objects: the managed object context (`NSManagedObjectContext`), the persistent store coordinator (`NSPersistentStoreCoordinator`), and the managed object model (`NSManagedObjectModel`).

`NSManagedObjectModel`
--------------------

The `NSManagedObjectModel` instance describes the data that is going to be accessed by the Core Data stack. `NSManagedObjectModel` (often referred to as the “mom”) is loaded into memory as the first step in the creation of the stack. An example of the `NSManagedObjectModel` is DataModel.momd. The `NSManagedObjectModel` defines the structure of the data

`NSPersistentStoreCoordinator`
----------------------------

The `NSPersistentStoreCoordinator` realizes objects from the data in the persistent store and passes those objects off to the requesting `NSManagedObjectContext`. It creates new instances of the entities in the model, and it retrieves existing instances from a persistent store (`NSPersistentStore`). The `NSPersistentStoreCoordinator` also verifies that the data is in a consistent state that matches the definitions in the `NSManagedObjectModel`.

`NSManagedObjectContext`
----------------------

When you fetch objects from a persistent store, you bring temporary copies onto the scratch pad where they form an object graph (or a collection of object graphs). You can then modify those objects, unless you actually save those changes, however, the persistent store remains unaltered.

All managed objects must be registered with a managed object context. You use the context to add objects to the object graph and remove objects from the object graph. The context tracks the changes you make, both to individual objects’ attributes and to the relationships between objects. By tracking changes, the context is able to provide undo and redo support for you. It also ensures that if you change relationships between objects, the integrity of the object graph is maintained.

When you save changes the context ensures that your objects are in a valid state. The changes are written to the persistent store (or stores), new records are added for objects you created, and records are removed for objects you deleted.


***Source:***
[Apple Core Data Programming: Initializing the Core Data Stack][1]


  [1]: https://developer.apple.com/library/ios/documentation/Cocoa/Conceptual/CoreData/InitializingtheCoreDataStack.html#//apple_ref/doc/uid/TP40001075-CH4-SW1

## Objective-C Example
    
This is a simple but robust core-data set-up for iOS 10+.
There are exactly two way to access core-data:

 1. **viewContext**.  The `viewContext` can only be used from the main thread, and only for reading.  
 2. **strong enqueueCoreDataBlock**. All writing should be done using `enqueueCoreDataBlock`.  There is no need to save at the end it will automatically save.  All writes are enqueued in an operationQueue so there can never be be write conflicts.

Make sure to NEVER use any managedObjects from context in another context.  Also discard all objects that are created or fetched in `enqueueCoreDataBlock` as the context that backs them will be destroyed after the block is executed.

// CoreDataManager.h

    @interface CoreDataManager : NSObject
    @property (nonatomic, readonly) NSManagedObjectContext * viewContext;
    - (void)enqueueCoreDataBlock:(void (^)(NSManagedObjectContext* context))block;
    @end

// CoreDataManager.m

    @implementation NSManagedObjectContext(SaveIfNeeded)
    -(BOOL) saveIfNeeded{
        BOOL toReturn = YES;
        if ([self hasChanges]) {
            NSError *error;
            toReturn = [self save:&error];
            if (toReturn == NO || error)
            {
                //Here you should log to your analytics service
                NSLog(@"--- Failed to commit data\n error: %@", error);
            }
        }
        return toReturn;
    }
    @end
    @interface CoreDataManager ()
    @property (nonatomic, strong) NSPersistentContainer* persistentContainer;
    @property (nonatomic, strong) NSOperationQueue* persistentContainerQueue;    
    @end
    @implementation CoreDataManager
    
    - (id)init
    {
        self = [super init]
        if (self)
        {
            self.persistentContainer = [[NSPersistentContainer alloc] initWithName:@"PROJECT_NAME_ALSO_NAME_OF_MODEL" managedObjectModel:managedObjectModel];
            [self.persistentContainer loadPersistentStoresWithCompletionHandler:^(NSPersistentStoreDescription * description, NSError * error) {              
            }];
            self.persistentContainer.viewContext.automaticallyMergesChangesFromParent = YES;
            _persistentContainerQueue = [[NSOperationQueue alloc] init];
            _persistentContainerQueue.maxConcurrentOperationCount = 1;
            _persistentContainerQueue.name = @"persistentContainerQueue";
            dispatch_queue_t queue = dispatch_queue_create("persistentContainerQueue.dispatchQueue", DISPATCH_QUEUE_SERIAL);
            _persistentContainerQueue.underlyingQueue = queue;            
        }
    }
    
    - (void)enqueueCoreDataBlock:(void (^)(NSManagedObjectContext* context))block{
        void (^blockCopy)(NSManagedObjectContext*) = [block copy];
    
        [self.persistentContainerQueue addOperation:[NSBlockOperation blockOperationWithBlock:^{
            NSManagedObjectContext* context =  self.persistentContainer.newBackgroundContext;
            [context performBlockAndWait:^{
                blockCopy(context);
                [context saveIfNeeded];
            }];
        }]];
    }
    
    -(NSManagedObjectContext*) viewContext{
        if (![NSThread mainThread]) {
            //here you should log to you analytics service. If you are in developer mode you should crash to force you to fix this
            NSLog(@"access context on wrong thread!!");
        }
        return self.persistentContainer.viewContext;
    }

## Swift 2 Example
    // Core Data stack
    
    lazy var applicationDocumentsDirectory: NSURL = {
        let urls = NSFileManager.defaultManager().URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask)
        return urls[urls.count-1]
    }()
    
    lazy var managedObjectModel: NSManagedObjectModel = {
        let modelURL = NSBundle.mainBundle().URLForResource("ProjectName", withExtension: "momd")!
        return NSManagedObjectModel(contentsOfURL: modelURL)!
    }()
    
    lazy var persistentStoreCoordinator: NSPersistentStoreCoordinator = {
    
        let coordinator = NSPersistentStoreCoordinator(managedObjectModel: self.managedObjectModel)
        let url = self.applicationDocumentsDirectory.URLByAppendingPathComponent("SingleViewCoreData.sqlite")
        var failureReason = "There was an error creating or loading the application's saved data."
        do {
            try coordinator.addPersistentStoreWithType(NSSQLiteStoreType, configuration: nil, URL: url, options: nil)
        } catch {
            var dict = [String: AnyObject]()
            dict[NSLocalizedDescriptionKey] = "Failed to initialize the application's saved data"
            dict[NSLocalizedFailureReasonErrorKey] = failureReason
    
            dict[NSUnderlyingErrorKey] = error as NSError
            let wrappedError = NSError(domain: "YOUR_ERROR_DOMAIN", code: 9999, userInfo: dict)
            print("Unresolved error \(wrappedError), \(wrappedError.userInfo)")
            abort()
        }
        
        return coordinator
    }()
    
    lazy var managedObjectContext: NSManagedObjectContext = {
        let coordinator = self.persistentStoreCoordinator
        var managedObjectContext = NSManagedObjectContext(concurrencyType: .MainQueueConcurrencyType)
        managedObjectContext.persistentStoreCoordinator = coordinator
        return managedObjectContext
    }()
    
    // Core Data Saving support
    
    func saveContext () {
        if managedObjectContext.hasChanges {
            do {
                try managedObjectContext.save()
            } catch {
                let nserror = error as NSError
                print("Unresolved error \(nserror), \(nserror.userInfo)")
                abort()
            }
        }
    }



## iOS 10 Example in Swift
    lazy var persistentContainer: NSPersistentContainer = {
        
        let container = NSPersistentContainer(name: "ProjectName")
        container.loadPersistentStores(completionHandler: { (storeDescription, error) in
            
            if let error = error {
                fatalError("Unresolved error \(error), \(error.userInfo)")
            }
        })
        return container
    }()
        
    func saveContext () {
        let context = persistentContainer.viewContext
        
        do {
            try context.save()
        } catch {
            let nserror = error as NSError
            fatalError("Unresolved error \(nserror), \(nserror.userInfo)")
        }
        
        if context.hasChanges {
            print("changes occured")
        }else {
            print("no changes occured")
        }
        
    }

