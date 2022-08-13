---
title: "Core Data"
slug: "core-data"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

Core Data is the model layer of your application in the broadest sense possible. It's the Model in the Model-View-Controller pattern that permeates the iOS SDK.

Core Data isn't the database of your application nor is it an API for persisting data to a database. Core Data is a framework that manages an object graph. It's as simple as that. Core Data can persist that object graph by writing it to disk, but that is not the primary goal of the framework.

## Operations on core data
**To Get context:**

    NSManagedObjectContext *context = ((AppDelegate*)[[UIApplication sharedApplication] delegate]).persistentContainer.viewContext;

**To fetch data:**

    NSFetchRequest<EntityName *> *fetchRequest = [EntityName fetchRequest];
    NSError *error ;
    NSArray *resultArray= [context executeFetchRequest:fetchRequest error:&error];

**To fetch data with sorting:**

    NSFetchRequest<EntityName *> *fetchRequest = [EntityName fetchRequest];
    NSSortDescriptor *sortDescriptor = [NSSortDescriptor sortDescriptorWithKey:@"someKey" ascending:YES];
    fetchRequest.sortDescriptors = @[sortDescriptor];
    NSError *error ;
    NSArray *resultArray= [context executeFetchRequest:fetchRequest error:&error];

**To add data:**

    NSManagedObject *entityNameObj = [NSEntityDescription insertNewObjectForEntityForName:@"EntityName" inManagedObjectContext:context];
    [entityNameObj setValue:@"someValue" forKey:@"someKey"];

**To save context:**

    [((AppDelegate*)[[UIApplication sharedApplication] delegate]) saveContext];

