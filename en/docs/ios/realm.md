---
title: "Realm"
slug: "realm"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Adding a new RLMObject to an existing Realm - Schema and Migrations

Adding new model classes to a Realm does not require a migration or a schema version bump; only making changes to an existing Realm.

## RLMObject Base Model Class with Primary Key - Objective-C
An example of a RLMObject base model class that uses a primary key and some generic default properties. Subclasses can then set metadata specific to their needs.
    
    @interface BaseModel : RLMObject
    
    @property NSString *uuid;
    @property NSString *metadata;
    
    @end

    @implementation BaseModel
    
    + (NSString *)primaryKey
    {
        return @"uuid";
    }
    
    + (NSDictionary *)defaultPropertyValues
    {
        NSMutableDictionary *defaultPropertyValues = [NSMutableDictionary dictionaryWithDictionary:[super defaultPropertyValues]];
        NSString *uuid = [[NSUUID UUID] UUIDString];
        [defaultPropertyValues setValue:@"" forKey:@"metadata"];
        [defaultPropertyValues setValue:uuid forKey:@"uuid"];
        return defaultPropertyValues;
    }
    
    + (NSArray *)ignoredProperties
    {
        return @[];
    }
    
    @end


