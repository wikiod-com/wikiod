---
title: "Items"
slug: "items"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Syntax
- Database.GetItem(ID itemId)
- Database.GetItem(ID itemId, Language language)
- Database.GetItem(ID itemId, Language language, Version version)
- Database.GetItem(string path)
- Database.GetItem(string path, Language language)
- Database.GetItem(string path, Language language, Version version)

Most of the above examples make use of `Sitecore.Context.Database` to fetch items. Keep in mind that most Sitecore implementations have multiple content databases, so using the correct database to fetch your item is important.

## Get Item by ID
To fetch the latest version of an item in the current language:

    Sitecore.Context.Database.GetItem(new ID("{11111111-1111-1111-1111-111111111111}"));



## Get Specific Version of Item
If you need to get a specific language or version of an item, you can use these overloads of `GetItem()`

    Sitecore.Context.Database.GetItem("/sitecore/content/Sitecore", Language.Current, new Version(5));

## Get Item by Path
To fetch the latest version of an item in the current language:

    Sitecore.Context.Database.GetItem("/sitecore/content/Sitecore")

## Get Raw Value of Field on a Sitecore Item
To get the raw value of a field on the Context Item:

    Sitecore.Context.Item["Field Name"];

To get the raw value of a field on a given item, `item`:

    item["Field Name"];


## Publish Sitecore item programmatically
When publishing Sitecore item programmatically the developer should keep in mind that Sitecore could be configured for several publishing targets, as well that several languages could be defined for item.

    ID targetDatabaseFieldId = ID.Parse("{39ECFD90-55D2-49D8-B513-99D15573DE41}");
    
         var publishingDatabases =
                                    PublishManager.GetPublishingTargets(mediaItem.Database)
                                    .Select(i => i[targetDatabaseFieldId]) //Get Target Database value
                                    .Where (i => i != null)
                                    .Select(i => Database.GetDatabase(i))
                                    .ToArray();
        
                            PublishManager.PublishItem(mediaItem, publishingDatabases,
                                LanguageManager.GetLanguages(StaticSettings.WebDatabase).ToArray(), false, false);

