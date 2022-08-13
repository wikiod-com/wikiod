---
title: "CloudKit"
slug: "cloudkit"
draft: false
images: []
weight: 9978
type: docs
toc: true
---

# Supported Types

* NSData
* NSDate (Date)
* NSNumber (Int / Double)
* NSString (String)
* NSArray (Array)
* CLLocation
* CKReference
* CKAsset

[More Details][1]

[CloudKit Dashboard][2]


  [1]: http://developer.apple.com/cloudkit
  [2]: https://icloud.developer.apple.com/dashboard/

## Registering app for use with CloudKit
What you need is to get an entitlements file so the app can access your iCloud and write records using CloudKit.

Follow the steps to grant access to iCloud from your app:

1- Select the project in the Project Navigator, and then open the General tab. 

2- In the Identity section, set your developer Apple ID to the Team dropdown menu. (If it is not available, add it in Xcode menu -> Preferences -> Accounts.

3- Go to Capabilities tab in the project properties and turn iCloud on. Then, select "Key-Value Storage" and "CloudKit".

[![enter image description here][1]][1]

4- Make sure these items are checked:

[![enter image description here][2]][2]

If all of the items are checked, then your app is ready to use CloudKit.


  [1]: http://i.stack.imgur.com/vQl0o.png
  [2]: http://i.stack.imgur.com/j1kzA.png

## Using CloudKit Dashboard
All the records create using CloudKit-related code can be previewed, edited and even removed in CloudKit Dashboard. To access CloudKit Dashboard, go [here][1].

There are several parts in the dashboard:

* Record Types (which will be discussed later)
* Security Roles (which is where you can set databases as public or private)
* Subscription Types (which your app could register for [Apple Push Notifications (APNs)][2] to notify you when a record is changed)

# Record Types

Here, you get a list of all the existing record types in the app. When you first open CloudKit Dashboard for an app,  there’s a record type called Users there, which you can use it or just delete it and use your own.

In this page, you could make your data typed manually. Of course, in most cases this is pointless, because iOS SDK can handle it way better than the dashboard, but the functionality is also there if you prefer. The most use of this page is for previewing types.


  [1]: https://icloud.developer.apple.com/dashboard/
  [2]: https://www.wikiod.com/ios/push-notifications

## Saving data to CloudKit
To save date to CloudKit, we must make:

* A `CKRecordID` (the key of your unique record)
* A `CKRecord` (which includes data)

# Making a record key

To ensure that every new record identifier is unique, we use the current *timestamp*, which  is unique. We get the timestamp using `NSDate`'s method `timeIntervalSinceReferenceDate()`. It is in form of ###.### (# are numbers), which we will use the integer part. To do this, we split the string:

## Swift

    let timestamp = String(format: "%f", NSDate.timeIntervalSinceReferenceDate())
    let timestampParts = timestamp.componentsSeparatedByString(".")
    let recordID = CKRecordID(recordName: timestampParts[0])

# Making the record

To make the record, we should specify the record type (explained in Using CloudKit Dashboard) as Users, the ID as the thing we made just now and the data. Here, we will add a sample text, a picture and the current date to the record:

## Swift

    let record = CKRecord(recordType: "Users", recordID: recordID)
    record.setObject("Some Text", forKey: "text")
    record.setObject(CKAsset(fileURL: someValidImageURL), forKey: "image")
    record.setObject(NSDate(), forKey: "date")

## Objective-C

    CKRecord *record = [[CKRecord alloc] initWithRecordType: "Users" recordID: recordID];
    [record setObject: "Some Text" forKey: "text"];
    [record setObject: [CKAsset assetWithFileURL: someValidImageURL] forKey: "image"];
    [record setObject: [[NSDate alloc] init] forKey: "date"];

> # Note
> Here, we didn't add the `UIImage` directly to the record, because as mentioned in Remarks, image format isn't directly supported in CloudKit, so we have converted `UIImage` into `CKAsset`.

# Accessing the container

## Swift

    let container = CKContainer.defaultContainer()
    let database = container.privateCloudDatabase // or container.publicCloudDatabase

# Saving the records to CloudKit database

## Swift

    database.saveRecord(record, completionHandler: { (_, error) -> Void in
        print(error ?? "")
    })

