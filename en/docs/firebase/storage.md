---
title: "Storage"
slug: "storage"
draft: false
images: []
weight: 9963
type: docs
toc: true
---

Firebase Storage provides secure file uploads and downloads for your Firebase apps, regardless of network quality. You can use it to store images, audio, video, or other user-generated content. Firebase Storage is backed by Google Cloud Storage, a powerful, simple, and cost-effective object storage service.

Firebase Storage stores your files in a Google Cloud Storage bucket shared with the default Google App Engine app, making them accessible through both Firebase and Google Cloud APIs. This allows you the flexibility to upload and download files from mobile clients via Firebase and do server-side processing such as image filtering or video transcoding using Google Cloud Platform. Firebase Storage scales automatically, meaning that there's no need to migrate from Firebase Storage to Google Cloud Storage or any other provider.

This integration makes files accessible directly from the Google Cloud Storage gcloud client libraries, so you can use Firebase Storage with your favorite server-side languages. For more control, you can also use the Google Cloud Storage XML and JSON APIs.

Firebase Storage integrates seamlessly with Firebase Authentication to identify users, and provides a declarative security language that lets you set access controls on individual files or groups of files, so you can make files as public or private as you want.

See the [public docs for Firebase Storage][1] for the most up to date APIs, samples, and example apps.


  [1]: https://firebase.google.com/docs/storage/

## Getting started on iOS
## Prerequisites

1. Create a new project and add an iOS app to that project in the [Firebase Console][1].
1. Download and include `GoogleServices-Info.plist` in your application.

## Add Firebase Storage to your app

Add the following dependency to your project's `Podfile`:

```Ruby
pod 'Firebase/Storage'
```

Run `pod install` and open the created `.xcworkspace` file.

Follow these instructions to install Firebase without CocoaPods

## Set up Firebase Storage

You must initialize Firebase before any Firebase app reference is created or
used. If you have already done this for another Firebase feature, you can skip
the following two steps.

Import the Firebase module:
```Objective-C
// Obj-C
@import Firebase;
```
```Swift
// Swift
import Firebase
```

Configure a `FIRApp` shared instance, typically in your application's `application:didFinishLaunchingWithOptions:` method:

```Objective-C
// Obj-C
[FIRApp configure];  
```
```Swift
// Swift
FIRApp.configure()  
```
Get a reference to the storage service, using the default Firebase App:

```Objective-C
// Obj-C
FIRStorage *storage = [FIRStorage storage];  
```
```Swift
// Swift
let storage = FIRStorage.storage()  
```

Create a reference to a file in Firebase Storage:
```Objective-C
// Obj-C
FIRStorageReference *reference = [[storage reference] child:@"path/to/file.txt"];
```
```Swift
// Swift
let reference = storage.reference().child("path/to/file.txt")
```

Upload a file to Firebase Storage:
```Objective-C
// Obj-C
NSData *data = ... 
FIRStorageUploadTask *uploadTask = [riversRef putData:data metadata:nil completion:^(FIRStorageMetadata *metadata, NSError *error) {
  if (error != nil) {
    // Uh-oh, an error occurred!
  } else {
    // Metadata contains file metadata such as size, content-type, and download URL.
    NSURL downloadURL = metadata.downloadURL;
  }
}];
```

```Swift
// Swift
let data: NSData! = ...
let uploadTask = riversRef.putData(data, metadata: nil) { metadata, error in
  if (error != nil) {
    // Uh-oh, an error occurred!
  } else {
    // Metadata contains file metadata such as size, content-type, and download URL.
    let downloadURL = metadata!.downloadURL
  }
}
```


  [1]: http://console.firebase.google.com

