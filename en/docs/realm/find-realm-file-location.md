---
title: "Find Realm file location"
slug: "find-realm-file-location"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Print file location - Swift
Add the following line to `ViewDidLoad` method:

    print(Realm.Configuration.defaultConfiguration.fileURL!)

The line above will print the location to Xcode's console. Copy the file path, go to **Finder** → Go → Go to Folder... (or **⌘+⇧+G**)→ paste the path and hit Go.

## Print file location - Objective-C
Log the realm file location using:    

    NSLog(@"%@",[RLMRealmConfiguration defaultConfiguration].fileURL);

The line above will print the location to Xcode's console. Copy the file path, go to **Finder** → Go → Go to Folder... (or **⌘+⇧+G**)→ paste the path and hit Go.

## Print file location - Xamarin
First you need to implement Realm at the start of your class

    using Realms;

Then to print the location to the console :

    Console.WriteLine( RealmConfiguration.PathToRealm() );

Or if you're using DefaultConfiguration, you can use :

    Console.WriteLine( RealmConfiguration.DefaultConfiguration.DatabasePath );

## **How to reach the file :**
If you're running on **IOS simulator** :

You can copy the file path, go to **Finder** → Go → Go to Folder... (or ⌘+⇧+G)→ paste the path and hit Go.

But if you're running on **Android emulator** :

Open **Android device Monitor** (on visual studio → tools menu → Android → Android device Monitor ) (on Xamarin studio → Tools menu → Open Android device Monitor ) → File Explorer tab → follow the file path

## Android
Copy the database from the emulator/phone to view it. It can be done by using ADB:

```
adb pull /data/data/<packagename>/files/
```

That command will pull all Realm files created by `Realm.getInstance(getContext())` or `Realm.getInstance(new RealmConfiguration.Builder(context).build())`. The default database file is called `default.realm`.

Note that this will only work on a emulator or rooted device.

