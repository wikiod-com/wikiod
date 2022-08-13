---
title: "Getting started with firebase-database"
slug: "getting-started-with-firebase-database"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Add Firebase to Your Android Project
Here the steps required to create a Firebase project and to connect it with an Android app.

# Add Firebase to your app


1. Create a Firebase project in the [Firebase console][1] and click **Create New Project**.

2. Click **Add Firebase to your Android app** and follow the setup steps. 

3. When prompted, enter your **app's package name**.  
It's important to enter the package name your app is using; this can only be set when you add an app to your Firebase project.

4. At the end, you'll download a `google-services.json` file. You can download this file again at any time. ( this file is located under project setting in Firebase console).

5. Switch android studio View to Project and paste google-service.json file under app folder

The next step is to Add the SDK to integrate the Firebase libraries in the project.

# Add the SDK

To integrate the Firebase libraries into one of your own projects, you need to perform a few basic tasks to prepare your Android Studio project. You may have already done this as part of adding Firebase to your app.

1. Add rules to your root-level `build.gradle` file, to include the **google-services plugin**:


    buildscript {
        // ...
        dependencies {
            // ...
            classpath 'com.google.gms:google-services:3.1.1'
        }
    }

Then, in your module Gradle file (usually the `app/build.gradle`), add the apply plugin line at the bottom of the file to enable the Gradle plugin:

    apply plugin: 'com.android.application'
    
    android {
      // ...
    }
    
    dependencies {
      // ...
      compile 'com.google.firebase:firebase-core:9.4.0'//THIS IS FOR ANALYTICS
      compile "com.google.firebase:firebase-database:11.0.2" 
    }
    
    // BELOW STATEMENT MUST BE WRITTEN IN BOTTOM
    apply plugin: 'com.google.gms.google-services'

**Notes:** 

 - Data cannot be read/write without Authenticating. If you want it
   without authentication. Change rules in Database firebase console.

    { "rules": { ".read": true, ".write": true } }

 - Add internet permission in Manifest

 - Upgrade Google Play Services and Google Repository

 
  [1]: https://firebase.google.com/console/
  [2]: https://support.google.com/firebase/answer/7015592


## Writing simple value into database
First, complete the [installation and setup][1] to connect your app to Firebase. Then from anywhere in your class, you can write:

    // Write a message to the database
    FirebaseDatabase database = FirebaseDatabase.getInstance();
    DatabaseReference myRef = database.getReference("message");
    
    myRef.setValue("Hello, World!");

It will write `Hello, Wold!` into `message` node, like seen below:

    "your-project-parent" : {
        "message" : "Hello, World!"
    }

**Explanation**

    FirebaseDatabase database = FirebaseDatabase.getInstance();

Above code will assign `FirebaseDatabase` instance into `database` object for further use.

    DatabaseReference myRef = database.getReference("message");

Above code will reference `myRef` object into `"message"` child of your project's parent (in this example, it is `"your-project-parent"`). So it is `"your-project-parent/message"`

    myRef.setValue("Hello, World!");

Above code will set `"Hello, World!"` into path referenced by `myRef` 

  [1]: https://www.wikiod.com/firebase-database/getting-started-with-firebase-database#Add Firebase to Your Android Project

## Automatically map custom model to data structure
After you have set a few data to database and have get a structure consisting of several nodes like this;

    "user" : {
        "-KdbKcU2ptfYF2xKb5aO" : {
          "firstName" : "Arthur",
          "lastName" : "Schopenhauer",
          "userName" : "AphorismMan",
          "phone" : "+9022-02-1778",
          "gender": "M",
          "age" : 25
        },
        "-KdbQFjs9BDviuqQVHjY" : {
          "firstName" : "Werner",
          "lastName" : "Heisenberg",
          "userName" : "whereAmI",
          "phone" : "+9005-12-1901",
          "gender": "M",
          "age" : 75
        }
      }
you can categorize data structures.

**Creating Class**

Create a model class to set to database.

    @IgnoreExtraProperties
    public class User {
        public String firstName;
        public String lastName;
        public String userName;
        public String phone;
        public String gender;
        public int age;
    
        public User() {
        }
    
        public User(String firstName, String lastName, String userName, String phone, String gender, int age) {
            this.firstName = firstName;
            this.lastName = lastName;
            this.userName = userName;
            this.phone = phone;
            this.gender = gender;
            this.age = age;
        }
    }

Some things to remember when creating a model class that you want to map to your data:

 1. You have to have an empty constructor
 2. Scope of Variables/Fields must be public, so that the [DataSnapshot][1] returning from the firebase can access these fields. If you don't do that, when you want to get data, DataSnapshot can't access to your model in callback and that will cause an exception.
 3. Names of Variables/Fields should match to those in your data structure.

**Sending to Firebase**

Create a User object

    User user = new User ( "Arthur","Schopenhauer","AphorismMan","+9022-02-1778","M",25)

and reference

    DatabaseReference databaseReference = FirebaseDatabase.getInstance().getReference();

Now you have the reference of your database. Create an `user` node with `databaseReference.child("user")`. If you do `.push()` your models will locate under randomly created unique ids like above, `"-KdbKcU2ptfYF2xKb5aO", "-KdbQFjs9BDviuqQVHjY"`. 


    databaseReference.child("user").push().setValue(user, new DatabaseReference.CompletionListener() {
                @Override
                public void onComplete(DatabaseError databaseError, DatabaseReference databaseReference) {
                    Toast.makeText(getActivity(), "User added.", Toast.LENGTH_SHORT).show();
    
                }
            });

If you want to set your datas under your specific key, do it with `.child("yourSpecificKey")` instead of `.push()`.

`databaseReference.child("user").child("yourSpecificKey").setValue(user,...`


  [1]: https://firebase.google.com/docs/reference/js/firebase.database.DataSnapshot

