---
title: "FirebaseRealtime database with Android"
slug: "firebaserealtime-database-with-android"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Example for data insert or data retrieve from Firebase


## Get value/s from firebase


## Add the Realtime Database in Android
1. Complete the [Installation and setup][1] to connect your app to Firebase.  
This will create the project in Firebase.

2. Add the dependency for Firebase Realtime Database to your module-level `build.gradle` file:  


    compile 'com.google.firebase:firebase-database:9.2.1'

3. Configure [Firebase Database Rules][2]

Now you are ready to work with the Realtime Database in Android.

For example you write a `Hello World` message to the database under the `message` key.

    // Write a message to the database
    FirebaseDatabase database = FirebaseDatabase.getInstance();
    DatabaseReference myRef = database.getReference("message");
    
    myRef.setValue("Hello, World!");


  [1]: https://www.wikiod.com/firebase-database/getting-started-with-firebase-database
  [2]: https://www.wikiod.com/firebase-database/firebase-realtime-database-rules

## Using setValue to save data
The`setValue()` method overwrites data at the specified location, including any child nodes.

You can use this method to:

1. Pass types that correspond to the available JSON types as follows:

- String
- Long
- Double
- Boolean
- Map<String, Object>
- List<Object>

2. Pass a custom Java object, if the class that defines it has a default constructor that takes no arguments and has public getters for the properties to be assigned.

This is an example with a CustomObject.   
First define the object.

    @IgnoreExtraProperties
    public class User {
    
        public String username;
        public String email;
    
        public User() {
            // Default constructor required for calls to DataSnapshot.getValue(User.class)
        }
    
        public User(String username, String email) {
            this.username = username;
            this.email = email;
        }
    
Then get the Database reference and set the value: 

       User user = new User(name, email);
       DatabaseReference mDatabase mDatabase = FirebaseDatabase.getInstance().getReference();
       mDatabase.child("users").child(userId).setValue(user);
     


