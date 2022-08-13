---
title: "Firbase Realtime Database with Android"
slug: "firbase-realtime-database-with-android"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## How to connect Realtime database with Android Application
How to implement FirebaseRealTime database in android application.
Following is the steps for do it.

1. First install firebase sdk, If you dont know how to install then following is the URL for help.
[Install Firebase SDK][1]

2. After thet register your project in firbase console, URL of the firbase console is
[Firebase Console Url][2]

3. After successfuly complet above to step add following dependency in you application level gradel.
compile 'com.google.firebase:firebase-database:9.2.1'

4. Also one more thing configure your firebase database rules. If you dont how to configure then following is the URL which help you.
[Configure firebase Rules][3]

5. Now after all thing done, Original code is start, First retrieve your database instance throw FirebaseDatabase like following,

    FirebaseDatabase database = FirebaseDatabase.getInstance();
    DatabaseReference myRef = database.getReference("message");

You can now create different different object of DatabaseReference for the access different node,

6. Now you can save or retrieve data using DataBaseReference like following way,
For the save :

    myRef.setValue("Demo for Save");

Read data : 

    myRef.addValueEventListener(new ValueEventListener() {
        @Override
        public void onDataChange(DataSnapshot dataSnapshot) {
            // This method is called once with the initial value and again
            // whenever data at this location is updated.
            String value = dataSnapshot.getValue(String.class);
            Log.d(TAG, "Value is: " + value);
        }
    
        @Override
        public void onCancelled(DatabaseError error) {
            // Failed to read value
            Log.w(TAG, "Failed to read value.", error.toException());
        }
    });

 
Note : This is the only introducation topic how to implement database in android application lost of more thing available in FirebaseRealtime database,


  [1]: https://firebase.google.com/docs/android/setup
  [2]: https://console.firebase.google.com/
  [3]: https://firebase.google.com/docs/database/security/quickstart#sample-rules

