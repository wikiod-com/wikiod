---
title: "Firebase Real-Time Database with Android"
slug: "firebase-real-time-database-with-android"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Integrate Firebase Real-Time database with an Android application
How to implement Firebase Real-Time database in Android applications.

**Setup/Installation:**

1. First, install the Firebase SDK ([guide][1])

2. Register your project using the Firebase [console][2]

3. After successfuly completing the two steps above, add the following dependency in your application level gradel.

       compile 'com.google.firebase:firebase-database:9.2.1'

4. [Optional] Configure your database security rules ([reference][3]).

**Implementation Sample:**
1. Declare and initialize the database reference

       FirebaseDatabase database = FirebaseDatabase.getInstance();
       DatabaseReference myRef = database.getReference("message");

You can later create different references to access different nodes

6. Write new data to the database

       myRef.setValue("Writing Demo");

7. Read data from the database 

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

 


  [1]: http://%20%20https://firebase.google.com/docs/android/setup
  [2]: https://console.firebase.google.com/
  [3]: http://%20https://firebase.google.com/docs/database/security/quickstart#sample-rules

