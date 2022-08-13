---
title: "How do I listen for errors when accessing the database?"
slug: "how-do-i-listen-for-errors-when-accessing-the-database"
draft: false
images: []
weight: 9942
type: docs
toc: true
---

There are many reasons a read or write operation may fail. A frequent one is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user) or because you're writing/listening at a location where you don't have permission.

## Detect errors when writing a value on Android
There are many reasons a write operation may fail. A frequent one is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the logcat output. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed write on Android you [attach a completion callback to `setValue`](https://firebase.google.com/docs/reference/android/com/google/firebase/database/DatabaseReference.html#setValue):

    ref.setValue("My new value", new DatabaseReference.CompletionListener() {
        public void onComplete(DatabaseError databaseError, DatabaseReference databaseReference) {
            throw databaseError.toException();
        }
    });

Throwing an exception like this ensures that it will be very difficult to overlook such an error next time.

## Detect errors when reading data on Android
A frequent reason why your read operation may not work is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the logcat output. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed read on Android you must implement the `onCancelled` method of your `ChildEventListener`:

    databaseRef.addChildEventListener(new ChildEventListener() {
        public void onChildAdded(DataSnapshot dataSnapshot, String s) { ... }
        public void onChildChanged(DataSnapshot dataSnapshot, String s) { ... }
        public void onChildRemoved(DataSnapshot dataSnapshot) { ... }
        public void onChildMoved(DataSnapshot dataSnapshot, String s) { ... }
        public void onCancelled(DatabaseError databaseError) {
            throw databaseError.toException();
        }
    });

Or if you have a `ValueEventListener`:

    databaseRef.addValueEventListener(new ValueEventListener() {
        public void onDataChange(DataSnapshot dataSnapshot, String s) { ... }
        public void onCancelled(DatabaseError databaseError) {
            throw databaseError.toException();
        }
    });

With this code in place it will be pretty hard to overlook a security error when reading data on Android.

## Detect errors when writing a value on iOS
There are many reasons a write operation may fail. A frequent one is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the output of your program. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed write on iOS you [attach a completion block to setValue](https://firebase.google.com/docs/reference/ios/firebasedatabase/interface_f_i_r_database_reference.html#a1107cae145ab12e1ef58ab9f8713b84d):

    let message = ["name": "puf", "text": "Hello from iOS"]
    ref!.childByAutoId().setValue(message) { (error) in
        print("Error while writing message \(error)")
    }


Throwing an exception like this ensures that it will be very difficult to overlook such an error next time.

## Detecting errors when reading data in JavaScript
A frequent reason why your read operation may not work is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the JavaScript console of your browser. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed read in JavaScript you must implement add a second callback to your `on()` clause:

    ref.on('value', function(snapshot) {
        console.log(snapshot.key, snapshot.val());
    }, function(error) {
        alert(error);
    })


With this code in place it will be pretty hard to overlook a security error when reading data in JavaScript.

## Detecting errors when writing a value in JavaScript
There are many reasons a write operation may fail. A frequent one is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the console output. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed write in JavaScript you attach a completion callback to `set`:

    ref.set("My new value").catch(function(error)
        console.error(error);
        alert(error);
    });

Showing an alert like this ensures that it will be very difficult to overlook such an error next time.


## Detect errors when reading data on iOS
A frequent reason why your read operation may not work is because your security rules reject the operation, for example because you're not authenticated (by default a database can only be accessed by an authenticated user).

You can see these security rule violations in the Console output. But it's easy to overlook these. You can also handle them in your own code and make them more prominently visible, which is especially useful during development (since your JSON, rules and code change often).

To detect a failed read on iOS you must implement the `withCancel` block of your observer:

        ref!.child("notAllowed").observe(.value, with: { (snapshot) in
            print("Got non-existing value: \(snapshot.key)")
        }, withCancel: { (error) in
            print(error)
        })


