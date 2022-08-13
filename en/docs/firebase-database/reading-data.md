---
title: "Reading data"
slug: "reading-data"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Understanding which data is inside dataSnapshot object
> Note: You need to [know which data referenced by getReference()][1] first before you can completely understand this example.

There are three common method to get your data from Firebase Realtime Database: 

 - `addValueEventListener()`
 - `addListenerForSingleValueEvent()`
 - `addChildEventListener()`

When we talk about which data is inside `dataSnapshot` object, then `addValueEventListener()` and `addListenerForSingleValueEvent()` is basically the same. The only difference is `addValueEventListener()` keep listen to changes made in the referenced data while `addListenerForSingleValueEvent()` is not.

<hr>

So consider we have this database:

    "your-project-name" : {
        "users" : {
            "randomUserId1" : {
                "display-name" : "John Doe",
                "gender" : "male"
            }
            "randomUserId2" : {
                "display-name" : "Jane Dae",
                "gender" : "female"
            }
        },
        "books" {
            "bookId1" : {
                "title" : "Adventure of Someone"
            },
            "bookId1" : {
                "title" : "Harry Potter"
            },
            "bookId1" : {
                "title" : "Game of Throne"
            }
        }
    }

<hr>

**DataSnapshot produced by addValueEventListener and addListenerForSingleValueEvent**

`dataSnapshot` produced by `addValueEventListener()` and `addListenerForSingleValueEvent()` will contain value(s) of the exact data it is referenced into. Like when `ref` is point to `"your-project-name"` then `dataSnapshot` should be :

    ... onDataChange(DataSnapshot dataSnapshot) {
        dataSnapshot.getKey(); // will have value of String: "your-project-name"
        for (DataSnapshot snapshot : dataSnapshot) {
            snapshot.getKey(); // will have value of String: "users", then "books"
            for (DataSnapshot deeperSnapshot : dataSnapshot) {
                snapshot.getKey();
                // if snapshot.getKey() is "users", this will have value of String: "randomUserId1", then "randomUserId2"
                // If snapshot.getKey() is "books", this will have value of String: "bookId1", then "bookId2"
            }
        }
    }

<hr>

**DataSnapshot produced by addChildEventListener**

`dataSnapshot` produced by `addChildEventListener()` will contain value(s) of data one level deeper inside the data it is referenced into. Like in these cases:

When `ref` is point to `"your-project-name"` then `dataSnapshot` should be :


    ... onChildAdded(DataSnapshot dataSnapshot, String s) {
        dataSnapshot.getKey(); // will have value of String: "users", then "books"
        for (DataSnapshot snapshot : dataSnapshot) {
            snapshot.getKey();
            // if dataSnapshot.getKey() is "users", this will have value of String: "randomUserId1", then "randomUserId2"
            // If dataSnapshot.getKey() is "books", this will have value of String: "bookId1", then "bookId2"
            for (DataSnapshot deeperSnapshot : dataSnapshot) {
                snapshot.getKey();
                // if snapshot.getKey() is "randomUserId1" or "randomUserId1", this will have value of String: "display-name", then "gender"
                // But the value will be different based on key
                // If snapshot.getKey() is "books", this will have value of String: "title", but the value will be different based on key
            }
        }
    }
    // dataSnapshot inside onChildChanged, onChildMoved, and onChildRemoved will have the same data as onChildAdded

<hr>

I know most likely we will want to use `.getValue()` instead of `getKey()`. But in here we use `getKey` because it will always contain one String and no need to convert into custom object, or Map, or other. Basically, when you know which key `dataSnapshot` is pointing into, you can easily know which value it contains and parse it into your own custom obeject (or anything)


  [1]: https://www.wikiod.com/firebase-database/reading-data#Understanding which data referenced by getReference()

## Understanding which data referenced by getReference()
In this example, we use this database:

    "your-project-name" : {
        "users" : {
            "randomUserId1" : {
                "display-name" : "John Doe",
                "gender" : "male"
            }
            "randomUserId2" : {
                "display-name" : "Jane Dae",
                "gender" : "female"
            }
        },
        "books" {
            "bookId1" : {
                "title" : "Adventure of Someone"
            },
            "bookId1" : {
                "title" : "Harry Potter"
            },
            "bookId1" : {
                "title" : "Game of Throne"
            }
        }
    }

If you use above database then:

 - `FirebaseDatabase.getInstance().getReference()`

    will point at your project's parent, `"your-project-name"` data. So the `dataSnapshot` you acquired will contain all of data inside it, including all of `"users"` data and `"books"` data.

 - `FirebaseDatabase.getInstance().getReference("users")` and `FirebaseDatabase.getInstance().getReference().child("users")`

    will have the same result, pointing at `"your-project-name/users"`

 - `FirebaseDatabase.getInstance().getReference("users/randomUserId1")` and `FirebaseDatabase.getInstance().getReference().child("users/randomUserId1")` and `FirebaseDatabase.getInstance().getReference().child("users").child("randomUserId1")`

    will have the same result, pointing at `"your-project-name/users/randomUserId1"`

> Note: this example is needed to fully understand [which data is inside dataSnapshot object][1]


  [1]: https://www.wikiod.com/firebase-database/reading-data#Understanding which data is inside dataSnapshot object

