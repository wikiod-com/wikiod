---
title: "How to get push key value from Firebase Database?"
slug: "how-to-get-push-key-value-from-firebase-database"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

In Firebase Database everything is a node, that follows the pattern key: value. Firebase Database provides us with a simple way to generate unique keys. Unique keys create new items while uploading data to a previously stored key will update.

## Android Example
Let's assume we have a Dogs application, then our model will be a Dog class.

    DatabaseReference reference = FirebaseDatabase.getInstance().getReference().child("dogs");

This is how to send a Dog to the database, a new unique dog and set the dog with the key.

    String key = reference.push().getKey();
    Dog dog = new Dog("Spike");
    dog.setKey(key);
    reference.child(key).setValue(dog);

The `reference.child(key).setValue(dog);` is equivalent of `reference.push().setValue(dog);` And add the benefit of getting the key inside the `Dog` object.

