---
title: "Firebase Realtime Database Rules"
slug: "firebase-realtime-database-rules"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Firebase Realtime Database Rules determine who has read and write access to your database, how your data is structured, and what indexes exist. These rules live on the Firebase servers and are enforced automatically at all times. Every read and write request will only be completed if your rules allow it. By default, your rules are set to allow only authenticated users full read and write access to your database. This is to protect your database from abuse until you have time to customize your rules or set up authentication.

Firebase Database Rules have a JavaScript-like syntax and come in four types:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/OJDOQ.jpg

## Authorization
Identifying your user is only part of security. Once you know who they are, you need a way to control their access to data in your database. Firebase Database Rules allow you to control access for each user. For example, here's a set of security rules that allows anyone to read the path `/foo/`, but no one to write to it:


    {
      "rules": {
        "foo": {
          ".read": true,
          ".write": false
        }
      }
    }


`.read` and `.write` rules cascade, so this ruleset grants read access to any data at path /foo/ as well as any deeper paths such as `/foo/bar/baz`. Note that `.read` and `.write` rules that permit access will override other rules in the database that do not allow access; in other words all applicable, `.read` and `.write` rules are ORed together).  So read access to `/foo/bar/baz` would still be granted in this example even if a rule at the path `/foo/bar/baz` evaluated to false.

The Firebase Database Rules include built-in variables and functions that allow you to refer to other paths, server-side timestamps, authentication information, and more. Here's an example of a rule that grants write access for authenticated users to `/users/<uid>/`, where <uid> is the ID of the user obtained through Firebase Authentication.

    {
      "rules": {
        "users": {
          "$uid": {
            ".write": "$uid === auth.uid"
          }
        }
      }
    }

## Data validation
The Firebase Realtime Database is schemaless. This makes it easy to change things as you develop, but once your app is ready to distribute, it's important for data to stay consistent. The rules language includes a `.validate` rule which allows you to apply validation logic using the same expressions used for `.read` and `.write` rules. The only difference is that all relevant validation rules must evaluate to true in order for the write to be allowed (in other words, all applicable `.validate` rules are ANDed together to allow a database write).

These rule enforce that data written to `/foo/` must be a string less than 100 characters:

    {
      "rules": {
        "foo": {
          ".validate": "newData.isString() && newData.val().length < 100"
        }
      }
    }

Validation rules have access to all of the same built-in functions and variables as     `.read` and `.write` rules. You can use these to create validation rules that are aware of data elsewhere in your database, your user's identity, server time, and much more.

## Defining database indexes
The Firebase Realtime Database allows ordering and querying data. For small data sizes, the database supports ad hoc querying, so indexes are generally not required during development. Before launching your app though, it is important to specify indexes for any queries you have to ensure they continue to work as your app grows.

Indexes are specified using the `.indexOn` rule. Here is an example index declaration that would index the height and length fields for a list of dinosaurs:

    {
      "rules": {
        "dinosaurs": {
          ".indexOn": ["height", "length"]
        }
      }
    }

