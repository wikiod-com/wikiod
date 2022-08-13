---
title: "How to use the Firebase Database to keep a list of Firebase Authentication users"
slug: "how-to-use-the-firebase-database-to-keep-a-list-of-firebase-authentication-users"
draft: false
images: []
weight: 9916
type: docs
toc: true
---

## How to save user profile data
Every authenticated user has a Firebase `uid` that's unique across all providers and is returned in the result of every authentication method.

A good way to store your user's data is to create a node to keep all the users's data and to protect it using your security rules

**- Database**

    {
       "users": {
          "uid1" : {
             "name": "Steve",
             "surname": "Jobs"
          },
          "uid2" : {
             "name": "Bill",
             "surname": "Gates"
          }
       }
    }

**- Security**

    {
        "rules": {
            "users": {
                "$uid": {
                    // If node's key matches the id of the auth user
                    ".write": "$uid == auth.uid"
                }
            }
        }
    }

The `$uid` in the above rules is a so-called "dollar variable", which ensures that the rules under it are applied to all child nodes of `users`. For more information see the documentation on [Using $ Variables to Capture Path Segments](https://firebase.google.com/docs/database/security/securing-data#using_variables_to_capture_path_segments).

## Why save user data in the database
[Firebase Authentication](https://firebase.google.com/docs/auth/) allows the users of your app to sign-in with social providers or their email+password. But what if you want to store additional information about a user, beyond what Firebase Authentication allows you to specify?

Or what if you want to display a list of the users in your app? Firebase Authentication doesn't have an API for this.

Most developers solve this problem by storing the additional information in a separate database. This topic covers how to store such information in the [Firebase Realtime Database](https://firebase.google.com/docs/database/).

## Handling User Account Data in the Realtime Database
The Firebase auth system is the source of a users `uid`, `displayName`, `photoURL`, and maybe `email`.  Password based accounts set these _persistent_ values in the auth system via the `.updateProfile` method.  Storing these values in the Realtime Database, rDB, `users` node poses the issue of stale data.  Display names, for example, may change. To keep these values in synch use [local storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage) in concert with `.onAuthStateChange`.

on every `.onAuthStateChange`
 - `getItem('displayName')` and `getItem('photoURL')`
 - compare to `user.displayName` and `user.photoURL`
 - if different
    - `setItem('displayName')` and `setItem('photoURL')`
    - `db.ref.child('users').update` the values of `displayName` and/or `photoURL`

`.onAuthStateChange` fires on every page load or reload, as well as on every auth state change.  It potentially fires often, e.g. multi page apps.  However reading and writing to local storage is synchronous and very fast so there will be no noticeable impact on app performance.

