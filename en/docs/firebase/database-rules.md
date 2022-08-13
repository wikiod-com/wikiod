---
title: "Database Rules"
slug: "database-rules"
draft: false
images: []
weight: 8000
type: docs
toc: true
---

With Firebase Realtime Database, your Database rules is your server side security. You need to be very careful and aware of who has access to your database. It is important that no one gains access to your data that shouldn't. 

By default, the Firebase Realtime Database rules allow any authenticated user to read and write all the data, this is probably not what you want your app to do. 

Take a look at the below examples for different scenarios.



The Firebase Realtime Database provides a flexible, expression-based rules language with JavaScript-like syntax to easily define how your data should be structured, how it should be indexed, and when your data can be read from and written to. Combined with our authentication services, you can define who has access to what data and protect your users' personal information from unauthorized access.

By default, your database rules require Firebase Authentication and grant full read and write permissions only to authenticated users. The default rules ensure your database isn't accessible by just anyone before you get a chance to configure i

Official Documentation
----------------------
https://firebase.google.com/docs/database/security/quickstart

## How to grant access only to authenticated users
Here's an example of a rule that gives each authenticated user a personal node at `/users/$user_id` where $user_id is the ID of the user obtained through **Authentication**.


    // These rules grant access to a node matching the authenticated
    // user's ID from the Firebase auth token
    {
      "rules": {
        "users": {
          "$user_id": {
            ".read": "$user_id === auth.uid",
            ".write": "$user_id === auth.uid"
          }
        }
      }
    }

## How to allow reading specific item from group, but prevent listing group members
It is common practice to create groups of items by creating simple value nodes with item ID as key. For example, we can add a user to the group "administrators" by creating a node at **`/administrators/$user_id`** with a value `true`. We don't want anyone to know who administrators are, for security reasons, but we still want to check if a Authenticated user is **administrator**. With these rules we can do just that:

    {
      "rules": {
        "administrators": {
          // No one can list administrators
          ".read": "false",
          "$uid": {
            // Authenticated user can check if they are in this group
            ".read": "$uid === auth.uid",
            // Administrators can write
            ".write": "data.parent().child(auth.uid).val() === true",
            // Allow only add or delete, no duplicates
            ".validate": "!data.exists() || !newData.exists() || newData.isBoolean()",
          }
        }
      }
    }


## The default rules
The default rules require Authentication.   
They allow full read and write access to authenticated users of your app. They are useful if you want data open to all users of your app but don't want it open to the world.

    // These rules require authentication
    {
      "rules": {
        ".read": "auth != null",
        ".write": "auth != null"
      }
    }

## How to set your files publicly readable and writable
Just define:

    // These rules give anyone, even people who are not users of your app,
    // read and write access to your database
    {
      "rules": {
        ".read": true,
        ".write": true
      }
    }


It can be useful during development but pay attention because This level of access means **anyone can read or write to your database**. 

## How to disable read and write access
You can define a private rules to disable read and write access to your database by users.   With these rules, **you can only access the database when you have administrative privileges (which you can get by accessing the database through the Firebase console or by [signing in from a server](https://firebase.google.com/docs/database/server/start))**.

    // These rules don't allow anyone read or write access to your database
    {
      "rules": {
        ".read": false,
        ".write": false
      }
    }

## How to configure rules
1. Go in the Firebase console.
2. Choose your project
3. Click on the Database section on the left, and then select the Rules tab.

If you would like to test your security rules before putting them into production, you can simulate operations in the console using the Simulate button in the upper right of the rules editor.

