---
title: "Firebase Realtime Database Transactions"
slug: "firebase-realtime-database-transactions"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

Transactions provide a mechanism to coordinate between multiple parties that might be accessing the same data at the same time. These "parties" might be different instances of the same code like different users running the same application or nodes in a server cluster, parts of the same program or event different programs like an administration application, a "end user" application and/or "backend" server logic.

## A distributed counter
Imagine many users all running a web application that is trying to increment a counter in the database. Each user must read the current count, add one and write out the updated value. To make sure no one reads the counter while someone else is is adding one we use a transaction:

    ref.transaction(function(value){
      if (value === null) {
        // the counter doesn't exist yet, start at one
        return 1;
      } else if (typeof value === 'number') {
        // increment - the normal case
        return value + 1;
      } else {
        // we can't increment non-numeric values
        console.log('The counter has a non-numeric value: ' + value)
        // letting the callback return undefined cancels the transaction
      }
    });


