---
title: "Using Firebase with Node"
slug: "using-firebase-with-node"
draft: false
images: []
weight: 9817
type: docs
toc: true
---

## Hello World Firebase Realtime Database in Node
**System Requirements:**

 - [Node JS][1]


**Getting Started**

 1. First Go to [Firebase Console][2] and Create New Project.
 2. After Creating the project, in project click on settings icon besides project Name in left sidebar and select Permissions.
 3. On Permissions Page Click on Service accounts in left sidebar then click on Create Service Account
 4. In the popup window enter your service account name and choose Account Role and select Furnish a new private key and after that select JSON and click Create(Leave Enable Google App Domain-wide Delegation Unchecked).
 5. When you click create it will download a JSON file with your Account Credentials, just save the file Anywhere in your System.
 6. Next step is to Create a Database in your Firebase Console for which Go to Firebase Console and click on Database in left-sidebar. After that just create a new Database Object with Name user_data with some dummy value.
 7. Now your Firebase Database project is setup now simply copy following code in your project directory.


    //Loading Firebase Package
    var firebase = require("firebase");
    
    /**
    * Update your Firebase Project
    * Credentials and Firebase Database
    * URL
    */
    firebase.initializeApp({
      serviceAccount: "<path to Firebase Credentials Json File>",
      databaseURL: "<Firebase Database URL>"
    });  //by adding your credentials, you get authorized to read and write from the database
   
    
    /**
    * Loading Firebase Database and refering 
    * to user_data Object from the Database
    */
    var db = firebase.database();
    var ref = db.ref("/user_data");  //Set the current directory you are working in
    
    /**
    * Setting Data Object Value
    */
    ref.set([
    {
        id:20,
        name:"Jane Doe",
        email:"jane@doe.com",
        website:"https://jane.foo.bar"
    },
    {
        id:21,
        name:"John doe",
        email:"john@doe.com",
        website:"https://foo.bar"
    }
    ]);
    
    /**
    * Pushing New Value
    * in the Database Object
    */
    ref.push({
        id:22,
        name:"Jane Doe",
        email:"jane@doe.com",
        website:"https://jane.foo.bar"
    });
    
    /**
    * Reading Value from
    * Firebase Data Object
    */
    ref.once("value", function(snapshot) {
      var data = snapshot.val();   //Data is in JSON format.
      console.log(data);
    });


 8.  Just change with the JSON Credentials file URL(For starters just copy the credentials file in Same folder and in index.js file just add the credentials File Name).
 9. Next step is to change the in index.js with actual Firebase Database URL, you will be able to find this URL in Firebase Console in Database Tab, The URL will be like *https://<something>.firebaseio.com/*.
 10. The final step is to do

    npm install firebase

 11. After Executing above command NPM will install necessary packages required for Firebase. Finally to run and test project execute

    node index.js

**What does the project actually do?**

The project loads the Data from cloud based Firebase Database. The project also demonstrates how to Write and Read data from a Firebase Data Object.

In order to view your data get updated in realtime, go to [your console][2] click on the project you made, and on the left, hit Database. There, you can see your data get updated in real-time, along with their values. 


  [1]: https://nodejs.org/en/
  [2]: https://console.firebase.google.com/
    
   

## Firebase-queue and worker
You can push tasks or data to the firebase realtime database and run a worker which listens to the firebase queue  to run some background processess

**Setup firebase**

 1. Create a Firebase project in the Firebase console, if you don't already have one. If you already have an existing Google project associated with your app, click Import Google Project. Otherwise, click Create New Project.\.
 2. Click settings icon and select Permissions.
 3. Select Service accounts from the menu on the left.
 4. Click Create service account.
    
    Enter a name for your service account. 
    
    You can optionally customize the ID from the one automatically generated from the name.
    
    Choose Project > Editor from the Role dropdown.
    
    Select Furnish a new private key and leave the Key type as JSON.
    
    Leave Enable Google Apps Domain-wide Delegation unselected.
    
    Click Create

 5. When you create the service account, a JSON file containing your service account's credentials is downloaded for you. You'll need this to initialize the SDK in the server.
 
**Setup server**

Install firebase-queue using npm in your nodejs app

    npm install firebase firebase-queue --save

Once you've installed firebase and firebase-queue, you can get started by creating a new Queue and passing it your Firebase reference and a processing function.

Now lets create a firebase queue task from the app when a new user is created and set worker to listen for firebase-queue task and send an email to the created users mail.

***server.js**

    var app=express();
    var Queue = require('firebase-queue'),
        Firebase = require('firebase');
   
  Update your Firebase Project Credentials and Firebase Database URL

    var firebase = Firebase.initializeApp({
        serviceAccount: "path/to/serviceAccountCredentials.json",
      databaseURL: "https://databaseName.firebaseio.com"
    });

or you can input firebase credentials directly as below

    var firebase = Firebase.initializeApp({ 
        serviceAccount: {
            projectId: "projectId",
            clientEmail: "foo@projectId.iam.gserviceaccount.com",
            privateKey: "-----BEGIN PRIVATE KEY-----\nkey\n-----END PRIVATE KEY-----\n"
        },
        databaseURL: "https://databaseName.firebaseio.com"
    });

    
    var refQueue = firebase.database().ref("queue/tasks");

    createUser =  funtion(email, password){
        var user = {
            username: email,
            password: password
        };
        user = new db.users(user);
        user.save(function(err, user){
            if(!err){
                refQueue.push({case: "NEW_USER", data: user});
            }
        })
    }
    
    createUser("abc@abc.com", "password");

***worker.js**
    
    var Queue = require('firebase-queue'),
        Firebase = require('firebase');
   
    //Update your Firebase Project Credentials and Firebase Database URL by one of the way specified in server.js
    var firebase = Firebase.initializeApp({
        serviceAccount: "path/to/serviceAccountCredentials.json",
      databaseURL: "https://databaseName.firebaseio.com"
    });

    var refQueue = firebase.database().ref("queue");
    
    var queue = new Queue(refQueue, function(data, progress, resolve, reject) {
        switch(data.case){
            case "NEW_USER":
                sendMail(data.data.email);
                console.log("user created");
                //sendMail function is not an inbuilt function and will not work unless you define and implement the function
                break;
    
        // Finish the task asynchronously
        setTimeout(function() {
            resolve();
        }, 1000);
    });


run server and worker seperately and test around with firebase queue

    node server.js

    node worker.js


  [1]: https://firebase.google.com/docs/server/setup

