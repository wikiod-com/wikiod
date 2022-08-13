---
title: "Firebase and AppScript  Introduction"
slug: "firebase-and-appscript--introduction"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Integrate Firebase with Google AppScript to Read and Write Data in the Firebase Database.

Firebase is a NoSQL database system by Google that uses realtime database to help create and host applications on mobile, desktop and tablet devices. NoSQL databases use the JSON objects to store the data in structured format.

## Connecting to a Firebase project in GAS and transferring data from Google Spreadsheet to Firebase
## Install Firebase resource in the the AppScript ##

 - To do that click on Resources and then on Libraries.
 - Firebase has a unique project library key that need to be installed in the AppScript.
[![][1]][1]
 - Click on Libraries 
    The following pop-up appears. Enter the following project key in the textbox.
    **MYeP8ZEEt1ylVDxS7uyg9plDOcoke7-2l**
    This is the project library key for Firebase.
[![enter image description here][2]][2]
 - Now in the version choose the stable public release version.
[![enter image description here][3]][3]
 - Click on Save. Now Firebase is successfully installed in your AppScript for you to work.

## Now let's take an example for reading and writing data from Firebase. ## 

- Now we take a sample table designed in Google Sheets.
[![enter image description here][4]][4]
- Now to build the database in the Firebase using this table in the sheets. Add the following code in the AppScript.

    

  

        function writeDataToFirebase() {
          var ss = SpreadsheetApp.openById("1LACsj0s3syAa9gvORdRWBhJ_YcXHybjQfHPgw3TLQ6g");
          var sheet = ss.getSheets()[0];
          var data = sheet.getDataRange().getValues();
          var dataToImport = {};
          for(var i = 1; i < data.length; i++) {
            var firstName = data[i][0];
            var lastName = data[i][1];
            dataToImport[firstName + '-' + lastName] = {
              firstName:firstName,
              lastName:lastName,
              emailAddress:data[i][2],
              semester:data[i][4],
              department:data[i][5],
            };
          }
          var firebaseUrl = "https://example-app.firebaseio.com/";
          var secret = "secret-key";
          var base = FirebaseApp.getDatabaseByUrl(firebaseUrl, secret);
          base.setData("", dataToImport);
        }

Replace the spreadsheet ID and the firebaseURL and the secret key.

## How to find the firebaseURL and the secret key? ##
- Go to your Firebase Dashboard and click on settings gear at top left corner. Click on Project Settings.
[![enter image description here][5]][5]
- Go to Service Accounts section you can find the databaseURL. This serves as the firebaseURL.
- Now click on Database Secrets tab and you can find the secret key.

## Now you have inserted the firebaseURL and the secret key. Now you are all set to go. Click on run code in the AppScript engine. ## 

- It will ask to review Permissions first time when you run.
- Click Review Permissions and Allow.
- Now you run your function and you can see the table created in Firebase Database.

**To see the database go to the Firebase dashboard and Click on the Database you can view the database.**

  [1]: https://i.stack.imgur.com/YrfXf.png
  [2]: https://i.stack.imgur.com/HpxbH.png
  [3]: https://i.stack.imgur.com/cma3Q.png
  [4]: https://i.stack.imgur.com/Ad15u.png
  [5]: https://i.stack.imgur.com/E0mlW.png

## Some more functions to implement read and write.
## 1.    To write a simple data to test whether connection is working or not. ##

    function myFunction(){
     var firebaseUrl = "https://example-app.firebaseio.com/";
     var secret = "secret-key";
     var base = FirebaseApp.getDatabaseByUrl(firebaseUrl, secret);
     base.setData("test", "Hello Firebase"); 
    }

## 2.    To read all the Data ##

    function getAllData() {
             var firebaseUrl = "https://example-app.firebaseio.com/";
             var secret = "secret-key";
    var base = FirebaseApp.getDatabaseByUrl(firebaseUrl, secret);
     var data = base.getData();
              for(var i in data) {
                Logger.log(data[i].firstName + ' ' + data[i].lastName);
              }
    }
**The data read is displayed in the Logs. To check logs click on click on View → Logs or simply use Control + Enter.**

## 3.    To read a specific record ##

    function getContact() {
      var firebaseUrl = "https://example-app.firebaseio.com/";
      var secret = "secret-key";
      var base = FirebaseApp.getDatabaseByUrl(firebaseUrl, secret);
      var contact = base.getData("Yash-Udasi");
      Logger.log(contact);
    }

**The data read is displayed in the Logs. To check logs click on click on View → Logs or simply use Control + Enter.**

## 4.    To update a specific record. ##

    function updateData() {
      var firebaseUrl = "https://example-app.firebaseio.com/";
      var secret = "secret-key";
      var base = FirebaseApp.getDatabaseByUrl(firebaseUrl, secret);
      base.updateData("Yash-Udasi/emailAddress", "yash.udasi@fyuff.com");
    }




