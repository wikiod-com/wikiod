---
title: "Firebase Push Notification Cordova"
slug: "firebase-push-notification-cordova"
draft: false
images: []
weight: 9578
type: docs
toc: true
---

## Firebase Push Notification in Cordova Android

<b>Add Firebase to Your Android Project</b>

<i>Add Firebase to your app</i>

To add Firebase to your app you'll need a Firebase project and a Firebase configuration file for your app.

1.  Create a Firebase project in the Firebase console, if you don't already have one. If you already have an existing Google project      associated with your mobile app, click Import Google Project. Otherwise, click Create New Project.
2.  Click Add Firebase to your Android app . If you're importing an existing Google project, this may happen automatically and you can     just download the config file.
3.  When prompted, enter your app's package name. It's important to enter the package name your app is using; this can only be set    
    when you add an app to your Firebase project.
4.  At the end, you'll download a google-services.json file. You can download this file again at any time.
    If you haven't done so already, copy this into your project's module folder, typically app/.


Cordova Firebase Push Notification Plugin
------------------------------------------
https://www.npmjs.com/package/cordova-plugin-fcm

For obtaining the access token:

        FCMPlugin.getToken(
          function(token){
            alert(token);
          },
          function(err){
            console.log('error retrieving token: ' + err);
          }
        );
        
Callback for receiving push notification:

        FCMPlugin.onNotification(
          function(data){
            if(data.wasTapped){
              //Notification was received on device tray and tapped by the user.
              alert( JSON.stringify(data) );
            }else{
              //Notification was received in foreground. Maybe the user needs to be notified.
              alert( JSON.stringify(data) );
            }
          },
          function(msg){
            console.log('onNotification callback successfully registered: ' + msg);
          },
          function(err){
            console.log('Error registering onNotification callback: ' + err);
          }
        );
        
  Place the get access token and callback for receiving push notification inside index.js file within receivedEvent function
  
  Sending Push Notification via REST API
  
        //POST: https://fcm.googleapis.com/fcm/send 
        //HEADER: Content-Type: application/json 
        //HEADER: Authorization: key=AIzaSyAMMh0mdVIRXPcBejyatAtdZgmklepwoNs //key is server-key
        {
          "notification":{
            "title":"Notification title",  //Any value 
            "body":"Notification body",  //Any value 
            "sound":"default", //If you want notification sound 
            "click_action":"FCM_PLUGIN_ACTIVITY",  //Must be present for Android 
            "icon":"fcm_push_icon"  //White icon Android resource
          },
          "data":{
            "param1":"value1", /Any data to be retrieved in the notification callback 
            "param2":"value2"
          },
            "to":"eRImo7algBM:APA91bHSxSOdmgsOi9su_XytEtCbei0Zi0ODgm76VHvbqeb-WPoZcLyNVpnaLWPLw7U1u93hO0ZhtBxn_hVGxPAwxXXfc-yNy6_kkfzUdTpcI2QPB0vzJBmOFzX3RRZ15wmFkCUFtyhc", //Topic or single device 
            "priority":"high", //If not set, notification won't be delivered on completely closed iOS app
            "restricted_package_name":"com.zensar.fcm" //Optional. Set for application filtering 
        }
        
  Configure the above REST API using Postman rest client.
  <a href="http://imgur.com/oL8fkcn"><img src="http://i.imgur.com/oL8fkcn.png" title="source: imgur.com" /></a>
  <a href="http://imgur.com/XQQfDSX"><img src="http://i.imgur.com/XQQfDSX.png" title="source: imgur.com" /></a>
  
<b>How it works</b>
<i>Send a push notification to a single device or topic.</i>

1.a Application is in foreground:
  The user receives the notification message in its device notification bar.
  The user taps the notification and the application is opened.
  The user receives the notification data in the JavaScript callback'.

1.b Application is in background:
  The user receives the notification message in its device notification bar.
  The user taps the notification and the application is opened.
  The user receives the notification data in the JavaScript callback'.
  

  


