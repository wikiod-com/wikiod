---
title: "Push Notification in Android and iOS"
slug: "push-notification-in-android-and-ios"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Using the new phonegap-plugin-push
For the purpose of sending push notifications to cordova apps. The first step is to obtain a device token. A "device token" is specific to each device and each project.

**Pre-requisite**: 

> 1. Google Cloud Messaging Project Number

 For this go to [Google Developer Console][1] and create a new project.<br>
 Under Project Information is the Project Number 

> 2. Google Cloud Messaging API Key for above Project (needed for server)
   
  Go to Library -> Google Cloud Messaging -> Enable. 
  Go to Credentials to create an API key of Type server.

  [![enter image description here][2]][2]

Adding the push-plugin to project:
   

    cordova plugin add https://github.com/phonegap/phonegap-plugin-push --variable SENDER_ID="XXXXXXX"

  >SENDER_ID represents the Project Id

Place the following code inside receivedEvent function within index.js


        var push = PushNotification.init({
            android: {
                senderID: "XXXXXX"
            },
            ios: {
                alert: "true",
                badge: "true",
                sound: "true"
            },
            windows: {}
        });

        push.on('registration', function(data) {
            console.log("device token: " + data.registrationId);
        });

        push.on('notification', function(data) {
               console.log(data.message);
               console.log(data.title);
               console.log(data.count);
               console.log(data.sound);
               console.log(data.image);
               console.log(data.additionalData);
        });

        push.on('error', function(e) {
               console.log(e.message)
        });


> On running the above code from an Android or iOS device gives a device token.

> NOTE: Device token shall be generated only on a real device not a virtual device.

> For testing push notification go to this link [Online Push Notification Test][3]
  
**For Android:** Enter the Device token, Message and API key

[![enter image description here][4]][4] 


  [1]: https://console.developers.google.com
  [2]: http://i.stack.imgur.com/Ripol.png
  [3]: http://apns-gcm.bryantan.info/
  [4]: http://i.stack.imgur.com/SygrS.png

