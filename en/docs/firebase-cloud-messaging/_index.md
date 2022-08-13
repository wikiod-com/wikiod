---
title : firebase-cloud-messaging Tutorial
slug : firebase-cloud-messaging-tutorial
weight : 9992
draft : false
images : []
type : docs
---

A notorious common question is "how to send notifications from device to device", sadly the answer is: you can't. FCM needs to be triggered in order to send push notifications. That can be done in 3 different ways:

  1. Directly in the Firebase web console
  2. Setting a Firebase Functions listener and then triggering FCM
  3. A server requests to FCM to send a push notification

A push notification is an information payload that is sent from FCM. There are 3 types of push notifications: `notification`, `data`, `notification and data`. This information can be represented as a JSON:

      {
        "to" : "APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx...",
        "notification" : {
          "body" : "great match!",
          "title" : "Portugal vs. Denmark",
          "icon" : "myicon"
        },
        "data" : {
          "Nick" : "Mario",
          "Room" : "PortugalVSDenmark"
        }
      }

The above example is for the third type, `notification` and `data` combined. That is what will be asked to FCM to send.

  1. The console can send `notification` and `notification` with `data` but never only `data`
  2. Functions and any Server can send the 3 types

The importance of the `notification` type is that allow applications to received default *pushes* empowering other teams such as marketing to increase application growth by simply using the web console without further coding needed beside adding the library to the project.

Please don't confuse the push notification, the `notification` type and visual notification, this last correspond to an Android class (commonly NotificationCompat).

The behavior of the push is different according to the type and if the app is in the foreground or in background. Not on foreground means, minimized or closed.

  1. `notification` will trigger a default visual notification **if the app is not in foreground**, this notification can be customized in the manifest, please see [documentation][1]. If the app is in the foreground, we have to customize the behavior inside the `onMessageReceived` method.
  2. `data` type behavior must always be customized.
  3. Combined `notification` and `data` if the app is **not in the foreground** will trigger default visual notification and `data` payload will be available when the user click. Since the launcher Activity is triggered when the visual notification is clicked then you have to literally `getIntent().getStringExtra("yourKey");` in that Activity to get the data. If the app is active (in the foreground), then you have to customize the behavior inside the `onMessageReceived` method and you get access to the `data` payload immediately.


To get the information payload you have to do it inside the `onMessageReceived` method, there the only available argument is the message:

 1. To get the `notification` you have to `remoteMessage.getNotification()` then you can get the body or title with the corresponding methods
 2. To get the `data` you have to `remoteMessage.getData().get("yourKey")`.

Is a good idea to add every *not null* verification, there will be several types of notifications arriving in advanced apps. A good strategy is to verify if each, the `notification` and the `data` are not null. A consequent usefull strategy will be to have always use a `type` key in the `data` notifications in order to do some flow control.

To send `data` from the Firebase web console, the advanced options must be opened.

The `notification` keys are limited, and indicated in the documentation. **The values in any type can be only String**.

If you have problems finding any documentation in Firebase, please go to the bottom of the page and change the language to "English", documentations are *thinner* in some other languages.

  [1]: https://firebase.google.com/docs/cloud-messaging/android/client

