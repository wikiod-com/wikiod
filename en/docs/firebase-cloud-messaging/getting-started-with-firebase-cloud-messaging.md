---
title: "Getting started with firebase-cloud-messaging"
slug: "getting-started-with-firebase-cloud-messaging"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Firebase Cloud Messaging is the Firebase service that handles push notifications. You can add this service in any client: web, Android or IOS. The specific functioning for each must be read from the [documentation][1].

**For adding FCM in any type of project, is always adding a library**.

Considering the special support for Android is worthy to take a few lines for it. Create a new project using Android Studio, in the menu go to Tools/Firebase, it will trigger the Firebase assistant. Select "Cloud Messaging" and follow steps one and two. 

 1. If your project previously adds another Firebase service, then the
    step one will be marked as completed, otherwise, you have to do it.
    The first step allows you to create a project in Firebase or create a new one. This step will download a google-service.json file which has the configuration to connect with the Firebase project. This file is inside the "app" folder.
 2. This step adds the Google Services library and the Firebase library to the gradle, it will also do some extra configuration in those files too.

This is the basis for adding FCM in a project. From this point on, the client is already able to receive FCM push notifications that contain a "notification" payload as long as the app is not in foreground (more details in the remarks).

To further customize the FCM behavior in the client we need to add 2 services, this is well [documented][2] in the official site. Again we will take some consideration for Android:

  1. Create a class that extends `FirebaseMessagingService` and override the onMessageReceived method
  2. Create a class that extends `FirebaseInstanceIdService` and override the onTokenRefresh method
  3. Register both classes in the manifest, please do this inside the `application` tag
    <service android:name=".IdService">
       <intent-filter>
           <action android:name="com.google.firebase.INSTANCE_ID_EVENT" />
       </intent-filter>
    </service>
    <service android:name=".MessageService">
       <intent-filter>
           <action android:name="com.google.firebase.MESSAGING_EVENT" />
       </intent-filter>
    </service>

You can get the `notification` payload and the `data` payload inside the `onMessageReceived` method using the only argument there. The `onTokenRefresh` method is called when the FCM token is assigned by FCM. An FCM token is a unique id for the app installation and the device and can be used as an address of the device to directly send push notifications.

Please read remarks for more information about the types of notification and the associated behavior.


  [1]: https://firebase.google.com/docs/cloud-messaging/
  [2]: https://firebase.google.com/docs/cloud-messaging/android/client

