---
title: "Getting started with google-cloud-messaging"
slug: "getting-started-with-google-cloud-messaging"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Send downstream messages from the cloud
Send a message using GCM HTTP connection server protocol:

      https://gcm-http.googleapis.com/gcm/send
      Content-Type:application/json
      Authorization:key=AIzaSyZ-1u...0GBYzPu7Udno5aA
      {
        "to": "/topics/foo-bar",
        "data": {
          "message": "This is a GCM Topic Message!",
         }
      }

To do this in [Postman][1], you simply have to set the following (*some details are as what is mentioned above*):

1. Set request type to `POST`
2. In the *Headers*, set the following:
    - Content-Type = application/json
    - Authorization = < Your GCM Server Key >
3. Set the payload parameters in the *Body* (*in this example, we used the *raw* option, see screenshot (2)*)
4. Send the request to https://gcm-http.googleapis.com/gcm/send

Screenshots:

(1)
[![enter image description here][2]][2]

(2)
[![enter image description here][3]][3]

Notice that the request was a success with the `message_id` in the response.

<sub> PS: I'm keeping the sample Server Key visible so that others can still try it out even if they haven't created a Project yet. **BUT, note that the Server Key must be always kept secret.** </sub>

  [1]: https://chrome.google.com/webstore/detail/postman/fhbjgbiflinjbdggehcddcbncdddomop?hl=en
  [2]: https://i.stack.imgur.com/3zSu7.png
  [3]: https://i.stack.imgur.com/TBdWA.png

## Handling downstream message in Android
Implement `onMessageReceived` that will catch the notification sent from GCM server.

      @Override
      public void onMessageReceived(String from, Bundle data) {
         String message = data.getString("message");
         Log.d(TAG, "From: " + from);
         Log.d(TAG, "Message: " + message);
         // Handle received message here.
      }

## Handling downstream message in iOS
To receive the notification, implement `application:didReceiveRemoteNotification:fetchCompletionHandler:` (or `application:didReceiveRemoteNotification:` for iOS < 8.0), and call `GCMService:appDidReceiveMessage:message` to acknowledge the reception of the message to GCM.

    - (void)application:(UIApplication *)application
        didReceiveRemoteNotification:(NSDictionary *)userInfo {
      NSLog(@"Notification received: %@", userInfo);
      // This works only if the app started the GCM service
      [[GCMService sharedInstance] appDidReceiveMessage:userInfo];
      // Handle the received message
      // ...
    }
    
    - (void)application:(UIApplication *)application
        didReceiveRemoteNotification:(NSDictionary *)userInfo
        fetchCompletionHandler:(void (^)(UIBackgroundFetchResult))handler {
      NSLog(@"Notification received: %@", userInfo);
      // This works only if the app started the GCM service
      [[GCMService sharedInstance] appDidReceiveMessage:userInfo];
      // Handle the received message
      // Invoke the completion handler passing the appropriate UIBackgroundFetchResult value
      // ...
    }

