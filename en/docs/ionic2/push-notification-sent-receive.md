---
title: "Push notification sent & receive"
slug: "push-notification-sent--receive"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

The SenderID that is present in the initialization example is a gcm sender id that is given to you by google.
It should also be present when you install the plugin 

    ionic plugin add phonegap-plugin-push --variable SENDER_ID="XXXXXXX" 

If you wish to add additional data to your push notifications look in to this link explaining how to add more typings
https://github.com/phonegap/phonegap-plugin-push/blob/master/docs/TYPESCRIPT.md

## Initialization
The push notification plugin requires an init an initialization which tells the plugin to start running using the sender id provided. 

      let push = Push.init({
          android: {
            senderID: "------------",
          },
          ios: {
            alert: "true",
            badge: true,
            sound: "false",
          },
          windows: {},
        });



## Registration
   The registration step registers the app with the device's system and returns a registration id

     import { Push, RegistrationEventResponse} from "ionic-native";
    
            //the push element is created in the initialization example
            push.on("registration", async (response: RegistrationEventResponse) => {
                        //The registration returns an id of the registration on your device
                        RegisterWithWebApi(response.registrationId);
        
            });

## Receiving a push notification
  To receive push notifications we are supposed to tell the plugin to listen to incoming push notifications. This step is done after initialization & registration

    import { Push, NotificationEventResponse} from "ionic-native";
          
            //the push element is created in the initialization example      
            push.on("notification", (response: NotificationEventResponse) => {
                let chatMessage: ChatMessage = <ChatMessage>{
                  title: response.title,
                  message: response.message,
                  receiver: response.additionalData.replyTo,
                  image: response.image
                };
                DoStuff(chatMessage));
           });

