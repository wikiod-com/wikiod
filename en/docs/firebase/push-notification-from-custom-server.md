---
title: "Push notification from custom server"
slug: "push-notification-from-custom-server"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

This can be done using 2 methods 
with **HTTP Post request**, With **Firebase admin SDK** running on your server. Here I will discuss both of them.



## Firebase Cloud Messaging HTTP Protocol
From your server request to the the link below to send the notification with some request parameters 

    https://fcm.googleapis.com/fcm/send
While requesting add headers as follows

    Authorization    key=<Your_key_from_the_console>
    Content-Type     application/json

The body of the request varies 

    {
      "to" : <tokens or the topic>,
      "notification" : {
        "title":"This is a test title",
        "body":"This is the body"
      },
      "data": {
          //whatever key value payer you need to send  
      }
    }

The to parameters takes Array of tokens like

    ["token1","token2",..........]
or a single token like

    "token"
or a topic name starting with **/topic/** like

    "/topic_name/"

For multiple topic use conditions using || and && operators like

    "/topic_name/ && /topic2/"

## Using Admin SDK(Node js)
At first initilize the firebase sdk and admin SDK

    const functions = require('firebase-functions');
    const admin = require('firebase-admin');
    
    admin.initializeApp({
      credential: admin.credential.cert({
            //your admin credential certificate generated from the console. Follow this [link][1].
        }),
      databaseURL: "https:///<PROJECT_NAME>.firebaseio.com"
    });

Create a payload JSON string as in the first example.

    var payload = {
                  notification: {
                    title: "Title of the notification,
                    body: "Body of the notification",
                  },
                  data:{
                    //required key value pair
                  }
                };

Then call different send methods to send the notification.

**For Topic**

    admin.messaging().sendToTopic("/topic/", payload)
                  .then(function(response) {
                    console.log("Successfully sent message:", response);
                  })
                  .catch(function(error) {
                    console.log("Error sending message:", error);
                  });
                });
**For device**
 

    admin.messaging().sendToDevice(token, payload).then(response=>{
                           response.results.forEach((result, index) => {
                            const error = result.error;
                            if (error) {
                              console.error('Failure sending notification to', tokens, error);
                            } else{
                              console.log('Sucessfully sent to '+tokens);
                            }
                          });

  [1]: https://firebase.google.com/docs/admin/setup#add_firebase_to_your_app





