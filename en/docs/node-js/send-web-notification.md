---
title: "Send Web Notification"
slug: "send-web-notification"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

## Send Web notification using GCM ( Google Cloud Messaging System)
Such Example is knowing wide spreading among **PWAs** (Progressive Web Applications) and in this example we're going to send a simple Backend like notification using **NodeJS** and **ES6**

1. Install Node-GCM Module : `npm install node-gcm`
2. Install Socket.io : `npm install socket.io`
3. Create a GCM Enabled application using [Google Console.][1]
4. Grabe your GCM Application Id (we will need it later on)
5. Grabe your GCM Application Secret code.
5. Open Your favorite code editor and add the following code : 

        'use strict';

        const express = require('express');
        const app = express();
        const gcm = require('node-gcm');
        app.io = require('socket.io')();
        
        // [*] Configuring our GCM Channel.
        const sender = new gcm.Sender('Project Secret');
        const regTokens = [];
        let message = new gcm.Message({
            data: {
                key1: 'msg1'
            }
        });
        
        // [*] Configuring our static files.
        app.use(express.static('public/'));
        
        // [*] Configuring Routes.
        app.get('/', (req, res) => {
            res.sendFile(__dirname + '/public/index.html');
        });
        
        // [*] Configuring our Socket Connection.
        app.io.on('connection', socket => {
            console.log('we have a new connection ...');
            socket.on('new_user', (reg_id) => {
                // [*] Adding our user notification registration token to our list typically hided in a secret place.
                if (regTokens.indexOf(reg_id) === -1) {
                    regTokens.push(reg_id);
        
                    // [*] Sending our push messages
                    sender.send(message, {
                        registrationTokens: regTokens
                    }, (err, response) => {
                        if (err) console.error('err', err);
                        else console.log(response);
                    });
                }
            })
        });
         
        module.exports = app

> PS : I'm using here a special hack in order to make Socket.io works
> with Express because simply it doesn't work outside of the box.

Now Create a **.json** file and name it : **Manifest.json**, open it and past the following : 

    {
        "name": "Application Name",
        "gcm_sender_id": "GCM Project ID"
    }

Close it and save in your application **ROOT** directory.

> PS : the Manifest.json file needs to be in root directory or it won't work.

In the code above I'm doing the following : 

1. I seted up and sent a normal index.html page that will use socket.io also.
2. I'm listening on a **connection** event fired from the **front-end** aka my **index.html page** (it will be fired once a new client successfully connected to our pre-defined link)
3. I'm sending a special token know's as the **registration token** from my index.html via socket.io **new_user** event, such token will be our user unique passcode and each code is generated usually from a supporting browser for the **Web notification API** (read more [here.][2] 
4. I'm simply using the **node-gcm** module to send my notification which will be handled and shown later on using [**Service Workers**][3]`.

This is from **NodeJS** point of view. in other examples I will show how we can send custom data, icons ..etc in our push message.

> PS : you can find the full working demo over [here.][4]

        

    


  [1]: https://console.cloud.google.com/
  [2]: https://developer.mozilla.org/en/docs/Web/API/notification
  [3]: https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API
  [4]: https://github.com/houssem-yahiaoui/webpush-notification

