---
title: "Push notifications"
slug: "push-notifications"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

So if you wanna make web app notification I suggest you to use Push.js or SoneSignal framework for Web/mobile app.

Push is the fastest way to get up and running with Javascript notifications. A fairly new addition to the official specification, the Notification API allows modern browsers such as Chrome, Safari, Firefox, and IE 9+ to push notifications to a user’s desktop.

You will have to use Socket.io and some backend framework, I will user Express for this example.


## Parameters
| module/framework | description |
| ------ | ------ |
| node.js/express   | Simple backe-end framework for Node.js application, very easy to use and extremely powerful   |
| Socket.io   | Socket.IO enables real-time bidirectional event-based communication. It works on every platform, browser or device, focusing equally on reliability and speed.|
| Push.js | The world's most versatile desktop notifications framework |
|OneSignal| Just another form off push notifications for Apple devices |
|Firebase|Firebase is Google's mobile platform that helps you quickly develop high-quality apps and grow your business.|

## Web notification
First, you will need to install [Push.js][1] module.

    $ npm install push.js --save

Or import it to your front-end app through [CDN][2]

    <script src="./push.min.js"></script> <!-- CDN link -->

After you are done with that, you should be good to go. This is how it should look like if u wanna make simple notification:

    Push.create('Hello World!')

I will assume that you know how to setup [Socket.io][3] with your app. Here is some code example of my backend app with express:

    var app = require('express')();
    var server = require('http').Server(app);
    var io = require('socket.io')(server);

    server.listen(80);

    app.get('/', function (req, res) {
      res.sendfile(__dirname + '/index.html');
    });

    io.on('connection', function (socket) {
      
      socket.emit('pushNotification', { success: true, msg: 'hello' });

    });

After your server is all set up, you should be able to move on to front-end stuff. Now all we have to do is import Socket.io [CDN][4] and add this code to my *index.html* file:

    <script src="../socket.io.js"></script> <!-- CDN link -->
    <script>
      var socket = io.connect('http://localhost');
      socket.on('pushNotification', function (data) {
        console.log(data);
        Push.create("Hello world!", {
            body: data.msg, //this should print "hello"
            icon: '/icon.png',
            timeout: 4000,
            onClick: function () {
                window.focus();
                this.close();
            }
        });
      });
    </script>

There you go, now you should be able to display your notification, this also works on any Android device, and if u wanna use [Firebase][5] cloud messaging, you can use it with this module, [Here][6] is link for that example written by Nick (creator of Push.js)


  [1]: https://pushjs.org/
  [2]: https://cdnjs.com/libraries/push.js
  [3]: https://socket.io/
  [4]: https://cdnjs.com/libraries/socket.io
  [5]: https://firebase.google.com/
  [6]: https://github.com/Nickersoft/push-fcm-plugin

## Apple
Keep in mind that this will not work on Apple devices (I didnt test them all), but if you want to make push notifications check [OneSignal][1] plugin.


  [1]: https://onesignal.com/

