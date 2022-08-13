---
title : PubNub Tutorial
slug : pubnub-tutorial
weight : 9939
draft : false
images : []
type : docs
---

This is an simple, yet thorough, example of initializing PubNub, subscribing to a channel and publishing to that channel. 

 - Once you init PUBNUB, you can subscribe to a channel. 
 - The `connect` callback indicates that subscription to the channel was successful, so we call our `pub` function which performs a `publish` to the channel we just subscribed to. 
 - This published message will be sent to the PubNub network which will send the message to all active subscribers. In this case, it is just us so we will receive that message in our `message` callback where we are displaying the various attributes of the received message to our browser's Console.

In a real world use case, you would update your web page UI to display the received message.

See also: [latest/official PubNub JavaScript SDK Docs][1]


  [1]: https://www.pubnub.com/docs/javascript/pubnub-javascript-sdk-v4

