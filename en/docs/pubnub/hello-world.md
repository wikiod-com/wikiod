---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

`<init>` (i.e. `require ("pubnub")`)

Initialize an instance of PubNub to invoke operations.

| Parameter | Details |  
| --------- | ------- |  
| publish_key | String - your publish key from your PubNub Admin Dashboard account |  
| subscribe_key | String - your publish key from your PubNub Admin Dashboard account |  


`subscribe`

Subscribe to a channel(s) and provide a means to receive messages published to the channel(s).

| Parameter | Details |  
| --------- | ------- |  
| channel | String - channel name or comma-delimited list of channel names |  
| message | function - the callback function that will receive messages published on the subscribe channels |  
| connect | function - the callback function that will be called when the subscription to the channels is successful |  

`publish`

Publish a message to a channel which will be received by subscribers on that channel.

| Parameter | Details |  
| --------- | ------- |  
| channel | String - channel name on which to send the message |  
| message | String - The message to publish on the channel. JSON format is recommended (do not stringify; use the JSON object)  |

## Publish and Subscribe for Node.JS SDK
# Publish and Subscribe for Node.JS

Install PubNub NPM Package.
```
npm install pubnub@3.15.2
```

Example Publish Subscribe with Node.JS

<!-- language: lang-js -->

```
var channel = "hello_world";
var pubnub = require("pubnub")({
    publish_key   : "your_pub_key"
,   subscribe_key : "your_sub_key"
});

pubnub.subscribe({
    channel : channel
,   message : receive  // print message
,   connect : send     // send message after subscribe connected
});

function receive(message) { console.log(message) }

function send(message) { 
    pubnub.publish({ 
        channel : channel 
    ,   message : message
    }); 
}
```

