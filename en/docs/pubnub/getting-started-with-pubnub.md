---
title: "Getting started with PubNub"
slug: "getting-started-with-pubnub"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Publish on Subscribe Success (connect)
This example show how to subscribe, and once that is successful, publishing a message to that channel. It also demonstrates the full set of parameters that can be included in the `subscribe`'s `message` callback function.

<!-- language: lang-js -->

    pubnub = PUBNUB({                          
        publish_key   : 'your_pub_key',
        subscribe_key : 'your_sub_key'
    });

    pubnub.subscribe({                                     
        channel : "channel-1",
        message : function (message, envelope, channelOrGroup, time, channel) {
            console.log(
            "Message Received." + "\n" +
            "Channel or Group: " + JSON.stringify(channelOrGroup) + "\n" +
            "Channel: " + JSON.stringify(channel) + "\n" +
            "Message: " + JSON.stringify(message) + "\n" +
            "Time: " + time + "\n" +
            "Raw Envelope: " + JSON.stringify(envelope)
        )},
        connect:    pub,
        disconnect: function(m) {console.log("DISCONNECT: " + m)},
        reconnect:  function(m) {console.log("RECONNECT: " + m)},
        error:      function(m) {console.log("ERROR: " + m)}
    });
 
    function pub() {
       pubnub.publish({                                    
            channel : "channel-1",
            message : {"msg": "I'm Puuumped!"},
            callback: function(m) {console.log("Publish SUCCESS: " + m)},
            error: function(m) {console.log("Publish ERROR: " + m)}
       })
    };

