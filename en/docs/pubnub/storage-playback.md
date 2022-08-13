---
title: "Storage & Playback"
slug: "storage--playback"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Parameters
| Parameter        | `Type / Required / Default` Description |
| ------           | ------ |
| channel          | `String / Yes` Specifies channel to return history messages from. |
| reverse          | `Boolean / No / false` Setting to true will traverse the time line in reverse starting with the oldest message first. Default is false. If both start and end arguments are provided, reverse is ignored and messages are returned starting with the newest message. |
| limit            | `Number / No / 100` Specifies the number of historical messages to return. |
| start            | `Number / No` Time token delimiting the start of time slice (exclusive) to pull messages from. |
| end              | `Number / No` Time token delimiting the end of time slice (inclusive) to pull messages from. |
| includeTimetoken | `Boolean / No / false` If true the message post timestamps will be included in the history response. |


Simply enabling *Storage & Playback* add-on for your keys in the PubNub Admin Dashboard will result in all published messages on all channels to be stored. You can prevent a message from being stored by passing the `storeInHistory` parameter as `false` when the message is published, like this:

<!-- language: lang-js -->
    pubnub.publish(
        {
            message: { 
                'price': 8.07
            },
            channel: 'channel1',
            storeInHistory: false // override default storage options
        },
        function (status, response) {
            // log status & response to browser console
            console.log("STATUS  : " + console.log(JSON.stringify(status));
            console.log("RESPONSE: " + console.log(JSON.stringify(response));
        }
    );

Otherwise, just omit the `storeInHistory` or set to `true` to store the message when it is published.

## JavaScript SDK v4 - History
<!-- language: lang-js -->

    // initialize pubnub object
    var pubnub = new PubNub({
        subscribeKey: "yourSubscribeKey",
        publishKey: "myPublishKey" // optional
    })
    
    // get up to the last 100 messages 
    // published to the channel
    pubnub.history(
        {
            channel: 'channel1'
        },
        function (status, response) {
            // log status & response to browser console
            console.log("STATUS  : " + console.log(JSON.stringify(status));
            console.log("RESPONSE: " + console.log(JSON.stringify(response));
        }
    );
    
    // this is the format of the response
    [
        [array of returned messages],
        "firstMessageTimetoken",
        "lastMessageTimetoken"
    ]
    
    // example of response
    [
        [{'price':10.02}, {'price':10.12}, {'price':10.08}, {'price':10.10}],
        "14691304969408991",
        "14691307326690522"
    ]


