---
title: "Presence"
slug: "presence"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

## Presence in JavaScript
Presence works by sending messages when a user joins, leaves, or times out from a particular channel. You can listen for these messages to track who is in a channel, and how long since they did anything.

First, make sure each user as a UUID. Set this when you initialize PubNub:

<!-- language: lang-js -->
```
var pubnub = PUBNUB({
     publish_key: 'my_pub_key',
     subscribe_key: 'my_sub_key',
     uuid: '1234_some_uuid'
 });
```

Now when you connect to a channel, add an extra listener for `join` events.

<!-- language: lang-js -->
```
pubnub.subscribe({
    channel: "channel-1",
    message: function(m){console.log(m)}
    presence: onPresenceEvent,
});

onPresenceEvent = function(message, envelope, channel){
    if (!message.action) {
        // presence interval mode happens
        // when occupancy > presence announce max
        // there is no action key
        console.log("Presence Interval Mode: occupancy = " + m.occupancy);
        return;
    }

    console.log(
        "Action:    " + message.action + "\n" +
        "UUID:      " + message.uuid + "\n" +
        "Channel:   " + JSON.stringify(channel) + "\n" +
        "Occupancy: " + message.occupancy + "\n" +
        "Timestamp: " + message.timestamp);

    else if (m.action == 'join') {
        // new subscriber to channel
        // add the user to your buddy list
    }
    else if (m.action == 'leave') {
        // subscriber explicitly unsubscribed channel
        // remove user from your buddy list
    }
    else if (m.action == 'timeout') {
        // subscriber implicitly unsubscribed channel (did not unsubscribe)
        // remove user from your buddy list        
    }
    else if (m.action == 'state-change') {
        // subscriber changed state
        // update the attributes about the user in the buddy list
        // i.e. - is typing, online status, etc.
        console.log("State Data: " + JSON.stringify(message.data));
    }
};
```

The message object sent to the `presence` callback will include the action taken (join, leave, timeout or state-change) and the UUID of the user who did the action, as well as timestamp and some other metadata.

When state is set, a `state-change` event is sent which will include the new state in the `data` key of the `message` key.

*Note*: If you have the Access Manager enabled, you *must* ensure that your grants cover both the regular channel as well as the presence channel. Otherwise, when you attempt to subscribe to the channel with a presence callback, the SDK will also subscribe you to the presence channel which will fail if you have not applied the grants. The presence channel name is the regular channel name with a "-pnpres" suffix; meaning a channel named "pubnub-sensor-array" will have a presence channel named "pubnub-sensor-array-pnpres". See the [Access Manager](https://www.wikiod.com/pubnub/access-manager) examples for more information.

## Setting State Upon Subscribe
When a user subscribes to a channel, you may want to [set state][1] for that newly subscribed user. While there is a *subscribe with state* API, there are some scenarios where this is not the most optimal/reliable technique (like during a disconnect/reconnect situation - the state will be lost and not reinstated).

It is better to explicitly set state once the channel is successfully subscribed. This means you use the `subscribe`'s `connect` callback to set the state.

<!-- language: lang-js -->

    var pubnub = PUBNUB({
        publish_key: 'my_pub_key',
        subscribe_key: 'my_sub_key',
        uuid: 'users_uuid'
    });

    pubnub.subscribe({
        channel: 'channel-1',
        message: function(msg, env, ch){console.log(msg)},
        connect: function(m) {
            console.log('CONNECT: ' + m);
            pubnub.state({
                channel  : 'channel-1', // use the channel param from the subscribe
                state    : {'nickname': 'Bandit', 'mood': 'Pumped!'},
                callback : function(m){console.log(m)},
                error    : function(m){console.log(m)}
            });
        },
        disconnect : function(m){console.log('DISCONNECT: ' + m)},
        reconnect  : function(m){console.log('RECONNECT: ' + m)},
        error      : function(m){console.log('CONNECT: ' + m)}
    });


  [1]: https://www.pubnub.com/docs/web-javascript/presence#setting_custom_presence_state_set_state

