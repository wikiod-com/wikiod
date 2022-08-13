---
title: "Stream Controller Channel Groups"
slug: "stream-controller-channel-groups"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

When using Channel Groups, you should not add or remove channels in your client side applications. This example shows adding channels to a channel group and subscribing to that channel group for simplicity sake. But in a real world scenario, you should have your server do all the add/remove of channels to/from channel groups. When you enable Access Manager, you will need the `manage` permission to add/remove channels to/from channel groups and you should never grant the `manage` permission to clients for security reasons. Only your server should be granted the `manage` permission.

## Subscribe with Channel Groups in JavaScript
With Stream Controller add-on enabled, you can use Channel Groups to subscribe to a 1000's of channels from a single client. You do this by creating a channel group and adding channels to the channel group. We'll assume `pubnub` variable has been initialized properly with your keys.

Create a generic callback handler function:

<!-- language: lang-js -->
    function displayCallback(m,e,c,d,f){
        console.log(JSON.stringify(m, null, 4));
    }


Create channel group and add channels to it:

<!-- language: lang-js -->
    pubnub.channel_group_add_channel({
      callback: displayCallback,
      error: displayCallback,
      channel_group: "sports",
      channel: "football,baseball,basketball,lacrosse,cricket"
    });

Now, subscribe to the channel group and you will be subscribed to all channels in that group:

<!-- language: lang-js -->
    pubnub.subscribe({
      callback: displayCallback,
      error: displayCallback,
      channel_group: "sports"
    });

Any messages published to the channels in the channel group will be received in the `displayCallback` function.

