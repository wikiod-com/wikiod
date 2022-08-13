---
title: "webhook"
slug: "webhook"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## pubnub webhook
    Webhook Overview

A WebHook is an HTTP callback: an HTTP POST that occurs when something happens; a simple event-notification via HTTP POST. A web application implementing WebHooks will POST a message to a URL when certain things happen.

    PubNub Presence

Pubnub presence all about the user presence at pubnub platform. it provides the presence of user when they are joining, leaving a channel or when there is a user's state changes. Presence Webhooks provide a means for your server to be notified whenever presence events occur on any channel for your keys. This provides an easy to scale solution for your server side application to monitor the presence events.

    How it would reduce the overhead

Without Presence Webhooks, your server would have to subscribe to all the channels'  -pnpres channels. So this can be a tedious task to control overs channels if your app has thousands of channels or more.

Pubnub Webhooks would help us in this scenario and it is easier to implement and scale with traditional, well-known web infrastructure(load balancers, web and app servers provided by your application service providers like Heroku, Rackspace, Azure, Amazon and others).

    PubNub Presence Webhooks

PubNub Presence Webhooks are a means for the PubNub network to invoke a REST endpoint your server directly as presence events occur. It would also help in load balancing. So here you need to create REST Endpoint URL of your server at which pubnub would sends the presence data.

    User Presence Events

There are four user events at pubnunb platform
1. join
2. leave
3. timeout
4. state-change

And two channel level events : active and inactive. please refer pubnub-doc for detailed information.

Each event has it's own Webhook for which you can provide a REST endpoint to your server to handle the event. Or you can provide one REST endpoint for all of them and just implement conditional logic on the action attribute on your server to handle each individual event.

Whatever you choose, you need to provide the sub-key and the REST URIs to PubNub Support to configure this for you. You likely will have more than one sub-key with different endpoints for different server environments (dev, test, production, for example).

Once your server's REST endpoints are implemented and the PubNub key configuration is in place, you are ready to go. But before you implement the REST endpoints, it might be helpful to know what the events' data looks like. 

Here is an example of a join :

    HTTP POST
    Content-Type: application/json
    
    {
        'action': 'join',
        'sub_key': 'sub-c-...',
        'channel': 'lacrosse'
        'uuid': '1234-5678-90ab-cdef',
        'timestamp': 1440568311,
        'occupancy': 1,
        'data': {'foo': 'bar'}
    } 

This would be the same for leave and timeout and state-change except for the action value, of course.

    Webhook Response Status

It is important that your REST endpoint implementation should return a status code (200 OK) immediately upon receiving the Webhook from PubNub.

    Pubnub Re-try 

If pubnub does not recieve 200 From Rest-Endpoint then it will send duplicate events because PubNub assumes no response means your server did not receive the event. PubNub will wait five seconds for the 200 response before trying again. After a third retry (four total attempts), PubNub will no longer attempt to send that particular event to your server.

