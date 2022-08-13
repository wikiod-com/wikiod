---
title: "Sending messages in Node.js"
slug: "sending-messages-in-nodejs"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

Twilio allows you to send and receive text messages with Node.js by using the twilio-node library to make calls. This guide assumes you've already setup an account with Twilio and have your account SID and the authentication token from the [Twilio Console](www.twilio.com/console).

## Parameters
| Parameter | Details |
| ------ | ------ |
| to | A valid phone number to receive the message |
| from | A Twilio number that is assigned to you |
| body | The body of the text message limited to 1600 characters |
| StatusCallback | A URL that Twilio posts to when a message status changes |
| maxPrice | Set the maximum price of a message |
| validityPeriod | The number of seconds the message will remain in the Twilio queue |
| provideFeedback | Boolean value, when set to true|
| mediaUrl | A URL containing a gif, png or jpeg content that will be sent with the message  |

# 2.0 SDK Deprecation
Twilio has two versions of the twilio-node module, a 2.0 SDK and a 3.0 SDK. It is recommended to use the 3.0 SDK as the 2.0 SDK will be deprecated on the 8/31/2017.

> Deprecation notice: New functionality will only be added to the new library (Node Helper Library 3.x). The old library (2.x) will be officially supported until 8/31/2017. After that day, Twilio will stop providing bug fixes and Support might ask you to upgrade before debugging issues. https://www.twilio.com/docs/libraries/node

# Parameter Reference
You can refer to the [Twilio REST documents][1] for a more detailed description. Twilio also has a [JSdocs][2] which can be used as a reference. 


  [1]: https://www.twilio.com/docs/api/rest/sending-messages
  [2]: https://twilio.github.io/twilio-node/3.0.0/Twilio.Api.V2010.AccountContext.MessageList.html

## Sending Your First Message
## Installing The Node Module

You can install this module by running the command below in your project directory:

```
npm install twilio
```

## Sending Your Message

    const accountSID = ''; // Obtained from the Twilio Console
    const authToken = '';   // Obtained from the Twilio Console
    
    const client = require('twilio')(accountSID, authToken);
    
    client.messages.create({
        to: '',  // Number you want to send to
        from: '', // From a valid Twilio number
        body: 'Hello, from Stack Overflow', // What appears in the text message
    })
    .then((message) => console.log(message.sid));

