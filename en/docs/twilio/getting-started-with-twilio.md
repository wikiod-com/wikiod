---
title: "Getting started with Twilio"
slug: "getting-started-with-twilio"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Sending an SMS Message from a US Number
This is how to send an SMS text message from a US number using Twilio's Node.js SDK.

First you need to install the Node.js client using:

    npm install twilio

Then, you have to [create an account on their website][1].

Once you have an account, you'll need the account SID and auth token that you can find on the online dashboard.

[![Location of Account SID and auth token on online dashboard][2]][2]

In the code example below, replace `[Account SID]` and `[Auth Token]` with the ones from your account.

<!-- language: lang-js -->
    // Twilio Credentials 
    var accountSid = '[Account SID]'; 
    var authToken = '[Auth Token]'; 
     
    //require the Twilio module and create a REST client 
    var client = require('twilio')(accountSid, authToken); 
     
    client.messages.create({ 
        to: "+16518675309", // Any number Twilio can deliver to
        from: "+14158141829", // A number you bought from Twilio and can use for outbound communication
        body: "Hey Jenny, thanks for the pull request, will merge it right away." 
    }, function(err, message) { 
        console.log(message.sid); 
    });
    


  [1]: https://www.twilio.com/try-twilio
  [2]: https://i.stack.imgur.com/9rOMX.gif

## Sending an SMS Message using Promises
Twilio's Node.JS API natively supports promises, allowing you to use promises when sending SMS messages (this example was taken and adapted directly from [Twilio's API Docs][1]).

<!-- language: lang-js -->
    // Create an authenticated Twilio REST API client
    var twilio = require('twilio');
    var client = new twilio.RestClient('ACCOUNT_SID', 'AUTH_TOKEN');
    
    // A simple example of sending an sms message using promises
    var promise = client.makeCall({
        to:'+16515556667777', // a number to call
        from:'+16518889999', // a Twilio number you own
        body: 'Hello, world.' // A URL containing TwiML instructions for the call
    });
    
    // You can assign functions to be called, at any time, after the request to
    // Twilio has been completed.  The first function is called when the request
    // succeeds, the second if there was an error.
    promise
    .then(function(sms) {
        console.log('Call success! SMS SID: ' + sms.sid);
    }, function(error) {
        console.error('Call failed!  Reason: ' + error.message);
    });
    


  [1]: https://twilio.github.io/twilio-node/

