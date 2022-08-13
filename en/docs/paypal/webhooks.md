---
title: "Webhooks"
slug: "webhooks"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| app | Our Express application reference |  
| bodyParser | The body-parser package reference for working with JSON encoded bodies |  
| clientId | The application client ID (OAuth 2 credentials) |  
| http | The http package for running the server |  
| paypal | The PayPal Node SDK reference object |  
| secret | The application secret (OAuth 2 credentials) |  
| webhookId | ID of the webhook to be modified |
| webhookUpdate | JSON object containing the webhook details to be updated |


These samples cover working examples of how to use PayPal webhooks to provide event monitoring for your application and payments.

## Testing Sandbox Webhooks with ngrok and Express (Node)
In this example we're going to look at testing webhook notifications in sandbox, using [ngrok][1] to provide a tunnel for our Node HTTP listener, running on localhost, to the internet. For this example, we're going to be using Node to set up notification webhooks for payment events (such as a payment being made), then set up the server to listen for incoming HTTP POST messages from the webhook events.

There are a few steps that we're going to follow here to make this happen:

 1. Set up a simple server to listen to incoming POST traffic from the webhooks, which will be the notification from PayPal, and start listening on localhost.
 2. Then use ngrok to provide a tunnel from localhost to the internet so that PayPal can post notification through.
 3. Lastly, subscribe our application (based on the credentials provided) to webhook events that we want to track, providing the public ngrok URI from step 2.

**Creating a Webhooks Listener**

The first thing that we need to do is create the listener. The reason why we're starting with the listener is because we need the ngrok live URL to provide to the webhooks when we create or update them.

    var bodyParser = require('body-parser'),
        http = require('http'),
        app = require('express')();
    
    app.use(bodyParser.json());
    
    app.post('/', function(req, res){
        console.log(JSON.stringify(req.body));
    });
    
    //create server
    http.createServer(app).listen(3001, function () {
       console.log('Server started: Listening on port 3001');
    });

Our listener is a simple route using Express. We listen for any incoming POST traffic, then spit out the POST body to the console. We can use this to do whatever we'd like with the listener when it comes in. 

When we create the HTTP server at the end, we set it up to listen on localhost port 3001. Run that script now to start listening for traffic.

**Using ngrok to Expose the Listener to the Internet**

With the listener set up on localhost:3001, our next job is to expose that script to the internet, so that traffic can be sent to it, which is the job of ngrok.

Run the following command from a terminal window:

    ngrok http 3001

That will initiate the process of providing a live tunnel for localhost on port 3001, and will provide the following information once run:

[![enter image description here][2]][2]

As our can see, the live address that we can use to point the PayPal webhook to our running listener on localhost is `http(s)://055b3480.ngrok.io`. That's all we need to know to set up the listener.

**Subscribing to Notifications**

Our last step is to create webhooks for our application, which will create notifications when certain events happen with payments, refunds, etc on our app. We only need to create these webhooks once to bind them to the application, so they do not need to be run each time you want to use them.

First we set up the PayPal environment by adding in the requirement for the PayPal Node SDK, our client ID / secret from creating an application, then configuring the environment for sandbox.

    var paypal = require('paypal-rest-sdk');
    
    var clientId = 'YOUR APPLICATION CLIENT ID';
    var secret = 'YOUR APPLICATION SECRET';
    
    paypal.configure({
      'mode': 'sandbox', //sandbox or live
      'client_id': clientId,
      'client_secret': secret
    });
    
Next, we set up the JSON structure for our webhooks. `webhooks` contains two pieces of information, the `url` that all webhook events should be sent to, and the `event_types` that we want to subscribe to. 

In the case of this sample, the `url` is set to our ngrok live URL, and the events we are listening for are cases where payments are completed or denied.

For a complete list of potential events, see https://developer.paypal.com/docs/integration/direct/rest-webhooks-overview/#event-type-support.

Lastly, we pass the `webhooks` object into the call to create the webhooks, `notification.webhook.create`. If successful, PayPal will send notifications to the endpoint we specified, which is running on localhost.

    var webhooks = {
        "url": "https://436e4d13.ngrok.io",
        "event_types": [{
            "name": "PAYMENT.SALE.COMPLETED"
        },{
            "name": "PAYMENT.SALE.DENIED"
        }
    ]};
    
    paypal.notification.webhook.create(webhooks, function (err, webhook) {
        if (err) {
            console.log(err.response);
            throw error;
        } else {
            console.log("Create webhook Response");
            console.log(webhook);
        }
    });

Once we issue a payment using those application credentials, information about the payment state will be sent to the endpoint that we set up.

An example of the POST body that PayPal sends as the notification might looks like the following, which was sent after a successful PayPal payment:

    {
      "id": "WH-9FE9644311463722U-6TR22899JY792883B",
      "create_time": "2016-04-20T16:51:12Z",
      "resource_type": "sale",
      "event_type": "PAYMENT.SALE.COMPLETED",
      "summary": "Payment completed for $ 7.47 USD",
      "resource": {
        "id": "18169707V5310210W",
        "state": "completed",
        "amount": {
          "total": "7.47",
          "currency": "USD",
          "details": {
            "subtotal": "7.47"
          }
        },
        "payment_mode": "INSTANT_TRANSFER",
        "protection_eligibility": "ELIGIBLE",
        "protection_eligibility_type": "ITEM_NOT_RECEIVED_ELIGIBLE,UNAUTHORIZED_PAYMENT_ELIGIBLE",
        "transaction_fee": {
          "value": "0.52",
          "currency": "USD"
        },
        "invoice_number": "",
        "custom": "",
        "parent_payment": "PAY-809936371M327284GK4L3FHA",
        "create_time": "2016-04-20T16:47:36Z",
        "update_time": "2016-04-20T16:50:07Z",
        "links": [
          {
            "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/sale\/18169707V5310210W",
            "rel": "self",
            "method": "GET"
          },
          {
            "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/sale\/18169707V5310210W\/refund",
            "rel": "refund",
            "method": "POST"
          },
          {
            "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/payment\/PAY-809936371M327284GK4L3FHA",
            "rel": "parent_payment",
            "method": "GET"
          }
        ]
      },
      "links": [
        {
          "href": "https:\/\/api.sandbox.paypal.com\/v1\/notifications\/webhooks-events\/WH-9FE9644311463722U-6TR22899JY792883B",
          "rel": "self",
          "method": "GET"
        },
        {
          "href": "https:\/\/api.sandbox.paypal.com\/v1\/notifications\/webhooks-events\/WH-9FE9644311463722U-6TR22899JY792883B\/resend",
          "rel": "resend",
          "method": "POST"
        }
      ]
    }
    
  [1]: https://ngrok.com/
  [2]: http://i.stack.imgur.com/UVwad.jpg

## Updating a Webhook with a New URL (Node Sample)
This sample will show you how to update an existing webhook forwarding URL (where the notifications should be POSTed to). To run this, you should have the ID provided back by PayPal when you first created your webhooks.

First, add the PayPal SDK and configure the environment (sandbox below).

    var paypal = require('paypal-rest-sdk');
    
    var clientId = 'YOUR APPLICATION CLIENT ID';
    var secret = 'YOUR APPLICATION SECRET';
    
    paypal.configure({
        'mode': 'sandbox', //sandbox or live
        'client_id': clientId,
        'client_secret': secret
    });

Next, set up the JSON structure and webhook details. Assign the ID for your webhook to `webhookId` first. Next, in the `webhookUpdate`, specify an operation of replace, set the `path` to `/url` to specify an update of that resource, and provide the new URL to replace it with under `value`.

    var webhookId = "YOUR WEBHOOK ID";
    var webhookUpdate = [{
        "op": "replace",
        "path": "/url",
        "value": "https://64fb54a2.ngrok.io"
    }];

Lastly, call `notification.webhook.replace(...)`, passing in `webhookId` and `webhookUpdate`. 

paypal.notification.webhook.replace(webhookId, webhookUpdate, function (err, res) {
    if (err) {
        console.log(err);
        throw err;
    } else {
        console.log(JSON.stringify(res));
    }
});

If all succeeds, an object similar to the following should be provided back from PayPal and, in the case of this sample, displayed in the terminal with the newly updated information.

    {  
        "id":"4U496984902512511",
        "url":"https://64fb54a2.ngrok.io",
        "event_types":[{  
            "name":"PAYMENT.SALE.DENIED",
            "description":"A sale payment was denied"
        }],
        "links":[{   
            "href":"https://api.sandbox.paypal.com/v1/notifications/webhooks/4U496984902512511",
            "rel":"self",
            "method":"GET"
        },{  
            "href":"https://api.sandbox.paypal.com/v1/notifications/webhooks/4U496984902512511",
            "rel":"update",
            "method":"PATCH"
        },{  
            "href":"https://api.sandbox.paypal.com/v1/notifications/webhooks/4U496984902512511",
            "rel":"delete",
            "method":"DELETE"
        }],
        "httpStatusCode":200
    }

