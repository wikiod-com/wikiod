---
title: "Making a Credit Card Payment (Node)"
slug: "making-a-credit-card-payment-node"
draft: false
images: []
weight: 9935
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| card_data | JSON object containing payment information for transaction | 
| credit_card_details | JSON object containing credit card data that is sent to PayPal to be vaulted |
| client_id | Your PayPal application client ID (OAuth 2 credentials) | 
| paypal | PayPal Node SDK reference | 
| secret | Your PayPal application secret (OAuth 2 credentials) | 
| uuid | Reference to the node-uuid package |

This sample takes the user through crediting a simple credit card transaction using the PayPal SDKs.

## Node Sample
Start by installing the PayPal Node module from NPM

    npm install paypal-rest-sdk

In your application file, add in the configuration information for the SDK 

    var paypal = require('paypal-rest-sdk');
    
    var client_id = 'YOUR CLIENT ID';
    var secret = 'YOUR SECRET';

    paypal.configure({
        'mode': 'sandbox', //sandbox or live
        'client_id': client_id,
        'client_secret': secret
    });

We add the requirement for the SDK, then set up variables for the client ID and secret that were obtained when [creating an application][1]. We then configure our application using these details, and specify the environment that we are working in (live or sandbox).

Next, we set up the JSON object that contains the payment information for the payer.

    var card_data = {
      "intent": "sale",
      "payer": {
        "payment_method": "credit_card",
        "funding_instruments": [{
          "credit_card": {
            "type": "visa",
            "number": "4417119669820331",
            "expire_month": "11",
            "expire_year": "2018",
            "cvv2": "874",
            "first_name": "Joe",
            "last_name": "Shopper",
            "billing_address": {
              "line1": "52 N Main ST",
              "city": "Johnstown",
              "state": "OH",
              "postal_code": "43210",
              "country_code": "US" }}}]},
      "transactions": [{
        "amount": {
          "total": "7.47",
          "currency": "USD",
          "details": {
            "subtotal": "7.41",
            "tax": "0.03",
            "shipping": "0.03"}},
        "description": "This is the payment transaction description." 
    }]};

Add an `intent` of `sale`, and a `payment_method` of `credit_card`. Next, add in the card and address details for the credit card under `funding_instruments`, and the amount to be charged under `transactions`. Multiple transaction objects can be placed here. 

Lastly, we make a request to `payment.create(...)`, passing in our `card_data` object, in order to process the payment. 

    paypal.payment.create(card_data, function(error, payment){
      if(error){
        console.error(error);
      } else {
        console.log(payment);
      }
    });

If the transaction was successful, we should see a response object similar to the following:

    {
      "id": "PAY-9BS08892W3794812YK4HKFQY",
      "create_time": "2016-04-13T19:49:23Z",
      "update_time": "2016-04-13T19:50:07Z",
      "state": "approved",
      "intent": "sale",
      "payer": {
        "payment_method": "credit_card",
        "funding_instruments": [
          {
            "credit_card": {
              "type": "visa",
              "number": "xxxxxxxxxxxx0331",
              "expire_month": "11",
              "expire_year": "2018",
              "first_name": "Joe",
              "last_name": "Shopper",
              "billing_address": {
                "line1": "52 N Main ST",
                "city": "Johnstown",
                "state": "OH",
                "postal_code": "43210",
                "country_code": "US"
              }
            }
          }
        ]
      },
      "transactions": [
        {
          "amount": {
            "total": "7.47",
            "currency": "USD",
            "details": {
              "subtotal": "7.41",
              "tax": "0.03",
              "shipping": "0.03"
            }
          },
          "description": "This is the payment transaction description.",
          "related_resources": [
            {
              "sale": {
                "id": "0LB81696PP288253D",
                "create_time": "2016-04-13T19:49:23Z",
                "update_time": "2016-04-13T19:50:07Z",
                "amount": {
                  "total": "7.47",
                  "currency": "USD"
                },
                "state": "completed",
                "parent_payment": "PAY-9BS08892W3794812YK4HKFQY",
                "links": [
                  {
                    "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/sale\/0LB81696PP288253D",
                    "rel": "self",
                    "method": "GET"
                  },
                  {
                    "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/sale\/0LB81696PP288253D\/refund",
                    "rel": "refund",
                    "method": "POST"
                  },
                  {
                    "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/payment\/PAY-9BS08892W3794812YK4HKFQY",
                    "rel": "parent_payment",
                    "method": "GET"
                  }
                ],
                "fmf_details": {
                  
                },
                "processor_response": {
                  "avs_code": "X",
                  "cvv_code": "M"
                }
              }
            }
          ]
        }
      ],
      "links": [
        {
          "href": "https:\/\/api.sandbox.paypal.com\/v1\/payments\/payment\/PAY-9BS08892W3794812YK4HKFQY",
          "rel": "self",
          "method": "GET"
        }
      ],
      "httpStatusCode": 201
    }

In this return object, having a `state` of `approved` tells us that the transaction was successful. Under the `links` object are a number of [HATEOAS][2] links that provide potential next steps that can be taken on the action that was just performed. In this case, we can retrieve information about the payment by making a GET request to the `self` endpoint provided.

  [1]: https://www.wikiod.com/improvement-requests
  [2]: https://developer.paypal.com/docs/integration/direct/paypal-rest-payment-hateoas-links/

## Making a Payment with a Vaulted Credit Card (Node)
In this example, we'll be looking at how to store a credit card using the PayPal vault, then reference that stored credit card to process a credit card transaction for a user.

The reason why we would want to use the vault is so that we don't have to store sensitive credit card information on our own servers. We simply reference the payment method via a provided vault ID, meaning that we don't have to deal with many PCI compliance regulations with storing the credit cards ourselves.

As with previous samples, we start with setting up our environment.  

    var paypal = require('paypal-rest-sdk'),
        uuid = require('node-uuid');
    
    var client_id = 'YOUR CLIENT ID';
    var secret = 'YOUR SECRET';
    
    paypal.configure({
      'mode': 'sandbox', //sandbox or live
      'client_id': client_id,
      'client_secret': secret
    });

The one difference to previous samples here is that we are requiring a new package, `node-uuid`, which is to be used to generate unique UUID's for the payers when storing the card. You can install that package via:

    npm install node-uuid

Next, we define the credit card JSON object that will be sent to the PayPal vault for storage. It contains information from the card, as well as a unique payer ID that we generate using `node-uuid`. You should store this unique `payer_id` in your own database as it will be used when creating a payment with the vaulted card.
    
    var create_card_details = {
        "type": "visa",
        "number": "4417119669820331",
        "expire_month": "11",
        "expire_year": "2018",
        "first_name": "John",
        "last_name": "Doe",
        "payer_id": uuid.v4()
    };

Lastly, we need to store the credit card and process the payment using that card. To vault a credit card, we call `credit_card.create(...)`, passing in the `credit_card_details` object that we just created. If all goes well, we should have an object returned to us with details about the vaulted card. For the sake of a payment with that card, we only really need two pieces of information: the payer_id that we already stored, and the vault ID, that should also be stored as a reference in our own database.

    paypal.credit_card.create(create_card_details, function(error, credit_card){
        if(error){
            console.error(error);
        } else {
            var card_data = {
                "intent": "sale",
                "payer": {
                    "payment_method": "credit_card",
                    "funding_instruments": [{
                        "credit_card_token": {
                            "credit_card_id": credit_card.id,
                            "payer_id": credit_card.payer_id
                        }
                    }]
                },
                "transactions": [{
                    "amount": {
                        "total": "7.47",
                        "currency": "USD",
                        "details": {
                            "subtotal": "7.41",
                            "tax": "0.03",
                            "shipping": "0.03"
                        }
                    },
                    "description": "This is the payment transaction description." 
                }]
            };
            
            paypal.payment.create(card_data, function(error, payment){
                if(error){
                    console.error(error);
                } else {
                    console.log(JSON.stringify(payment));
                }
            });
        }
    });

In the section following the successful vaulting of the credit card, we are simply defining the card details and processing the payment, as we did with the previous credit card processing example. The main difference in the structure of the `card_data` object is the `funding_instruments ` section, that we define under `payer`. Instead of defining the credit card information, we instead use the following object that contains the vault ID reference, and the payer ID:

    "credit_card_token": {
        "credit_card_id": credit_card.id,
        "payer_id": credit_card.payer_id
    }

That is how we use a vaulted card to process a payment. 

