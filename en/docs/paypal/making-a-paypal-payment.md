---
title: "Making a PayPal payment"
slug: "making-a-paypal-payment"
draft: false
images: []
weight: 9697
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| clientId | Your PayPal application client ID (OAuth 2 credentials) | 
| links | Simple reference object for all return HATEOAS links from PayPal |  
| paymentId | The ID of the payment returned from PayPal in order to complete payment |  
| payerId | The ID of the payer returned from PayPal in order to complete payment |  
| paypal | PayPal Node SDK reference | 
| payReq | JSON object containing payment information for transaction | 
| req | The request object from the server request |  
| res | The response object from the server request |  
| secret | Your PayPal application secret (OAuth 2 credentials) | 

These samples cover how to process a payment via PayPal, using the PayPal SDKs. These are simple request samples that outline the multi-step process for allowing this payment option.

## Node Express Server Example
In this example, we're going to set up an Express server integration to display how to process a payment with PayPal, using the PayPal Node SDK. We will use a static JSON structure for the payment details for the sake of brevity.

There are three general steps that we will follow when building out the functions to handle the PayPal payment:

 1. We create a JSON object containing the payment that we intend to process through PayPal. We then send that to PayPal to obtain a link to redirect the user to in order to confirm payment.
2. Next, we redirect the user to PayPal to confirm the payment. Once confirmed, PayPal redirects the user back to our application.
3. Once returned to the app, we complete the payment on behalf of the user.

Breaking this down as a simple Node app, we start by obtaining the PayPal Node SDK from NPM: 

    npm install paypal-rest-sdk

Next, we set up the app configuration and packages.

    var http = require('http'),
        paypal = require('paypal-rest-sdk'),
        bodyParser = require('body-parser'),
        app = require('express')();
    
    var client_id = 'YOUR APPLICATION CLIENT ID';
    var secret = 'YOUR APPLICATION SECRET';
    
    //allow parsing of JSON bodies
    app.use(bodyParser.json());
    
    //configure for sandbox environment
    paypal.configure({
        'mode': 'sandbox', //sandbox or live
        'client_id': client_id,
        'client_secret': secret
    });

We require four requirements for this app:

 1. The HTTP package for our server.
 2. The PayPal Node SDK package.
 3. The bodyParser package for working with JSON encoded bodies.
 4. The Express framework for our server.

The next few lines set up variables for the client ID and secret that were obtained when [creating an application][1]. We then set up `bodyParser` to allow for JSON encoded bodies, then configure our application using the application details, and specify the environment that we are working in (live for production or sandbox for testing).
    
Now let's create the route for creating a payment request with PayPal. 

    app.get('/create', function(req, res){
        //build PayPal payment request
        var payReq = JSON.stringify({
            'intent':'sale',
            'redirect_urls':{
                'return_url':'http://localhost:3000/process',
                'cancel_url':'http://localhost:3000/cancel'
            },
            'payer':{
                'payment_method':'paypal'
            },
            'transactions':[{
                'amount':{
                    'total':'7.47',
                    'currency':'USD'
                },
                'description':'This is the payment transaction description.'
            }]
        });
    
        paypal.payment.create(payReq, function(error, payment){
            if(error){
                console.error(error);
            } else {
                //capture HATEOAS links
                var links = {};
                payment.links.forEach(function(linkObj){
                    links[linkObj.rel] = {
                        'href': linkObj.href,
                        'method': linkObj.method
                    };
                })
            
                //if redirect url present, redirect user
                if (links.hasOwnProperty('approval_url')){
                    res.redirect(links['approval_url'].href);
                } else {
                    console.error('no redirect URI present');
                }
            }
        });
    });

The first thing we do is set up the payment request JSON object, which contains the information that we need to provide PayPal with to create the payment. We set the `intent` to `sale`, specify the redirect URLs (where PayPal should forward the user to after they confirm / cancel the payment), add in a `payment_method` of `paypal` to signal that we will make a PayPal payment, then specify the transaction information for the payer to confirm.

We then call `payment.create(...)`, passing in our `payReq` object. This will send the create payment request to PayPal. Once that returns, and is successful, we can loop through the provided [HATEOAS][2] links in the return object to extract the URL that we need to redirect the user to, which is labeled under `approval_url`. 

The format for the HATEOAS links can cause fragile reference code if used directly, so we loop through all provided links and put them in a better reference object to future proof against changes. If the `approval_url` is then found in that object, we redirect the user.

At this point the user is redirected to PayPal to confirm the payment. Once they do, they are redirected back to the `return_url` that we specified in the `createPayment(...)` function. 

We now have to provide a route to handle that return, in order to complete the payment.
    
    app.get('/process', function(req, res){
        var paymentId = req.query.paymentId;
        var payerId = { 'payer_id': req.query.PayerID };
    
        paypal.payment.execute(paymentId, payerId, function(error, payment){
            if(error){
                console.error(error);
            } else {
                if (payment.state == 'approved'){ 
                    res.send('payment completed successfully');
                } else {
                    res.send('payment not successful');
                }
            }
        });
    });

When the user is returned back to your app, there will be three query string parameters that will be sent along as well, the `paymentId`, `PayerID`, and `token`. We only need to deal with the first two.

We extract the parameters, and place the PayerID in a simple object for the need of the payment execution step. Next, a call is made to `payment.execute(...)`, passing in those two parameters, in order to complete the payment.

Once that request is made, we see if the payment completed successfully by checking if `payment.state` is set to `approved`. If so, we can store what we need from the payment object that is returned.

Our last step is to initialize our server and listen for traffic coming to the routes we specified.

    //create server
    http.createServer(app).listen(3000, function () {
       console.log('Server started: Listening on port 3000');
    });

Once the server is initialized, going to `http://localhost:3000/create` initializes the payment process.

  [1]: https://www.wikiod.com/improvement-requests
  [2]: https://developer.paypal.com/docs/integration/direct/paypal-rest-payment-hateoas-links/

