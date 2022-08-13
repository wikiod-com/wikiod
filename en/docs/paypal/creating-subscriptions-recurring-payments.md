---
title: "Creating Subscriptions  Recurring Payments"
slug: "creating-subscriptions--recurring-payments"
draft: false
images: []
weight: 9927
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| billingAgreementAttributes | Configuration object to create the billing agreement | 
| billingPlan | Billing plan ID from the query string | 
| billingPlanAttribs | Configuration object to create the billing plan |
| billingPlanUpdateAttributes | Configuration object for changing a billing plan to an active state |  
| clientId | Your application client ID (OAuth keys) | 
| http | Reference to the http package to set up our simple server |
| isoDate | ISO date for setting the subscription start date | 
| links | HATEOAS link object for extracting the redirect URL to PayPal | 
| params | Query string parameters | 
| paypal | Reference to the PayPal SDK |
| secret | Your application secret (OAuth keys) |
| token | The billing agreement approval token provided after PayPal redirect to execute the billing agreement |    


These examples go through the process of creating a subscription / recurring payment system using PayPal. 

The process for creating a subscription is to:

 1. Create a billing plan. This is a reusable model that outlines the details of the subscription. 
 2. Activate the billing plan.
 3. When you want to create a subscription for a user, you create a billing agreement using the ID of the billing plan that they should be subscribed to.
 4. Once created, you redirect the user to PayPal to confirm the subscription. Once confirmed, the user is redirected back to the merchant's website.
 5. Lastly, you execute the billing agreement to begin the subscription.

## Step 2: Creating a Subscription for a User using a Billing Agreement (Node Sample)
The second step to creating a subscription for a user is to create and execute a billing agreement, based on an existing activated billing plan. This example assumes that you have already gone through and activated a billing plan in the previous example, and have an ID for that billing plan to reference in the example.

When you are setting up a billing agreement to create a subscription for a user, you'll follow 3 steps, which may be reminiscent of processing a PayPal payment:

 1. You create a billing agreement, referencing an underlying billing plan via the ID.
 2. Once created, you redirect the user to PayPal (if paying via PayPal) to confirm the subscription. Once confirmed, PayPal redirects the user back to your site using the redirect provided in the underlying billing plan.
 3. You then execute the billing agreement using a token provided back via the PayPal redirect.

This example is setting up an Express based HTTP server to showcase the billing agreement process.

To start the example, we first need to set up our configuration. We add four requirements, the PayPal SDK, `body-parser` for handling JSON encoded bodies, `http` for our simple server integration, and `express` for the Express framework. We then define our client ID and secret from creating an application, configure the SDK for the sandbox, then configure bodyParser for handling JSON bodies.

    var paypal = require('paypal-rest-sdk'),
        bodyParser = require('body-parser'),
        http = require('http'),
        app = require('express')();
    
    var clientId = 'YOUR APPLICATION CLIENT ID';
    var secret = 'YOUR APPLICATION SECRET';
    
    paypal.configure({
      'mode': 'sandbox', //sandbox or live
      'client_id': clientId,
      'client_secret': secret
    });
    
    app.use(bodyParser.json());

Our first step in the billing agreement is to create a route to handle the creation of a billing agreement, and redirecting the user to PayPal to confirm that subscription. We are assuming that a billing plan ID is passed as a query string parameter, such as by loading the following URL with a plan ID from the previous example:

    http://localhost:3000/createagreement?plan=P-3N543779E9831025ECYGDNVQ

We now need to use that information to create the billing agreement.

    app.get('/createagreement', function(req, res){
        var billingPlan = req.query.plan;
        
        var isoDate = new Date();
        isoDate.setSeconds(isoDate.getSeconds() + 4);
        isoDate.toISOString().slice(0, 19) + 'Z';
    
        var billingAgreementAttributes = {
            "name": "Standard Membership",
            "description": "Food of the World Club Standard Membership",
            "start_date": isoDate,
            "plan": {
                "id": billingPlan
            },
            "payer": {
                "payment_method": "paypal"
            },
            "shipping_address": {
                "line1": "W 34th St",
                "city": "New York",
                "state": "NY",
                "postal_code": "10001",
                "country_code": "US"
            }
        };
    
        // Use activated billing plan to create agreement
        paypal.billingAgreement.create(billingAgreementAttributes, function (error, billingAgreement){
            if (error) {
                console.error(error);
                throw error;
            } else {
                //capture HATEOAS links
                var links = {};
                billingAgreement.links.forEach(function(linkObj){
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

We start by extracting the billing plan ID from the query string and create the date when the plan should start.

The next object definition, `billingAgreementAttributes`, consists of information for the subscription. It contains readable information on the plan, a reference to the billing plan ID, the payment method, and shipping details (if needed for the subscription). 

Next, a call to `billingAgreement.create(...)` is made, passing in the `billingAgreementAttributes` object we just created. If all is successful, we should have a billing agreement object passed back to us containing details about our newly created subscription. That object also contains a number of HATEOAS links providing us next steps that can be taken on this newly created agreement. The one we care about here is labeled as `approval_url`. 

We loop through all provided links to put them into an easily referenced object. If `approval_url` is one of those links, we redirect the user to that link, which is PayPal.

At this point the user confirms the subscription on PayPal, and is redirected back to the URL provided in the underlying billing plan. Along with that URL, PayPal will also pass a token along the query string. That token is what we're going to use to execute (or start) the subscription.

Let's set up that functionality in the following route.

    app.get('/processagreement', function(req, res){
        var token = req.query.token;
        
        paypal.billingAgreement.execute(token, {}, function (error, billingAgreement) {
            if (error) {
                console.error(error);
                throw error;
            } else {
                console.log(JSON.stringify(billingAgreement));
                res.send('Billing Agreement Created Successfully');
            }
        });
    });

We extract the token from the query string, then make a call to `billingAgreement.execute`, passing along that token. If all is successful, we now have a valid subscription for the user. The return object contains information about the active billing agreement.

Lastly, we set up our HTTP server to listen for traffic to our routes.

    //create server
    http.createServer(app).listen(3000, function () {
       console.log('Server started: Listening on port 3000');
    });

## Step 1: Creating a Subscription Model using a Billing Plan (Node Sample)
When creating a subscription for a user, you first need to create and activate a billing plan that a user is then subscribed to using a billing agreement. The full process for creating a subscription is detailed in the remarks of this topic.

Within this example, we're going to be using the [PayPal Node SDK][1]. You can obtain it from NPM using the following command:

    npm install paypal-rest-sdk

Within our .js file, we first set up our SDK configuration, which includes adding a requirement for the SDK, defining our client ID and secret from [creating our application][2], and then configuring the SDK for the sandbox environment.

    var paypal = require('paypal-rest-sdk');
    
    var clientId = 'YOUR CLIENT ID';
    var secret = 'YOUR SECRET';

    paypal.configure({
      'mode': 'sandbox', //sandbox or live
      'client_id': clientId,
      'client_secret': secret
    });

Next, we need to set up two JSON objects. The `billingPlanAttribs` object contains the information and payment breakdown for the billing plan that we can subscribe users to, and the `billingPlanUpdateAttributes` object contains values for setting the billing plan to an active state, allowing it to be used.

    var billingPlanAttribs = {
        "name": "Food of the World Club Membership: Standard",
        "description": "Monthly plan for getting the t-shirt of the month.",
        "type": "fixed",
        "payment_definitions": [{
            "name": "Standard Plan",
            "type": "REGULAR",
            "frequency_interval": "1",
            "frequency": "MONTH",
            "cycles": "11",
            "amount": {
                "currency": "USD",
                "value": "19.99"
            }
        }],
        "merchant_preferences": {
            "setup_fee": {
                "currency": "USD",
                "value": "1"
            },
            "cancel_url": "http://localhost:3000/cancel",
            "return_url": "http://localhost:3000/processagreement",
            "max_fail_attempts": "0",
            "auto_bill_amount": "YES",
            "initial_fail_amount_action": "CONTINUE"
        }
    };
    
    var billingPlanUpdateAttributes = [{
        "op": "replace",
        "path": "/",
        "value": {
            "state": "ACTIVE"
        }
    }];

Within the `billingPlanAttribs` object, there are some relevant pieces of information:

 - **name / description / type**: Basic visual information to describe the plan, and the type of plan.
 - **payment_definitions**: Information on how the plan should function and be billed. More details on fields [here][3].
 - **merchant_preferences**: Additional fee structures, redirect URLs, and settings for the subscription plan. More details on fields [here][4].

With those objects in place, we can now create and activate the billing plan.

    paypal.billingPlan.create(billingPlanAttribs, function (error, billingPlan){
        if (error){
            console.log(error);
            throw error;
        } else {
            // Activate the plan by changing status to Active
            paypal.billingPlan.update(billingPlan.id, billingPlanUpdateAttributes, function(error, response){
                if (error) {
                    console.log(error);
                    throw error;
                } else {
                    console.log(billingPlan.id);
                }
            });
        }
    });

We call `billingPlan.create(...)`, passing in the `billingPlanAttribs` object that we just created. If that is successful, the return object will contain information about the billing plan. For the sake of the example, we just need to use the billing plan ID in order to activate the plan for use.

Next, we call `billingPlan.update(...)`, passing in the billing plan ID and the `billingPlanUpdateAttributes` object we created earlier. If that is successful, our billing plan is now active and ready to use.

In order to create a subscription for a user (or multiple users) on this plan, we'll need to reference the billing plan id (`billingPlan.id` above), so store that in a place that can be referenced easily. 

In the second subscription step, we need to create a billing agreement based on the plan we just created and execute it to begin processing subscriptions for a user.

  [1]: https://github.com/paypal/PayPal-node-SDK/
  [2]: https://www.wikiod.com/paypal/getting-started-with-paypal#Creating an application and obtaining client id / secret keys
  [3]: https://developer.paypal.com/docs/api/#paymentdefinition-object
  [4]: https://developer.paypal.com/docs/api/#merchantpreferences-object

