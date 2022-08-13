---
title: "Stripe.Net Introduction"
slug: "stripenet-introduction"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
 - var stripeSubscriptionOptions = new
   StripeSubscriptionCreateOptions();
   
   //create a variable to hold options object
   
               stripeSubscriptionOptions.Quantity = model.update;
   
   //example option of quantity of seats for a subscription
   
               var subscriptionService = new StripeSubscriptionService();
   
   //create a service to make the API call
   
               var stripeSubscription = subscriptionService.Create(user.CustomerIdentifier, planId,
   stripeSubscriptionOptions);
   
   // service.create( string CustID, string PlanID, Object
   SubscriptionOptions)
   
   //Customer ID should be saved from your database, you can retrieve
   planID from stripe using a PlanService and create the options object
   like above. If you NuGet the Stripe.Net intellisense works for these
   as well.



Somewhere in the beginning of your controller you should call

    StripeConfiguration.SetApiKey(YOUR SECRET KEY VAR);

and for data safety it should be a value hidden as a secret in appsettings

if you do not set the API key you will not be able to modify subscriptions or create customers

## Starting with Stripe.Net in ASP.Net Core 1.0
https://github.com/jaymedavis/stripe.net is a great starting point. <br>
Assuming you are using MVC w/Razor you need to have a few things in your View page

    <script type="text/javascript" src="https://js.stripe.com/v2/"></script>

This script calls upon the stripe.js to handle creating a token.

    <script type="text/javascript">
            Stripe.setPublishableKey('YOUR STRIPE PUBLIC KEY');

    var stripeResponseHandler = function (status, response) {
        var $form = $('#payment-form');

        if (response.error) {
            // Show the errors on the form
            $form.find('.payment-errors').text(response.error.message);
            $form.find('button').prop('disabled', false);
        } else {
            // token contains id, last4, and card type
            var token = response.id;
            // Insert the token into the form so it gets submitted to the server
            $form.append($('<input type="hidden" asp-for="stripeToken" />').val(token));
            // and re-submit
            $form.get(0).submit();
        }
    };

    jQuery(function ($) {
        $('#payment-form').submit(function (e) {
            var $form = $(this);

            // Disable the submit button to prevent repeated clicks
            $form.find('button').prop('disabled', true);

            Stripe.card.createToken($form, stripeResponseHandler);

            // Prevent the form from submitting with the default action
            return false;
        });
    });
    </script>
To add the token to a model make sure you change

    $form.append($('<input type="hidden" asp-for="stripeToken" />').val(token));
to reflect your model. The form should look like this one.

     <form asp-action="confirm" method="POST" id="payment-form">
                <span class="payment-errors"></span>

                <div class="row">
                    <label>
                        <span>Card Number</span>
                        <input type="text" data-stripe="number" value="4242424242424242">
                    </label>
                </div>

                <div class="row">
                    <label>
                        <span>CVC</span>
                        <input type="text" data-stripe="cvc" value="123">
                    </label>
                </div>

                <div class="row">
                    <label>
                        <span>Expiration (MM/YYYY)</span>
                        <input type="text" data-stripe="exp-month" value="12">
                    </label>
                    <input type="text" data-stripe="exp-year" value="2020">
                </div>

                <button type="submit">Buy Now</button>
            </form> 
The controller takes one of the users and checks for a CustomerIdentifier which is the Id of the customer given by stripe. If this isn't saved in the database then it works on creating a customer for them

     public async Task<IActionResult> Index(OrderViewModel model)   //HOME PAGE beginning of managing subs
        {
            //get the user and their ID
            var user = await GetCurrentUserAsync();
            var userId = user?.Id;
            // If they have a customer Identifier use it
            if (!string.IsNullOrEmpty(user.CustomerIdentifier))  //eventually if user has a saved card as well
            {
                //Create the API call to subscription and put its response in a list
                var subscriptionService = new StripeSubscriptionService();
                IEnumerable<StripeSubscription> response = subscriptionService.List(user.CustomerIdentifier);

                ViewBag.Subscription = response;

                ViewBag.Customer = user.CustomerIdentifier;


            }
            ModelState.Clear();
            return View(model);
        }
     public async Task<IActionResult> Confirm(OrderViewModel model, string stripeToken)  // CREATE CHARGE NO CARD/ NO CUSTOMER
        {
            model.stripeToken = stripeToken;
            //get the user and their ID
            var user = await GetCurrentUserAsync();
            var userId = user?.Id;
            var planId = "YOUR PLAN ID HERE";  //plan ID only 1 atm but will need to be a dynamic plan ID list later
            // If they have a customer Identifier use it
            if (!string.IsNullOrEmpty(user.CustomerIdentifier))
            {
                //Create the API call to subscription and put its response in a list
                //Use the subscription options to apply a quantity to the initial subscription for seats
                var stripeSubscriptionOptions = new StripeSubscriptionCreateOptions();
                stripeSubscriptionOptions.Quantity = model.update;
                var subscriptionService = new StripeSubscriptionService();
                var stripeSubscription = subscriptionService.Create(user.CustomerIdentifier, planId, stripeSubscriptionOptions);
                //save Subscriptions data here

                ModelState.Clear();
                //await SaveSubscription(stripeSubscription, user);
                await _userManager.UpdateAsync(user);
            }
            else // Customer is new and doesn't have an ID
            {
                //Create API options
                var customer = new StripeCustomerCreateOptions();
                
                // Add option values
                customer.Email = $"{user.Email}";
                customer.Description = $"{user.Email} [{userId}]";
                customer.PlanId = planId;
                
                customer.SourceToken = model.stripeToken;
                //Make the call to create the customer with the creation options
                var customerService = new StripeCustomerService();
                StripeCustomer stripeCustomer = customerService.Create(customer);

                //save the customer ID
                user.CustomerIdentifier = stripeCustomer.Id;

                //create card update options and add billing info
                var cardOptions = new StripeCardUpdateOptions();
                cardOptions.AddressLine1 = model.BillingInfo.AddressL1;
                cardOptions.AddressLine2 = model.BillingInfo.AddressL2;
                cardOptions.AddressCountry = model.BillingInfo.Country;
                cardOptions.AddressCity = model.BillingInfo.City;
                cardOptions.AddressState = model.BillingInfo.State;
                cardOptions.AddressZip = model.BillingInfo.Zip;
                cardOptions.Name = model.BillingInfo.Name;
                var cardUpdate = new StripeCardService();
                // get the customer card ID and then update the card info
                StripeCustomer customerCardGet = customerService.Get(user.CustomerIdentifier);
                var cardId = customerCardGet.DefaultSourceId;
                StripeCard Card = cardUpdate.Update(user.CustomerIdentifier, cardId, cardOptions);


                //save Subscriptions data here
                //user.ConcurrentUsers = Stripe Quantity
                
                ModelState.Clear();

                await _userManager.UpdateAsync(user);
               
            }
            ViewBag.Success = "confirm";
            return View("Success");
        }
This particular example includes some of the extras like taking billing information in the controller. If you want this just add it to the form on the view and create a model to hold it.

