---
title: "Getting started with PayPal"
slug: "getting-started-with-paypal"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Creating an application and obtaining client id / secret keys
In order to begin building with PayPal APIs, you have to create an application to obtain a client ID and secret. 

Go to https://developer.paypal.com/developer/applications/, sign in, and click on "Create App", as shown below:

[![enter image description here][1]][1]

Next, enter an application name, select the sandbox testing account that you want to use (if it's a new account, leave the default value), and click "Create App".

[![enter image description here][2]][2]

Once the application is created, you will be provided with your sandbox and live client ID and secret, which will look similar to the following:

[![enter image description here][3]][3]

These credentials are what you will use when making requests to PayPal APIs in order to authenticate your application and make requests. 

  [1]: http://i.stack.imgur.com/b3l0P.jpg
  [2]: http://i.stack.imgur.com/MZGfe.jpg
  [3]: http://i.stack.imgur.com/7yfrg.jpg

## Setting up sandbox user test accounts
When testing you PayPal integration on sandbox, you'll need to have sandbox user accounts set up to use to go through the payment flow.

Go to https://developer.paypal.com/developer/accounts/, log in using your PayPal account, and click on "Create Account", as below:

[![enter image description here][1]][1]

Enter in the accounts details for the new test user, including a unique email, account information, payment method, balance, etc, and click on "Create Account" at the bottom of the page once done. This will create the new account for you to begin using. 

To see account details for this new user, expand the entry on the accounts page, and click on "Profile". 

[![enter image description here][2]][2]

Once that profile information loads, clicking on the "Funding" tab will give you payment information for that account, including credit card information that may be used for direct credit card processing against sandbox.

NOTE: When using the sandbox API endpoints, you need to use sandbox test account to log in and pay for the test goods, as your live account information will not work.

  [1]: http://i.stack.imgur.com/mZM5n.jpg
  [2]: http://i.stack.imgur.com/rVDrx.jpg

