---
title: "PHP-Stripe Connect documentation for Symfony2"
slug: "php-stripe-connect-documentation-for-symfony2"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Parameters
| Parameter | Details |   
| --------- | ------- |   
| amount | required - A positive integer in the smallest currency unit (e.g., 100 cents to charge $1.00 or 100 to charge ¥100, a 0-decimal currency) representing how much to charge the card. The minimum amount is $0.50 US or equivalent in charge currency.
currency | required - 3-letter ISO code for currency.|
| description | optional, default is null - An arbitrary string which you can attach to a charge object. It is displayed when in the web interface alongside the charge. Note that if you use Stripe to send automatic email receipts to your customers, your receipt emails will include the description of the charge(s) that they are describing.|
| receipt_email | optional default is null - The email address to send this charge's receipt to. The receipt will not be sent until the charge is paid. If this charge is for a customer, the email address specified here will override the customer's email address. Receipts will not be sent for test mode charges. If receipt_email is specified for a charge in live mode, a receipt will be sent regardless of your |
| exp_month | required - Two digit number representing the card's expiration month. |
exp_year | required - Two or four digit number representing the card's expiration year.|
| number | required - The card number, as a string without any separators. |
| cvc | usually required -Card security code. Required unless your account is registered in Australia, Canada, or the United States. Highly recommended to always include this value.

## Symfony2- Stripe Integration Example

Download the Stripe API Library and place it in vendor Folder

source : [https://github.com/stripe/stripe-php][1]
    
include the library in your controller

    use Stripe\BalanceTransaction;
    use Stripe\Charge;
    use Stripe\Stripe;
    require_once('../vendor/stripe/init.php');
    
set the strip key

     \Stripe\Stripe::setApiKey('stripe_secret_key');

   Call the charge function for transaction

    $card = array(
        'number' =>'cardccn',
        'cvc' =>'cardcvc',
        'exp_month' => 'expMonth',
        'exp_year' => 'expYear',
        );
        
    $charge = Charge::create(
        array(
            'amount' => ('amount') * 100, // Amount will store in cent in Stripe Account
            'currency' => 'usd',
            'card' => $card,
            'description' => '$data['description',
            'receipt_email'=>'receipt_email'
            )
        );
 
get the details of charge

     $data = Charge::retrieve('ch_%');

  [1]: https://github.com/stripe/stripe-php


