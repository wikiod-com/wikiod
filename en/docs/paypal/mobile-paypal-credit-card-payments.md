---
title: "Mobile PayPal  Credit Card Payments"
slug: "mobile-paypal--credit-card-payments"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## Parameters
| Parameter | Details |  
| --------- | ------- |  
| button | Simple payment button |  
| config | PayPal configuration object housing our client ID (from application creation) and the environment we want to use (sandbox or live) |  
| payment | PayPal payment details |
| paymentConfig | Configuration Intent for the payment information and settings |    
| serviceConfig | Configuration Intent for the config parameter data |  


Samples related to processing payments on mobile devices

## Android: Accepting a PayPal / Credit Card Payment
In this tutorial we're going to learn how to set up the [PayPal Android SDK][1] to process a simple payment via either a PayPal payment or a credit card purchase. At the end of this example, you should have a simple button in an application that, when clicked, will forward the user to PayPal to confirm a set payment, then return the user back to the application and log the confirmation of payment.

The complete application code for this example is available in the [PayPal Developer Github Repository][2].

Let's get started.

The first step is to [obtain and add the SDK to your project][3]. We add the reference to our build.gradle dependancies like so:

    dependencies {
        compile 'com.paypal.sdk:paypal-android-sdk:2.14.1'
        ...
    }
    

Now we head over to our MainActivity.java file (or wherever you'd like to add the PayPal button integration), and add in a `config` object for our client ID and the environment (sandbox) that we will be using.

    private static PayPalConfiguration config = new PayPalConfiguration()
        .environment(PayPalConfiguration.ENVIRONMENT_SANDBOX)
        .clientId("YOUR CLIENT ID");
    

Now we're going to create a button in our `onCreate(...)` method, which will enable us to process a payment via PayPal once clicked.

    @Override
    protected void onCreate(Bundle savedInstanceState){
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    
        final Button button = (Button) findViewById(R.id.paypal_button);
    }
    

We now need to define the functionality for that button. In your res > layout > main XML file you can add the following definition for the button, which will define the text and onClick handler for the button with the paypal_button ID.

    <Button android:id="@+id/paypal_button"
        android:layout_height="wrap_content"
        android:layout_width="wrap_content"
        android:text="@string/paypal_button"
        android:onClick="beginPayment" />
    

When clicked, the button will call the `beginPayment(...)` method. We can then add the text for the button to our strings.xml file, like so:

    <string name="paypal_button">Pay with PayPal</string>
    

With the button in place, we now have to handle the button click in order to begin payment processing. Add in the following `beginPayment(...)` method below our previous `onCreate(...)` method.

    public void beginPayment(View view){
        Intent serviceConfig = new Intent(this, PayPalService.class);
        serviceConfig.putExtra(PayPalService.EXTRA_PAYPAL_CONFIGURATION, config);
        startService(serviceConfig);
    
        PayPalPayment payment = new PayPalPayment(new BigDecimal("5.65"), 
            "USD", "My Awesome Item", PayPalPayment.PAYMENT_INTENT_SALE);
    
        Intent paymentConfig = new Intent(this, PaymentActivity.class);
        paymentConfig.putExtra(PayPalService.EXTRA_PAYPAL_CONFIGURATION, config);
        paymentConfig.putExtra(PaymentActivity.EXTRA_PAYMENT, payment);
        startActivityForResult(paymentConfig, 0);
    }
    

What we are doing here is first setting up the service intent (`serviceConfig`), using the `config` that we had defined previously for our client ID and the sandbox environment. We then specify the payment object that we want to process. For the sake of this example, we are setting a static price, currency, and description. In your final application, these values should be obtained from what the user is trying to buy in the application. Lastly, we set up the `paymentConfig`, adding in both the `config` and `payment` objects that we had previously defined, and start the activity.

At this point the user will be presented with the PayPal login and payment screens, allowing them to select whether to pay with PayPal or a credit card (via manual entry or card.io if the camera is available). That screen will look something like this:

![PayPal payment confirmation screen][4]

Once done, we need to have a handler ready for when PayPal forwards the user back to the application after confirmation of payment or cancellation. Let's override `onActivityResult(...)` for that purpose.

    @Override
    protected void onActivityResult (int requestCode, int resultCode, Intent data){
        if (resultCode == Activity.RESULT_OK){
            PaymentConfirmation confirm = data.getParcelableExtra(
                PaymentActivity.EXTRA_RESULT_CONFIRMATION);
            if (confirm != null){
                try {
                    Log.i("sampleapp", confirm.toJSONObject().toString(4));
    
                    // TODO: send 'confirm' to your server for verification
    
                } catch (JSONException e) {
                    Log.e("sampleapp", "no confirmation data: ", e);
                }
            }
        } else if (resultCode == Activity.RESULT_CANCELED) {
            Log.i("sampleapp", "The user canceled.");
        } else if (resultCode == PaymentActivity.RESULT_EXTRAS_INVALID) {
            Log.i("sampleapp", "Invalid payment / config set");
        }
    }
    

Within the `onActivityResult(...)` method, we are checking to see if the `resultCode` that comes back is RESULT_OK (user confirmed payment), RESULT_CANCELED (user cancelled payment), or RESULT_EXTRAS_INVALID (there was a configuration issue). In the case of a valid confirmation, we get the object that is returned from the payment and, in this sample, log it. What will be returned to us should look something like the following:

    {
        "client": {
            "environment": "sandbox",
            "paypal_sdk_version": "2.14.1",
            "platform": "Android",
            "product_name": "PayPal-Android-SDK"
        },
        "response": {
            "create_time": "2016-05-02T15:33:43Z",
            "id": "PAY-0PG63447RB821630KK1TXGTY",
            "intent": "sale",
            "state": "approved"
        },
        "response_type": "payment"
    }
    

If we look under the `response` object, we can see that we have a `state` of `approved`, meaning that the payment was confirmed. At this point, that object should be sent to your server to confirm that a payment actually went through. For more information on those steps, [see these docs][5].

Our last step is to cleanup in our `onDestroy(...)`.

    @Override
    public void onDestroy(){
        stopService(new Intent(this, PayPalService.class));
        super.onDestroy();
    }
    

That's all there is to it. In this example we've created a simple button to process a payment with either PayPal or a credit card. From this point, there are a few next steps for you to expand upon this sample:

*   Pulling in payment information dynamically based on user product selection in the `beginPayment(...)` method.
*   Sending the payment confirmation to your server and verifying that the payment actually went through.
*   Handling the error and cancellation user cases within the app.

 [1]: https://github.com/paypal/PayPal-Android-SDK
 [2]: https://github.com/paypaldev/android-samples/tree/master/payment-sample
 [3]: https://github.com/paypal/PayPal-Android-SDK#add-the-sdk-to-your-project
 [4]: https://devblog.paypal.com/wp-content/uploads/2016/05/5556_Nexus_6_API_21_2.jpg
 [5]: https://developer.paypal.com/webapps/developer/docs/integration/mobile/verify-mobile-payment/

