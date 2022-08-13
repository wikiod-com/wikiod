---
title: "Mobile Future Payments (End to End App)"
slug: "mobile-future-payments-end-to-end-app"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

This example shows a practical end to end example of creating a [PayPal future payment][1] from an Android device, using a Node server. 


  [1]: https://developer.paypal.com/docs/integration/mobile/make-future-payment/

## Android Step 1: Layout, Initialization, and Handling Server Response
The complete sample code for this application (Android + Node server) is available in the [PayPal Developer Github repository][1].

The first stage of creating the Android portion of our application is to set up a basic layout and handle responses that come back from the server that we'll set up in Node.

Start by creating a new PayPalConfiguration object to house your application information. 

    private static PayPalConfiguration config = new PayPalConfiguration()
            .environment(PayPalConfiguration.ENVIRONMENT_SANDBOX)
            .clientId("YOUR APPLICATION CLIENT ID")
            .merchantName("My Store")
            .merchantPrivacyPolicyUri(Uri.parse("https://www.example.com/privacy"))
            .merchantUserAgreementUri(Uri.parse("https://www.example.com/legal"));

Next, we add a simple button to `onCreate(...)` to act as our payment initiation. This is simply to trigger off the action, and should be placed as the initiation process for creating a future payment for a user (e.g. when they agree upon a subscription).

    @Override
    protected void onCreate(Bundle savedInstanceState){
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        final Button button = (Button) findViewById(R.id.paypal_button);
    }

Under `res > layout > activity_main.xml` we add the definition for the button with its associated action, when clicked it calls `beginFuturePayment(...)`, which we'll define in a minute. 

    <Button android:id="@+id/paypal_button"
        android:layout_height="wrap_content"
        android:layout_width="wrap_content"
        android:text="@string/paypal_button"
        android:onClick="beginFuturePayment" />

Under `res > values > strings.xml` we then add a string reference for the button.

    <string name="paypal_button">Process Future Payment</string>

Now we add the button handler, to initiate the call to begin the future payment process when the user clicks the button. What we are doing here is starting the payment service with the configuration object we set up at the top of this example.

    public void beginFuturePayment(View view){
        Intent serviceConfig = new Intent(this, PayPalService.class);
        serviceConfig.putExtra(PayPalService.EXTRA_PAYPAL_CONFIGURATION, config);
        startService(serviceConfig);

        Intent intent = new Intent(this, PayPalFuturePaymentActivity.class);
        intent.putExtra(PayPalService.EXTRA_PAYPAL_CONFIGURATION, config);
        startActivityForResult(intent, 0);
    }

When that call to make a future payment is initiated, we will be given some information that will need to be sent to our server. We extract this information from the valid future payment request (`authCode` and `metadataId`), then make execute the async request to the server to complete the future payment (detailed in step 2).

    @Override
    protected void onActivityResult (int requestCode, int resultCode, Intent data){
        if (resultCode == Activity.RESULT_OK){
            PayPalAuthorization auth = data.getParcelableExtra(PayPalFuturePaymentActivity.EXTRA_RESULT_AUTHORIZATION);
            if (auth != null){
                try{
                    //prepare params to be sent to server
                    String authCode = auth.getAuthorizationCode();
                    String metadataId = PayPalConfiguration.getClientMetadataId(this);
                    String [] params = {authCode, metadataId};

                    //process async server request for token + payment
                    ServerRequest req = new ServerRequest();
                    req.execute(params);

                } catch (JSONException e) {
                    Log.e("FPSample", "JSON Exception: ", e);
                }
            }
        } else if (resultCode == Activity.RESULT_CANCELED) {
            Log.i("FPSample", "User canceled.");
        } else if (resultCode == PayPalFuturePaymentActivity.RESULT_EXTRAS_INVALID) {
            Log.i("FPSample", "Invalid configuration");
        }
    }

Lastly, we define our `onDestroy()`.

    @Override
    public void onDestroy(){
        stopService(new Intent(this, PayPalService.class));
        super.onDestroy();
    }


  [1]: https://github.com/paypaldev/android-samples/tree/master/future-payment-sample


## Android Step 2: Async Server Request
The complete sample code for this application (Android + Node server) is available in the [PayPal Developer Github repository][1].

At this point the PayPal future payments button has been clicked, we have an auth code and metadata ID from the PayPal SDK, and we need to pass those on to our server to complete the future payment process.

In the background process below, we are doing a few things:

 - We set up the URI that for our server to be `http://10.0.2.2:3000/fpstore`, which is hitting the `/fpstore` endpoint of our server running on localhost. 
 - The JSON object that will be sent through is then set up, which contains the auth code and metadata ID.
 - The connection is then made. In the case of a successful request (200 / 201 range) we can expect a response back from the server. We read that response and then return it.
 - Lastly, we have a `onPostExecute(...)` method set up to handle that returned server string. In the case of this example, it's simply logged.


    public class ServerRequest extends AsyncTask<String, Void, String> {
        protected String doInBackground(String[] params){
            HttpURLConnection connection = null;
            try{
                //set connection to connect to /fpstore on localhost
                URL u = new URL("http://10.0.2.2:3000/fpstore");
                connection = (HttpURLConnection) u.openConnection();
                connection.setRequestMethod("POST");
    
                //set configuration details
                connection.setRequestProperty("Content-Type", "application/json");
                connection.setRequestProperty("Accept", "application/json");
                connection.setAllowUserInteraction(false);
                connection.setConnectTimeout(10000);
                connection.setReadTimeout(10000);
    
                //set server post data needed for obtaining access token
                String json = "{\"code\": \"" + params[0] + "\", \"metadataId\": \"" + params[1] + "\"}";
                Log.i("JSON string", json);
    
                //set content length and config details
                connection.setRequestProperty("Content-length", json.getBytes().length + "");
                connection.setDoInput(true);
                connection.setDoOutput(true);
                connection.setUseCaches(false);
    
                //send json as request body
                OutputStream outputStream = connection.getOutputStream();
                outputStream.write(json.getBytes("UTF-8"));
                outputStream.close();
    
                //connect to server
                connection.connect();
    
                //look for 200/201 status code for received data from server
                int status = connection.getResponseCode();
                switch (status){
                    case 200:
                    case 201:
                        //read in results sent from the server
                        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                        StringBuilder sb = new StringBuilder();
                        String line;
                        while ((line = bufferedReader.readLine()) != null){
                            sb.append(line + "\n");
                        }
                        bufferedReader.close();
    
                        //return received string
                        return sb.toString();
                }
    
            } catch (MalformedURLException ex) {
                Log.e("HTTP Client Error", ex.toString());
            } catch (IOException ex) {
                Log.e("HTTP Client Error", ex.toString());
            } catch (Exception ex) {
                Log.e("HTTP Client Error", ex.toString());
            } finally {
                if (connection != null) {
                    try{
                        connection.disconnect();
                    } catch (Exception ex) {
                        Log.e("HTTP Client Error", ex.toString());
                    }
                }
            }
            return null;
        }
    
        protected void onPostExecute(String message){
            //log values sent from the server - processed payment
            Log.i("HTTP Client", "Received Return: " + message);
        }
    }


  [1]: https://github.com/paypaldev/android-samples/tree/master/future-payment-sample

## Android Step 3: Node Server to Get Access Token & Process Payment
The complete sample code for this application (Android + Node server) is available in the [PayPal Developer Github repository][1].

From step 2, an async request has been made to our server at the `/fpstore` endpoint, passing along the auth code and metadata ID. We now need to exchange those for a token in order to complete the request and process the future payment.

First we set up our configuration variables and object.

    var bodyParser = require('body-parser'),
        http = require('http'),
        paypal = require('paypal-rest-sdk'),
        app = require('express')();
    
    var client_id = 'YOUR APPLICATION CLIENT ID';
    var secret = 'YOUR APPLICATION SECRET';
    
    paypal.configure({
        'mode': 'sandbox',
        'client_id': client_id,
        'client_secret': secret
    });
    
    app.use(bodyParser.urlencoded({ extended: false }))
    app.use(bodyParser.json());
    
Now we set up an Express route that will listen for POST requests sent to the `/fpstore` endpoint from our Android code.

We are doing a number of things in this route:

 - We capture the auth code and metadata ID from the POST body.
 - We then make a request to `generateToken()`, passing through the code object. If successful, we obtain a token that can be used to create the payment. 
 - Next, the config objects are created for the future payment that is to be made, and a request to `payment.create(...)` is made, passing along the future payment and payment config objects. This creates the future payment.


    app.post('/fpstore', function(req, res){
        var code = {'authorization_code': req.body.code};
        var metadata_id = req.body.metadataId;
        
        //generate token from provided code
        paypal.generateToken(code, function (error, refresh_token) {
            if (error) {
                console.log(error);
                console.log(error.response);
            } else {
                //create future payments config 
                var fp_config = {'client_metadata_id': metadata_id, 'refresh_token': refresh_token};
    
                //payment details
                var payment_config = {
                    "intent": "sale",
                    "payer": {
                        "payment_method": "paypal"
                    },
                    "transactions": [{
                        "amount": {
                            "currency": "USD",
                            "total": "3.50"
                        },
                        "description": "Mesozoic era monster toy"
                    }]
                };
    
                //process future payment
                paypal.payment.create(payment_config, fp_config, function (error, payment) {
                    if (error) {
                        console.log(error.response);
                        throw error;
                    } else {
                        console.log("Create Payment Response");
                        console.log(payment);
                        
                        //send payment object back to mobile
                        res.send(JSON.stringify(payment));
                    }
                });
            }
        });
    });
    
Lastly, we create the server on to listen on port 3000.

    //create server
    http.createServer(app).listen(3000, function () {
       console.log('Server started: Listening on port 3000');
    });


  [1]: https://github.com/paypaldev/android-samples/tree/master/future-payment-sample

