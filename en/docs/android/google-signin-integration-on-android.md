---
title: "Google signin integration on android"
slug: "google-signin-integration-on-android"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

This topic is based on How to integrate google sign-in, On android apps

## Integration of google Auth in your project. (Get a configuration file)
First get the Configuration File for Sign-in from 

Open link below

[https://developers.google.com/identity/sign-in/android/start-integrating][1]


click on get A configuration file 

 - Enter App name And package name  and click on choose and configure services 
 - [provide SHA1][1] Enable google SIGNIN and generate configuration files

Download the configuration file and place the file in app/ folder of your project 


  [1]: http://stackoverflow.com/a/33479550/4044380

 1. Add the dependency to your project-level build.gradle:

> classpath 'com.google.gms:google-services:3.0.0'

 2. Add the plugin to your app-level build.gradle:(bottom)

> apply plugin: 'com.google.gms.google-services'

 3. add this dependency to your app gradle file

     

> dependencies {
>         compile 'com.google.android.gms:play-services-auth:9.8.0'
>     }



## Code Implementation Google SignIn
 - In your sign-in activity's onCreate method, configure Google Sign-In to request the user data required by your app.

   

     GoogleSignInOptions gso = new GoogleSignInOptions.Builder(GoogleSignInOptions.DEFAULT_SIGN_IN)
            .requestEmail()
            .build();

 

- create a GoogleApiClient object with access to the Google Sign-In API and the options you specified.

   

     mGoogleApiClient = new GoogleApiClient.Builder(this)
            .enableAutoManage(this /* FragmentActivity */, this /* OnConnectionFailedListener */)
            .addApi(Auth.GOOGLE_SIGN_IN_API, gso)
            .build();

 - Now When User click on Google signin button call this Function.

        private void signIn() {
        Intent signInIntent = Auth.GoogleSignInApi.getSignInIntent(mGoogleApiClient);
        startActivityForResult(signInIntent, RC_SIGN_IN);
       }

 - implement OnActivityResult to get the response.

   

     @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
    
        // Result returned from launching the Intent from GoogleSignInApi.getSignInIntent(...);
        if (requestCode == RC_SIGN_IN) {
            GoogleSignInResult result = Auth.GoogleSignInApi.getSignInResultFromIntent(data);
            handleSignInResult(result);
        }
    }

 - Last step Handle The Result and get User Data

        private void handleSignInResult(GoogleSignInResult result) {
        Log.d(TAG, "handleSignInResult:" + result.isSuccess());
        if (result.isSuccess()) {
            // Signed in successfully, show authenticated UI.
            GoogleSignInAccount acct = result.getSignInAccount();
            mStatusTextView.setText(getString(R.string.signed_in_fmt, acct.getDisplayName()));
            updateUI(true);
        } else {
            // Signed out, show unauthenticated UI.
            updateUI(false);
        }
       }






 



