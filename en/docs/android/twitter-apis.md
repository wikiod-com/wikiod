---
title: "Twitter APIs"
slug: "twitter-apis"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Creating login with twitter button and attach a callback to it
1. Inside your layout, add a Login button with the following code:

        <com.twitter.sdk.android.core.identity.TwitterLoginButton
            android:id="@+id/twitter_login_button"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_centerInParent="true"/>


2. In the Activity or Fragment that displays the button, you need to create and attach a Callback to the Login Buttonas the following:

        import com.twitter.sdk.android.core.Callback;
        import com.twitter.sdk.android.core.Result;
        import com.twitter.sdk.android.core.TwitterException;
        import com.twitter.sdk.android.core.TwitterSession;
        import com.twitter.sdk.android.core.identity.TwitterLoginButton;
        ...

        loginButton = (TwitterLoginButton) findViewById(R.id.login_button);
        loginButton.setCallback(new Callback<TwitterSession>() {
            @Override
            public void success(Result<TwitterSession> result) {
                Log.d(TAG, "userName: " + session.getUserName());
                Log.d(TAG, "userId: " + session.getUserId());
                Log.d(TAG, "authToken: " + session.getAuthToken());
                Log.d(TAG, "id: " + session.getId());
                Log.d(TAG, "authToken: " + session.getAuthToken().token);
                Log.d(TAG, "authSecret: " + session.getAuthToken().secret);
            }

            @Override
            public void failure(TwitterException exception) {
                // Do something on failure
            }
        });


3. Pass the result of the authentication Activity back to the button:

        @Override
        protected void onActivityResult(int requestCode, int resultCode, Intent data) {
            super.onActivityResult(requestCode, resultCode, data);
            // Make sure that the loginButton hears the result from any
            // Activity that it triggered.
            loginButton.onActivityResult(requestCode, resultCode, data);
        }

   **Note,** If using the TwitterLoginButton in a Fragment, use the following steps instead:

        @Override
        protected void onActivityResult(int requestCode, int resultCode, Intent data) {
            super.onActivityResult(requestCode, resultCode, data);
    
            // Pass the activity result to the fragment, which will then pass the result to the login
            // button.
            Fragment fragment = getFragmentManager().findFragmentById(R.id.your_fragment_id);
            if (fragment != null) {
                fragment.onActivityResult(requestCode, resultCode, data);
            }
        }


4. Add the following lines to your **build.gradle** dependencies:

        apply plugin: 'io.fabric'

        repositories {
            maven { url 'https://maven.fabric.io/public' }
        }

        compile('com.twitter.sdk.android:twitter:1.14.1@aar') {
            transitive = true;
        }




