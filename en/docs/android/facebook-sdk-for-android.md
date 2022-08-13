---
title: "Facebook SDK for Android"
slug: "facebook-sdk-for-android"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## Syntax
- **newInstance** : To create single instance of Facebook helper class.
- **loginUser** : To login user.
- **signOut** : To log out user.
- **getCallbackManager** : To get callback for Facebook.
- **getLoginCallback** : To get callback for Login.
- **getKeyHash** : To generate Facebook Key Hash.

## Parameters
| Parameter | Details |
| ------ | ------ |
| TAG   | A String used while logging |
| FacebookSignInHelper | A static reference to facebook helper |
| CallbackManager | A callback for facebook operations |
| Activity | A context |
| PERMISSION_LOGIN | An array that contains all permission required from facebook to login.|
| loginCallback | A callback for facebook login |

## How to add Facebook Login  in Android
Add below dependencies to your `build.gradle`

      // Facebook login
        compile 'com.facebook.android:facebook-android-sdk:4.21.1'

Add below helper class to your utility package:

    /**
     * Created by Andy
     * An utility for Facebook
     */
    public class FacebookSignInHelper {
        private static final String TAG = FacebookSignInHelper.class.getSimpleName();
        private static FacebookSignInHelper facebookSignInHelper = null;
        private CallbackManager callbackManager;
        private Activity mActivity;
        private static final Collection<String> PERMISSION_LOGIN = (Collection<String>) Arrays.asList("public_profile", "user_friends","email");
        private FacebookCallback<LoginResult> loginCallback;
    
    
    
        public static FacebookSignInHelper newInstance(Activity context) {
            if (facebookSignInHelper == null)
                facebookSignInHelper = new FacebookSignInHelper(context);
            return facebookSignInHelper;
        }
    
    
        public FacebookSignInHelper(Activity mActivity) {
            try {
                this.mActivity = mActivity;
                // Initialize the SDK before executing any other operations,
                // especially, if you're using Facebook UI elements.
                FacebookSdk.sdkInitialize(this.mActivity);
                callbackManager = CallbackManager.Factory.create();
                loginCallback = new FacebookCallback<LoginResult>() {
                    @Override
                    public void onSuccess(LoginResult loginResult) {
                       // You are logged into Facebook
                    }
    
                    @Override
                    public void onCancel() {
                        Log.d(TAG, "Facebook: Cancelled by user");
                    }
    
                    @Override
                    public void onError(FacebookException error) {
                        Log.d(TAG, "FacebookException: " + error.getMessage());
                    }
                };
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    
        /**
         * To login user on facebook without default Facebook button
         */
        public void loginUser() {
            try {
                LoginManager.getInstance().registerCallback(callbackManager, loginCallback);
                LoginManager.getInstance().logInWithReadPermissions(this.mActivity, PERMISSION_LOGIN);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
    
        /**
         * To log out user from facebook
         */
        public void signOut() {
            // Facebook sign out
            LoginManager.getInstance().logOut();
        }
    
        public CallbackManager getCallbackManager() {
            return callbackManager;
        }
    
        public FacebookCallback<LoginResult> getLoginCallback() {
            return loginCallback;
        }
    
        /**
         * Attempts to log debug key hash for facebook
         *
         * @param context : A reference to context
         * @return : A facebook debug key hash
         */
        public static String getKeyHash(Context context) {
            String keyHash = null;
            try {
                PackageInfo info = context.getPackageManager().getPackageInfo(
                        context.getPackageName(),
                        PackageManager.GET_SIGNATURES);
                for (Signature signature : info.signatures) {
                    MessageDigest md = MessageDigest.getInstance("SHA");
                    md.update(signature.toByteArray());
                    keyHash = Base64.encodeToString(md.digest(), Base64.DEFAULT);
                    Log.d(TAG, "KeyHash:" + keyHash);
                }
            } catch (PackageManager.NameNotFoundException e) {
                e.printStackTrace();
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            } catch (Exception e) {
                e.printStackTrace();
            }
            return keyHash;
        }
    }

Add below code in Your Activity:

    FacebookSignInHelper facebookSignInHelper = FacebookSignInHelper.newInstance(LoginActivity.this, fireBaseAuthHelper);
    facebookSignInHelper.loginUser();

Add below code to your `OnActivityResult`:

     facebookSignInHelper.getCallbackManager().onActivityResult(requestCode, resultCode, data);





## Create your own custom button for Facebook login


## A minimalistic guide to Facebook login/signup implementation
 1. You have to setup the [prerequisites][1].
 2. Add the Facebook activity to the _AndroidManifest.xml_ file:

        <activity 
            android:name="com.facebook.FacebookActivity"
            android:configChanges= "keyboard|keyboardHidden|screenLayout|screenSize|orientation"
            android:theme="@android:style/Theme.Translucent.NoTitleBar"
            android:label="@string/app_name" />

3. Add the login button to your layout XML file:

       <com.facebook.login.widget.LoginButton
           android:id="@+id/login_button"
           android:layout_width="wrap_content"
           android:layout_height="wrap_content" />   

4. Now you have the Facebook button. If the user clicks on it, the Facebook login dialog will come up on top of the app's screen. Here the user can fill in their credentials and press the _Log In_ button. If the credentials are correct, the dialog grants the corresponding permissions and a callback is sent to your original activity containing the button. The following code shows how you can receive that callback:

       loginButton.registerCallback(callbackManager, new FacebookCallback<LoginResult>() {
           @Override
           public void onSuccess(LoginResult loginResult) {
               // Completed without error. You might want to use the retrieved data here.
           }
    
           @Override
           public void onCancel() {
               // The user either cancelled the Facebook login process or didn't authorize the app.
           }
    
           @Override
           public void onError(FacebookException exception) {
               // The dialog was closed with an error. The exception will help you recognize what exactly went wrong.
           }
       });  

  [1]: https://developers.facebook.com/docs/facebook-login/android#prerequisites

## Setting permissions to access data from the Facebook profile


## Logging out of Facebook


