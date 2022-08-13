---
title: "Firebase"
slug: "firebase"
draft: false
images: []
weight: 9558
type: docs
toc: true
---

[Firebase][1] is a mobile and web application platform with tools and infrastructure designed to help developers build high-quality apps.


  [1]: https://firebase.google.com/
**Features**
    
   Firebase Cloud Messaging, Firebase Auth, Realtime Database, Firebase Storage, Firebase Hosting, Firebase Test Lab for Android, Firebase Crash Reporting.


Firebase - Extended documentation:
-----------------------------------------
There is [another tag][1] where you can find more topics and examples about the use of Firebase.    


Other related topics:
-----------------------------------------
- [Firebase Realtime DataBase][2]  
- [Firebase App Indexing][3]
- [Firebase Crash Reporting][4]
- [Firebase Cloud Messaging][5]


  [1]: https://www.wikiod.com/firebase
  [2]: https://www.wikiod.com/android/firebase-realtime-database
  [3]: https://www.wikiod.com/android/firebase-app-indexing
  [4]: https://www.wikiod.com/android/firebase-crash-reporting
  [5]: https://www.wikiod.com/android

## Add Firebase to Your Android Project
Here are simplified steps (based on the [official documentation][1]) required to create a Firebase project and connect it with an Android app.

# Add Firebase to your app


1. Create a Firebase project in the [Firebase console][2] and click **Create New Project**.

2. Click **Add Firebase to your Android app** and follow the setup steps. 

3. When prompted, enter your **app's package name**.  
It's important to enter the fully qualified package name your app is using; this can only be set when you add an app to your Firebase project.

4. At the end, you'll download a `google-services.json` file. You can download this file again at any time.

5. If you haven't done so already, copy the `google-services.json` file into your project's module folder, typically `app/`.

The next step is to Add the SDK to integrate the Firebase libraries in the project.

# Add the SDK

To integrate the Firebase libraries into one of your own projects, you need to perform a few basic tasks to prepare your Android Studio project. You may have already done this as part of adding Firebase to your app.

1. Add rules to your root-level `build.gradle` file, to include the **google-services plugin**:


    buildscript {
        // ...
        dependencies {
            // ...
            classpath 'com.google.gms:google-services:3.1.0'
        }
    }

Then, in your module Gradle file (usually the `app/build.gradle`), add the apply plugin line at the bottom of the file to enable the Gradle plugin:

    apply plugin: 'com.android.application'
    
    android {
      // ...
    }
    
    dependencies {
      // ...
      compile 'com.google.firebase:firebase-core:11.0.4'
    }
    
    // ADD THIS AT THE BOTTOM
    apply plugin: 'com.google.gms.google-services'

The final step is to add the dependencies for the Firebase SDK using one or more
**libraries available** for the different Firebase features.

|Gradle Dependency Line  |  Service |
|------------------------|----------|
|com.google.firebase:firebase-core:11.0.4  |  Analytics|
|com.google.firebase:firebase-database:11.0.4 |  Realtime Database|
|com.google.firebase:firebase-storage:11.0.4 |   Storage|
|com.google.firebase:firebase-crash:11.0.4 |   Crash Reporting|
|com.google.firebase:firebase-auth:11.0.4 |  Authentication|
|com.google.firebase:firebase-messaging:11.0.4 |   Cloud Messaging / Notifications|
|com.google.firebase:firebase-config:11.0.4 |  Remote Config|
|com.google.firebase:firebase-invites:11.0.4 |   Invites / Dynamic Links|
|com.google.firebase:firebase-ads:11.0.4 |   AdMob|
|com.google.android.gms:play-services-appindexing:11.0.4 |  App Indexing|


  [1]: https://firebase.google.com/docs/android/setup
  [2]: https://firebase.google.com/console/

## Updating a Firebase users's email
    public class ChangeEmailActivity extends BaseAppCompatActivity implements ReAuthenticateDialogFragment.OnReauthenticateSuccessListener {
    
        @BindView(R.id.et_change_email)
        EditText mEditText;
        private FirebaseUser mFirebaseUser;
    
        @OnClick(R.id.btn_change_email)
        void onChangeEmailClick() {
    
            FormValidationUtils.clearErrors(mEditText);
    
            if (FormValidationUtils.isBlank(mEditText)) {
                FormValidationUtils.setError(null, mEditText, "Please enter email");
                return;
            }
    
            if (!FormValidationUtils.isEmailValid(mEditText)) {
                FormValidationUtils.setError(null, mEditText, "Please enter valid email");
                return;
            }
    
            changeEmail(mEditText.getText().toString());
        }
    
        @Override
        protected void onCreate(@Nullable Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            mFirebaseUser = mFirebaseAuth.getCurrentUser();
        }
    
        private void changeEmail(String email) {
            DialogUtils.showProgressDialog(this, "Changing Email", "Please wait...", false);
            mFirebaseUser.updateEmail(email)
                    .addOnCompleteListener(new OnCompleteListener<Void>() {
                        @Override
                        public void onComplete(@NonNull Task<Void> task) {
                            DialogUtils.dismissProgressDialog();
                            if (task.isSuccessful()) {
                                showToast("Email updated successfully.");
                                return;
                            }
    
                            if (task.getException() instanceof FirebaseAuthRecentLoginRequiredException) {
                                FragmentManager fm = getSupportFragmentManager();
                                ReAuthenticateDialogFragment reAuthenticateDialogFragment = new ReAuthenticateDialogFragment();
                                reAuthenticateDialogFragment.show(fm, reAuthenticateDialogFragment.getClass().getSimpleName());
                            }
                        }
                    });
        }
    
        @Override
        protected int getLayoutResourceId() {
            return R.layout.activity_change_email;
        }
    
        @Override
        public void onReauthenticateSuccess() {
            changeEmail(mEditText.getText().toString());
        }
    }

## Create a Firebase user
    public class SignUpActivity extends BaseAppCompatActivity {
    
        @BindView(R.id.tIETSignUpEmail)
        EditText mEditEmail;
        @BindView(R.id.tIETSignUpPassword)
        EditText mEditPassword;
    
        @Override
        protected void onCreate(@Nullable Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        }
    
        @OnClick(R.id.btnSignUpSignUp)
        void signUp() {
    
            FormValidationUtils.clearErrors(mEditEmail, mEditPassword);
    
            if (FormValidationUtils.isBlank(mEditEmail)) {
                mEditEmail.setError("Please enter email");
                return;
            }
    
            if (!FormValidationUtils.isEmailValid(mEditEmail)) {
                mEditEmail.setError("Please enter valid email");
                return;
            }
    
            if (TextUtils.isEmpty(mEditPassword.getText())) {
                mEditPassword.setError("Please enter password");
                return;
            }
    
            createUserWithEmailAndPassword(mEditEmail.getText().toString(), mEditPassword.getText().toString());
        }
    
        private void createUserWithEmailAndPassword(String email, String password) {
            DialogUtils.showProgressDialog(this, "", getString(R.string.str_creating_account), false);
            mFirebaseAuth
                    .createUserWithEmailAndPassword(email, password)
                    .addOnCompleteListener(this, new OnCompleteListener<AuthResult>() {
                        @Override
                        public void onComplete(@NonNull Task<AuthResult> task) {
                            if (!task.isSuccessful()) {
                                Toast.makeText(SignUpActivity.this, task.getException().getMessage(),
                                        Toast.LENGTH_SHORT).show();
                                DialogUtils.dismissProgressDialog();
                            } else {
                                Toast.makeText(SignUpActivity.this, R.string.str_registration_successful, Toast.LENGTH_SHORT).show();
                                DialogUtils.dismissProgressDialog();
                                startActivity(new Intent(SignUpActivity.this, HomeActivity.class));
                            }
                        }
                    });
        }
    
        @Override
        protected int getLayoutResourceId() {
            return R.layout.activity_sign_up;
        }
    }

## Change Password
    public class ChangePasswordActivity extends BaseAppCompatActivity implements ReAuthenticateDialogFragment.OnReauthenticateSuccessListener {
        @BindView(R.id.et_change_password)
        EditText mEditText;
        private FirebaseUser mFirebaseUser;
    
        @OnClick(R.id.btn_change_password)
        void onChangePasswordClick() {
    
            FormValidationUtils.clearErrors(mEditText);
    
            if (FormValidationUtils.isBlank(mEditText)) {
                FormValidationUtils.setError(null, mEditText, "Please enter password");
                return;
            }
    
            changePassword(mEditText.getText().toString());
        }
    
        private void changePassword(String password) {
            DialogUtils.showProgressDialog(this, "Changing Password", "Please wait...", false);
            mFirebaseUser.updatePassword(password)
                    .addOnCompleteListener(new OnCompleteListener<Void>() {
                        @Override
                        public void onComplete(@NonNull Task<Void> task) {
                            DialogUtils.dismissProgressDialog();
                            if (task.isSuccessful()) {
                                showToast("Password updated successfully.");
                                return;
                            }
    
                            if (task.getException() instanceof FirebaseAuthRecentLoginRequiredException) {
                                FragmentManager fm = getSupportFragmentManager();
                                ReAuthenticateDialogFragment reAuthenticateDialogFragment = new ReAuthenticateDialogFragment();
                                reAuthenticateDialogFragment.show(fm, reAuthenticateDialogFragment.getClass().getSimpleName());
                            }
                        }
                    });
        }
    
        @Override
        protected void onCreate(@Nullable Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            mFirebaseUser = mFirebaseAuth.getCurrentUser();
        }
    
        @Override
        protected int getLayoutResourceId() {
            return R.layout.activity_change_password;
        }
    
        @Override
        public void onReauthenticateSuccess() {
            changePassword(mEditText.getText().toString());
        }
    }

## Firebase Cloud Messaging
First of all you need to setup your project adding Firebase to your Android project following the [steps described in this topic][1].

# Set up Firebase and the FCM SDK

Add the FCM dependency to your app-level `build.gradle` file

    dependencies {
     compile 'com.google.firebase:firebase-messaging:11.0.4'
    }

And at the very bottom (this is important) add:

    // ADD THIS AT THE BOTTOM
    apply plugin: 'com.google.gms.google-services'

# Edit your app manifest

Add the following to your app's manifest:

- A service that extends `FirebaseMessagingService`. This is required if you want to do any message handling beyond receiving notifications on apps in the background. 

- A service that extends `FirebaseInstanceIdService` to handle the creation, rotation, and updating of registration tokens. 

For example:

        <service
            android:name=".MyInstanceIdListenerService">
            <intent-filter>
                <action android:name="com.google.firebase.INSTANCE_ID_EVENT"/>
            </intent-filter>
        </service>
        <service
            android:name=".MyFcmListenerService">
            <intent-filter>
                <action android:name="com.google.firebase.MESSAGING_EVENT" />
            </intent-filter>
        </service>

Here are simple implementations of the 2 services.

To retrieve the current registration token extend the `FirebaseInstanceIdService` class and override the `onTokenRefresh()` method:

    public class MyInstanceIdListenerService extends FirebaseInstanceIdService {

        // Called if InstanceID token is updated. Occurs if the security of the previous token had been
        // compromised. This call is initiated by the InstanceID provider.
        @Override
        public void onTokenRefresh() {
            // Get updated InstanceID token.
            String refreshedToken = FirebaseInstanceId.getInstance().getToken();
    
            // Send this token to your server or store it locally
        }
    }


To receive messages, use a service that extends `FirebaseMessagingService` and override the `onMessageReceived` method.


    public class MyFcmListenerService extends FirebaseMessagingService {
        
        /**
         * Called when message is received.
         *
         * @param remoteMessage Object representing the message received from Firebase Cloud Messaging.
         */
        @Override
        public void onMessageReceived(RemoteMessage remoteMessage) {
            String from = remoteMessage.getFrom();

            // Check if message contains a data payload.
            if (remoteMessage.getData().size() > 0) {
                Log.d(TAG, "Message data payload: " + remoteMessage.getData());
                Map<String, String> data = remoteMessage.getData();
            }
    
            // Check if message contains a notification payload.
            if (remoteMessage.getNotification() != null) {
                Log.d(TAG, "Message Notification Body: " + remoteMessage.getNotification().getBody());
            }

            // do whatever you want with this, post your own notification, or update local state
        }

in **Firebase** can grouped user by their behavior like "AppVersion,free user,purchase user,or any specific rules" and then send notification to specific group by send **Topic** Feature in fireBase.  
to register user in topic use 

    FirebaseMessaging.getInstance().subscribeToTopic("Free");

then in fireBase console, send notification by topic name

More info in the dedicated topic [Firebase Cloud Messaging][2].

  [1]: https://www.wikiod.com/android/firebase#Add Firebase to Your Android Project
 [2]: https://www.wikiod.com/android


## Sign In Firebase user with email and password
    public class LoginActivity extends BaseAppCompatActivity {
    
        @BindView(R.id.tIETLoginEmail)
        EditText mEditEmail;
        @BindView(R.id.tIETLoginPassword)
        EditText mEditPassword;
    
        @Override
        protected void onResume() {
            super.onResume();
            FirebaseUser firebaseUser = mFirebaseAuth.getCurrentUser();
            if (firebaseUser != null)
                startActivity(new Intent(this, HomeActivity.class));
        }
    
        @Override
        protected int getLayoutResourceId() {
            return R.layout.activity_login;
        }
    
        @OnClick(R.id.btnLoginLogin)
        void onSignInClick() {
    
            FormValidationUtils.clearErrors(mEditEmail, mEditPassword);
    
            if (FormValidationUtils.isBlank(mEditEmail)) {
                FormValidationUtils.setError(null, mEditEmail, "Please enter email");
                return;
            }
    
            if (!FormValidationUtils.isEmailValid(mEditEmail)) {
                FormValidationUtils.setError(null, mEditEmail, "Please enter valid email");
                return;
            }
    
            if (TextUtils.isEmpty(mEditPassword.getText())) {
                FormValidationUtils.setError(null, mEditPassword, "Please enter password");
                return;
            }
    
            signInWithEmailAndPassword(mEditEmail.getText().toString(), mEditPassword.getText().toString());
        }
    
        private void signInWithEmailAndPassword(String email, String password) {
            DialogUtils.showProgressDialog(this, "", getString(R.string.sign_in), false);
            mFirebaseAuth
                    .signInWithEmailAndPassword(email, password)
                    .addOnCompleteListener(this, new OnCompleteListener<AuthResult>() {
                        @Override
                        public void onComplete(@NonNull Task<AuthResult> task) {
    
                            DialogUtils.dismissProgressDialog();
    
                            if (task.isSuccessful()) {
                                Toast.makeText(LoginActivity.this, "Login Successful", Toast.LENGTH_SHORT).show();
                                startActivity(new Intent(LoginActivity.this, HomeActivity.class));
                                finish();
                            } else {
                                Toast.makeText(LoginActivity.this, task.getException().getMessage(),
                                        Toast.LENGTH_SHORT).show();
                            }
                        }
                    });
        }
    
        @OnClick(R.id.btnLoginSignUp)
        void onSignUpClick() {
            startActivity(new Intent(this, SignUpActivity.class));
        }
    
    
        @OnClick(R.id.btnLoginForgotPassword)
        void forgotPassword() {
            startActivity(new Intent(this, ForgotPasswordActivity.class));
        }
    }

## Send Firebase password reset email
    public class ForgotPasswordActivity extends AppCompatActivity {
    
        @BindView(R.id.tIETForgotPasswordEmail)
        EditText mEditEmail;
        private FirebaseAuth mFirebaseAuth;
        private FirebaseAuth.AuthStateListener mAuthStateListener;
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_forgot_password);
            ButterKnife.bind(this);
    
            mFirebaseAuth = FirebaseAuth.getInstance();
    
            mAuthStateListener = new FirebaseAuth.AuthStateListener() {
                @Override
                public void onAuthStateChanged(@NonNull FirebaseAuth firebaseAuth) {
                    FirebaseUser firebaseUser = firebaseAuth.getCurrentUser();
                    if (firebaseUser != null) {
                        // Do whatever you want with the UserId by firebaseUser.getUid()
                    } else {
    
                    }
                }
            };
        }
    
        @Override
        protected void onStart() {
            super.onStart();
            mFirebaseAuth.addAuthStateListener(mAuthStateListener);
        }
    
        @Override
        protected void onStop() {
            super.onStop();
            if (mAuthStateListener != null) {
                mFirebaseAuth.removeAuthStateListener(mAuthStateListener);
            }
        }
    
        @OnClick(R.id.btnForgotPasswordSubmit)
        void onSubmitClick() {
    
            if (FormValidationUtils.isBlank(mEditEmail)) {
                FormValidationUtils.setError(null, mEditEmail, "Please enter email");
                return;
            }
    
            if (!FormValidationUtils.isEmailValid(mEditEmail)) {
                FormValidationUtils.setError(null, mEditEmail, "Please enter valid email");
                return;
            }
    
            DialogUtils.showProgressDialog(this, "", "Please wait...", false);
            mFirebaseAuth.sendPasswordResetEmail(mEditEmail.getText().toString())
                    .addOnCompleteListener(new OnCompleteListener<Void>() {
                        @Override
                        public void onComplete(@NonNull Task<Void> task) {
                            DialogUtils.dismissProgressDialog();
                            if (task.isSuccessful()) {
                                Toast.makeText(ForgotPasswordActivity.this, "An email has been sent to you.", Toast.LENGTH_SHORT).show();
                                finish();
                            } else {
                                Toast.makeText(ForgotPasswordActivity.this, task.getException().getMessage(), Toast.LENGTH_SHORT).show();
                            }
                        }
                    });
        }
    }

## Re-Authenticate Firebase user
    public class ReAuthenticateDialogFragment extends DialogFragment {
    
        @BindView(R.id.et_dialog_reauthenticate_email)
        EditText mEditTextEmail;
        @BindView(R.id.et_dialog_reauthenticate_password)
        EditText mEditTextPassword;
        private OnReauthenticateSuccessListener mOnReauthenticateSuccessListener;
    
        @OnClick(R.id.btn_dialog_reauthenticate)
        void onReauthenticateClick() {
    
            FormValidationUtils.clearErrors(mEditTextEmail, mEditTextPassword);
    
            if (FormValidationUtils.isBlank(mEditTextEmail)) {
                FormValidationUtils.setError(null, mEditTextEmail, "Please enter email");
                return;
            }
    
            if (!FormValidationUtils.isEmailValid(mEditTextEmail)) {
                FormValidationUtils.setError(null, mEditTextEmail, "Please enter valid email");
                return;
            }
    
            if (TextUtils.isEmpty(mEditTextPassword.getText())) {
                FormValidationUtils.setError(null, mEditTextPassword, "Please enter password");
                return;
            }
    
            reauthenticateUser(mEditTextEmail.getText().toString(), mEditTextPassword.getText().toString());
        }
    
        private void reauthenticateUser(String email, String password) {
            DialogUtils.showProgressDialog(getActivity(), "Re-Authenticating", "Please wait...", false);
            FirebaseUser firebaseUser = FirebaseAuth.getInstance().getCurrentUser();
            AuthCredential authCredential = EmailAuthProvider.getCredential(email, password);
            firebaseUser.reauthenticate(authCredential)
                    .addOnCompleteListener(new OnCompleteListener<Void>() {
                        @Override
                        public void onComplete(@NonNull Task<Void> task) {
                            DialogUtils.dismissProgressDialog();
                            if (task.isSuccessful()) {
                                mOnReauthenticateSuccessListener.onReauthenticateSuccess();
                                dismiss();
                            } else {
                                ((BaseAppCompatActivity) getActivity()).showToast(task.getException().getMessage());
                            }
                        }
                    });
        }
    
        @Override
        public void onAttach(Context context) {
            super.onAttach(context);
            mOnReauthenticateSuccessListener = (OnReauthenticateSuccessListener) context;
        }
    
        @OnClick(R.id.btn_dialog_reauthenticate_cancel)
        void onCancelClick() {
            dismiss();
        }
    
        @Override
        public View onCreateView(LayoutInflater inflater, ViewGroup container,
                                 Bundle savedInstanceState) {
            View view = inflater.inflate(R.layout.dialog_reauthenticate, container);
            ButterKnife.bind(this, view);
            return view;
        }
    
        @Override
        public void onResume() {
            super.onResume();
            Window window = getDialog().getWindow();
            window.setLayout(WindowManager.LayoutParams.MATCH_PARENT, WindowManager.LayoutParams.WRAP_CONTENT);
        }
    
        interface OnReauthenticateSuccessListener {
            void onReauthenticateSuccess();
        }
    }

## Firebase Storage Operations


## Firebase Realtime Database: how to set/get data
**Note:** Let's setup some anonymous authentication for the example

    {
      "rules": {
        ".read": "auth != null",
        ".write": "auth != null"
      }
    }

Once it is done, create a child by editing your database address. For example:

https://your-project.firebaseio.com/ to **https://your-project.firebaseio.com/chat**

We will put data to this location from our Android device. You **don't have to** create the database structure (tabs, fields... etc), it will be automatically created when you'll send Java object to Firebase!

Create a Java object that contains all the attributes you want to send to the database:

    public class ChatMessage {
        private String username;
        private String message;
    
        public ChatMessage(String username, String message) {
            this.username = username;
            this.message = message;
        }
    
        public ChatMessage() {} // you MUST have an empty constructor
    
        public String getUsername() {
            return username;
        }
    
        public String getMessage() {
            return message;
        }
    }

Then in your activity:

    if (FirebaseAuth.getInstance().getCurrentUser() == null) {
            FirebaseAuth.getInstance().signInAnonymously().addOnCompleteListener(new OnCompleteListener<AuthResult>() {
                @Override
                public void onComplete(@NonNull Task<AuthResult> task) {
                    if (task.isComplete() && task.isSuccessful()){
                        FirebaseDatabase database = FirebaseDatabase.getInstance();
                        DatabaseReference reference = database.getReference("chat"); // reference is 'chat' because we created the database at /chat
                    }
                }
            });
    }

  
To send a value:

    ChatMessage msg = new ChatMessage("user1", "Hello World!");
    reference.push().setValue(msg);

To receive changes that occurs in the database:

    reference.addChildEventListener(new ChildEventListener() {
        @Override
        public void onChildAdded(DataSnapshot dataSnapshot, String s) {
            ChatMessage msg = dataSnapshot.getValue(ChatMessage.class);
            Log.d(TAG, msg.getUsername()+" "+msg.getMessage());
        }
    
        public void onChildChanged(DataSnapshot dataSnapshot, String s) {}
        public void onChildRemoved(DataSnapshot dataSnapshot) {}
        public void onChildMoved(DataSnapshot dataSnapshot, String s) {}
        public void onCancelled(DatabaseError databaseError) {}
    });

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/xRSuq.png

## Demo of FCM based notifications
This example shows how to use the Firebase Cloud Messaging(FCM) platform.
FCM is a successor of Google Cloud Messaging(GCM). It does not require C2D_MESSAGE permissions from the app users.

Steps to integrate FCM are as follows.
 1. Create sample hello world project in Android Studio
    Your Android studio screen would look like the following picture.
[![Demo Project screen with basic activity in Android Studio][1]][1]
 2. Next step is to set up firebase project. Visit https://console.firebase.google.com and create a project with an identical name, so that you can track it easily.[![enter image description here][2]][2]
 3. Now it is time to add firebase to your sample android project you have just created. You will need package name of your project and Debug signing certificate SHA-1(optional).

    a. Package name - It can be found from the android manifest XML file.
    
    b. Debug signing SHA-1 certificate - It can be found by running following command in the terminal.

`keytool -list -v -keystore ~/.android/debug.keystore -alias androiddebugkey -storepass android -keypass android`

   Enter this information in the firebase console and add the app to firebase project. Once you click on add app button, your browser would automatically download a JSON file named "google-services.json".

 4. Now copy the google-services.json file you have just downloaded into your Android app module root directory.[![enter image description here][3]][3]

 5. Follow the instructions given on the firebase console as you proceed ahead.
    a. Add following code line to your project level build.gradle

    `dependencies{
        classpath 'com.google.gms:google-services:3.1.0' .....`

    b. Add following code line at the end of your app level build.gradle.
        
            //following are the dependencies to be added
            compile 'com.google.firebase:firebase-messaging:11.0.4'
            compile 'com.android.support:multidex:1.0.1'
        }
        // this line goes to the end of the file
        apply plugin: 'com.google.gms.google-services'
    
    c. Android studio would ask you to sync project. Click on Sync now.

 6. Next task is to add two services.
    a. One extending FirebaseMessagingService with intent-filter as following

            <intent-filter>
                <action android:name="com.google.firebase.MESSAGING_EVENT"/>
            </intent-filter>
    b. One extending FirebaseInstanceIDService.
        
        <intent-filter>
            <action android:name="com.google.firebase.INSTANCE_ID_EVENT"/>
        </intent-filter>

 7. FirebaseMessagingService code should look like this.
        
        import android.app.Service;
        import android.content.Intent;
        import android.os.IBinder;

        import com.google.firebase.messaging.FirebaseMessagingService;

        public class MyFirebaseMessagingService extends FirebaseMessagingService {
            public MyFirebaseMessagingService() {
            }
        }

 8. FirebaseInstanceIdService should look like this.
        
        import android.app.Service;
        import android.content.Intent;
        import android.os.IBinder;
        
        import com.google.firebase.iid.FirebaseInstanceIdService;
        
        public class MyFirebaseInstanceIDService extends FirebaseInstanceIdService {
            public MyFirebaseInstanceIDService() {
            }
        }

 9. Now it is time to capture the device registration token. Add following line of code to MainActivity's onCreate method. 

        String token = FirebaseInstanceId.getInstance().getToken();
        Log.d("FCMAPP", "Token is "+token);

 10. Once we have the access token, we can use firebase console to send out the notification. Run the app on your android handset. [![Firebase Console Notification][4]][4]

Click on Notification in Firebase console and UI will help you to send out your first message. Firebase offers functionality to send messages to single device(By using the device token id we captured) or all the users using our app or to specific group of users. Once you send your first message, your mobile screen should look like following.

[![Notification][5]][5]

Thank you

  [1]: http://i.stack.imgur.com/3k33n.png
  [2]: http://i.stack.imgur.com/sK4vn.png
  [3]: http://i.stack.imgur.com/ih9DF.png
  [4]: http://i.stack.imgur.com/Rn47F.png
  [5]: http://i.stack.imgur.com/sin55.png

## Firebase Sign Out


