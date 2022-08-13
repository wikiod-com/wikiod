---
title: "Activity"
slug: "activity"
draft: false
images: []
weight: 9833
type: docs
toc: true
---

An Activity represents a single screen with a user **interface(UI)**. An Android App may have more than one Activity, for example, An email App can have one activity to list all the emails, another activity to show email contents, yet another activity to compose new email. All the activities in an App work together to create perfect user experience.

## Syntax
 - void onCreate(Bundle savedInstanceState) // Called when the activity is starting. 

 - void onPostCreate(Bundle savedInstanceState) // Called when activity start-up is complete (after onStart() and onRestoreInstanceState(Bundle) have been called). 

 - void onStart() // Called after onCreate(Bundle) — or after onRestart() when the activity had been stopped, but is now again being displayed to the user. 

 - void onResume() // Called after onRestoreInstanceState(Bundle), onRestart(), or onPause(), for your activity to start interacting with the user. 

 - void onPostResume() // Called when activity resume is complete (after onResume() has been called). 

 - void onRestart() // Called after onStop() when the current activity is being re-displayed to the user (the user has navigated back to it).

 - void onPause() // Called as part of the activity lifecycle when an activity is going into the background, but has not (yet) been killed. 

 - void onStop() // Called when you are no longer visible to the user. 

 - void onDestroy() // Perform any final cleanup before an activity is destroyed. 

 - void onNewIntent(Intent intent) // This is called for activities that set launchMode to "singleTop" in their package, or if a client used the FLAG_ACTIVITY_SINGLE_TOP flag when calling startActivity(Intent). 

 - void onSaveInstanceState(Bundle outState) // Called to retrieve per-instance state from an activity before being killed so that the state can be restored in onCreate(Bundle) or onRestoreInstanceState(Bundle) (the Bundle populated by this method will be passed to both). 

 - void onRestoreInstanceState(Bundle savedInstanceState) // This method is called after onStart() when the activity is being re-initialized from a previously saved state, given here in savedInstanceState. 

## Parameters
| Parameter | Details |
| ------ | ------ |
| [Intent][1]   |  Can be used with [startActivity][2] to launch an Activity   |
| [Bundle][3] | A mapping from String keys to various [Parcelable][4] values. |
| [Context][5] | Interface to global information about an application environment. |


  [1]: https://www.wikiod.com/android/intent
  [2]: https://developer.android.com/reference/android/content/Context.html#startActivity(android.content.Intent)
  [3]: https://developer.android.com/reference/android/os/Bundle.html
  [4]: https://developer.android.com/reference/android/os/Parcelable.html
  [5]: https://developer.android.com/reference/android/content/Context.html

An [Activity][1] is an application component that provides a screen with which users can interact in order to do something, such as dial the phone, take a photo, send an email, or view a map. Each activity is given a window in which to draw its user interface. The window typically fills the screen, but may be smaller than the screen and float on top of other windows.


  [1]: https://developer.android.com/reference/android/app/Activity.html

## Activity launchMode
Launch mode defines the behaviour of new or existing activity in the task.<br/>
There are possible launch modes:

 - standard
 - singleTop
 - singleTask
 - singleInstance

It should be defined in android manifest in `<activity/>` element as `android:launchMode` attribute.

    <activity
        android:launchMode=["standard" | "singleTop" | "singleTask" | "singleInstance"] />
<br/>

<h1>Standard:</h1>

Default value. If this mode set, new activity will always be created for each new intent. So it's possible to get many activities of same type. New activity will be placed on the top of the task. There is some difference for different android version: if activity is starting from another application, on androids <= 4.4 it will be placed on same task as starter application, but on >= 5.0 new task will be created.


<h1>SingleTop:</h1>

This mode is almost the same as `standard`. Many instances of singleTop activity could be created. The difference is, if an instance of activity already exists on the top of the current stack, `onNewIntent()` will be called instead of creating new instance.


<h1>SingleTask:</h1>

Activity with this launch mode can have only one instance **in the system**. New task for activity will be created, if it doesn't exist. Otherwise, task with activity will be moved to front and `onNewIntent` will be called.


<h1>SingleInstance:</h1>

This mode is similar to `singleTask`. The difference is task that holds an activity with `singleInstance` could have only this activity and nothing more. When `singleInstance` activity create another activity, new task will be created to place that activity.

## Exclude an activity from back-stack history


## Android Activity LifeCycle Explained
Assume an application with a MainActivity which can call the Next Activity using a button click.

    public class MainActivity extends AppCompatActivity {
    
        private final String LOG_TAG = MainActivity.class.getSimpleName();
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
            Log.d(LOG_TAG, "calling onCreate from MainActivity");
        }
        @Override
        protected void onStart() {
            super.onStart();
            Log.d(LOG_TAG, "calling onStart from MainActivity");
        }
        @Override
        protected void onResume() {
            super.onResume();
            Log.d(LOG_TAG, "calling onResume  from MainActivity");
        }
    
        @Override
        protected void onPause() {
            super.onPause();
            Log.d(LOG_TAG, "calling onPause  from MainActivity");
        }
    
        @Override
        protected void onStop() {
            super.onStop();
            Log.d(LOG_TAG, "calling onStop  from MainActivity");
        }
    
        @Override
        protected void onDestroy() {
            super.onDestroy();
            Log.d(LOG_TAG, "calling onDestroy  from MainActivity");
        }
    
        @Override
        protected void onRestart() {
            super.onRestart();
            Log.d(LOG_TAG, "calling onRestart  from MainActivity");
        }
        public void toNextActivity(){
            Log.d(LOG_TAG, "calling Next Activity");
            Intent intent = new Intent(this, NextActivity.class);
            startActivity(intent);
        } }

and

    public class NextActivity extends AppCompatActivity {
        private final String LOG_TAG = NextActivity.class.getSimpleName();
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_next);
            Log.d(LOG_TAG, "calling onCreate from Next Activity");
        }
        @Override
        protected void onStart() {
            super.onStart();
            Log.d(LOG_TAG, "calling onStart from Next Activity");
        }
        @Override
        protected void onResume() {
            super.onResume();
            Log.d(LOG_TAG, "calling onResume  from Next Activity");
        }
    
        @Override
        protected void onPause() {
            super.onPause();
            Log.d(LOG_TAG, "calling onPause  from Next Activity");
        }
    
        @Override
        protected void onStop() {
            super.onStop();
            Log.d(LOG_TAG, "calling onStop  from Next Activity");
        }
    
        @Override
        protected void onDestroy() {
            super.onDestroy();
            Log.d(LOG_TAG, "calling onDestroy  from Next Activity");
        }
    
        @Override
        protected void onRestart() {
            super.onRestart();
            Log.d(LOG_TAG, "calling onRestart  from Next Activity");
        } }

*When app is first created*  
D/MainActivity: calling onCreate from MainActivity  
D/MainActivity: calling onStart from MainActivity  
D/MainActivity: calling onResume  from MainActivity  
 are called

*When screen sleeps*  
08:11:03.142 D/MainActivity: calling onPause  from MainActivity  
08:11:03.192 D/MainActivity: calling onStop  from MainActivity  
are called. And again when it wakes up  
08:11:55.922 D/MainActivity: calling onRestart  from MainActivity  
08:11:55.962 D/MainActivity: calling onStart from MainActivity  
08:11:55.962 D/MainActivity: calling onResume  from MainActivity  
are called

***Case1:***
When Next Activity is called from Main Activity  
D/MainActivity: calling Next Activity  
D/MainActivity: calling onPause  from MainActivity  
D/NextActivity: calling onCreate from Next Activity  
D/NextActivity: calling onStart from Next Activity  
D/NextActivity: calling onResume  from Next Activity  
D/MainActivity: calling onStop  from MainActivity  

When Returning back to the Main Activity from Next Activity using back button  
D/NextActivity: calling onPause  from Next Activity  
D/MainActivity: calling onRestart  from MainActivity  
D/MainActivity: calling onStart from MainActivity  
D/MainActivity: calling onResume  from MainActivity  
D/NextActivity: calling onStop  from Next Activity  
D/NextActivity: calling onDestroy  from Next Activity  

***Case2:***
When Activity is partially obscured (When overview button is pressed) or When app goes to background and another app completely obscures it  
D/MainActivity: calling onPause  from MainActivity  
D/MainActivity: calling onStop  from MainActivity  
and when the app is back in the foreground ready to accept User inputs,   
D/MainActivity: calling onRestart  from MainActivity  
D/MainActivity: calling onStart from MainActivity  
D/MainActivity: calling onResume  from MainActivity  
are called

***Case3:***
When an activity is called to fulfill implicit intent and user has make a selection. For eg., when share button is pressed and user has to select an app from the list of applications shown  
D/MainActivity: calling onPause  from MainActivity  

The activity is visible but not active now. When the selection is done and app is active  
D/MainActivity: calling onResume  from MainActivity  
is called

***Case4:***  
When the app is killed in the background(to free resources for another foreground app), *onPause*(for pre-honeycomb device) or *onStop*(for since honeycomb device) will be the last to be called before the app is terminated.

onCreate and onDestroy will be called utmost once each time the application is run. But the onPause, onStop, onRestart, onStart, onResume maybe called many times during the lifecycle.

## End Application with exclude from Recents
First define an ExitActivity in the AndroidManifest.xml

    <activity
            android:name="com.your_example_app.activities.ExitActivity"
            android:autoRemoveFromRecents="true"
            android:theme="@android:style/Theme.NoDisplay" />

Afterwards the ExitActivity-class

    /**
     * Activity to exit Application without staying in the stack of last opened applications
     */
    public class ExitActivity extends Activity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            if (Utils.hasLollipop()) {
                finishAndRemoveTask();
            } else if (Utils.hasJellyBean()) {
                finishAffinity();
            } else {
                finish();
            }
        }

       /**
        * Exit Application and Exclude from Recents
        *
        * @param context Context to use
        */
        public static void exitApplication(ApplicationContext context) {
            Intent intent = new Intent(context, ExitActivity.class);
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NO_ANIMATION | Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
            context.startActivity(intent);
        }
    }

## Presenting UI with setContentView
Activity class takes care of creating a window for you in which you can place your UI with `setContentView`.<br/>
There are three `setContentView` methods:

 - `setContentView(int layoutResID)` - Set the activity content from a layout resource. 
 - `setContentView(View view)` - Set the activity content to an explicit view.
 - `setContentView(View view, ViewGroup.LayoutParams params)` - Set the activity content to an explicit view with provided params.

When `setContentView` is called, this view is placed directly into the activity's view hierarchy. It can itself be a complex view hierarchy.

<br/>
<h1>Examples</h1>
<h2>Set content from resource file:</h2>
Add resource file (main.xml in this example) with view hierarchy:

    <?xml version="1.0" encoding="utf-8"?>
    <FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
        android:layout_width="match_parent"
        android:layout_height="match_parent" >

        <TextView android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:text="Hello" /> 

    </FrameLayout>

Set it as content in activity:

    public final class MainActivity extends Activity {

        @Override
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);

            // The resource will be inflated, 
            // adding all top-level views to the activity.
            setContentView(R.layout.main);
        }
    }

<h2>Set content to an explicit view:</h2>

    public final class MainActivity extends Activity {
    
        @Override
        public void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
    
            // Creating view with container
            final FrameLayout root = new FrameLayout(this);
            final TextView text = new TextView(this);
            text.setText("Hello");
            root.addView(text);

            // Set container as content view
            setContentView(root);
        }
    }



## Clear your current Activity stack and launch a new Activity
If you want to clear your current Activity stack and launch a new Activity (for example, logging out of the app and launching a log in Activity), there appears to be two approaches.

 **1. Target (API >= 16)** 

Calling `finishAffinity()` from an Activity

 **2. Target (11 <= API < 16)**

    Intent intent = new Intent(this, LoginActivity.class);
    intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK |Intent.FLAG_ACTIVITY_CLEAR_TOP);
    startActivity(intent);
    finish();



## Up Navigation for Activities


