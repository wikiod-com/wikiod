---
title: "Intent"
slug: "intent"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Start new Activity
This example explain how to use an Intent for start a new Activity.

You need two activities:

 1. CurrentActivity
 2. DestinationActivity

In CurrentActivity you have to created an Intent. For that you have to specify two arguments:

 1. Context: It's CurrentActivity, because Activity is a subclass of Context.
 2. DestinationActivity class

`Intent intent = new Intent(Context, DestinationActivity.class);`

Then, call `startActivity` passing the intent created.

`startActivity(intent);`

Now we have this source:


    Intent intent = new Intent(this, DestinationActivity.class);
    startActivity(intent);

For example, you can put it in a method an call it when an event ocurred.

    void nextActivity(){
         Intent intent = new Intent(this, DestinationActivity.class);
         startActivity(intent);
    }

    public class CurrentActivity extends AppCompatActivity {
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.current_activity);
            
            nextActivity();
            finish(); // Finish current activity, if you don't finished it, the current activity will be in background. You can finish it then.
        }
    }

## Use of Intent to Share text
On calling, an application chooser dialog will appear and by selecting an application you can share your content with it.

For calling use this line of code in your program class:

    share(context, "This is a test message", "Test Subject")

Function's definition:

    public static void share (Context context, String text, String subject, String title, String dialogHeaderText) {
        Intent intent = new Intent(android.content.Intent.ACTION_SEND);
        intent.setType("text/plain");
        intent.putExtra(android.content.Intent.EXTRA_SUBJECT, subject);
        intent.putExtra(android.content.Intent.EXTRA_TEXT, text);
        intent.putExtra(Intent.EXTRA_TITLE, title);
        context.startActivity(Intent.createChooser(intent, dialogHeaderText));
    }

See original post here: http://stackoverflow.com/a/35159850/3819836

