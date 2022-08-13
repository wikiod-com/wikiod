---
title: "Exceptions"
slug: "exceptions"
draft: false
images: []
weight: 9943
type: docs
toc: true
---

## ActivityNotFoundException
This is a very common `Exception`. It causes your application to stop during the start or execution of your app. In the `LogCat` you see the message: 

    android.content.ActivityNotFoundException : Unable to find explicit activity class; 
    have you declared this activity in your AndroidManifest.xml?

In this case, check if you have declared your activity in the `AndroidManifest.xml` file.

The simplest way to declare your `Activity` in `AndroidManifest.xml` is:

    <activity  android:name="com.yourdomain.YourStoppedActivity" />           



## NetworkOnMainThreadException
From [the documentation][1]:

> The exception that is thrown when an application attempts to perform a networking operation on its main thread.
> 
> This is only thrown for applications targeting the Honeycomb SDK or higher. Applications targeting earlier SDK versions are allowed to do networking on their main event loop threads, but it's heavily discouraged.

Here's an example of a code fragment that may cause that exception:

    public class MainActivity extends AppCompatActivity {
    
        @Override
        protected void onCreate(Bundle savedInstanceState) {
            super.onCreate(savedInstanceState);
            setContentView(R.layout.activity_main);
    
            Uri.Builder builder = new Uri.Builder().scheme("http").authority("www.google.com");
            HttpURLConnection urlConnection = null;
            BufferedReader reader = null;
            URL url;
            try {
                url = new URL(builder.build().toString());
                urlConnection = (HttpURLConnection) url.openConnection();
                urlConnection.setRequestMethod("GET");
                urlConnection.connect();
            } catch (IOException e) {
                Log.e("TAG","Connection error", e);
            } finally{
                if (urlConnection != null) {
                    urlConnection.disconnect();
                }
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (final IOException e) {
                        Log.e("TAG", "Error closing stream", e);
                    }
                }
            }
        }
    }  

Above code will throw `NetworkOnMainThreadException` for applications targeting Honeycomb SDK (Android v3.0) or higher as the application is trying to perform a network operation on the main thread.

To avoid this exception, your network operations must always run in a background task via an `AsyncTask`, `Thread`, `IntentService`, etc.

    private class MyAsyncTask extends AsyncTask<String, Integer, Void> {

        @Override
        protected Void doInBackground(String[] params) {
            Uri.Builder builder = new Uri.Builder().scheme("http").authority("www.google.com");
            HttpURLConnection urlConnection = null;
            BufferedReader reader = null;
            URL url;
            try {
                url = new URL(builder.build().toString());
                urlConnection = (HttpURLConnection) url.openConnection();
                urlConnection.setRequestMethod("GET");
                urlConnection.connect();
            } catch (IOException e) {
                Log.e("TAG","Connection error", e);
            } finally{
                if (urlConnection != null) {
                    urlConnection.disconnect();
                }
                if (reader != null) {
                    try {
                        reader.close();
                    } catch (final IOException e) {
                        Log.e("TAG", "Error closing stream", e);
                    }
                }
            }

            return null;
        }
    } 

[1]: https://developer.android.com/reference/android/os/NetworkOnMainThreadException.html

## OutOfMemoryError
This is a runtime error that happens when you request a large amount of memory on the heap. This is common when loading a Bitmap into an ImageView.

You have some options:

1. Use a large application heap

Add the "largeHeap" option to the application tag in your AndroidManifest.xml. This will make more memory available to your app but will likely not fix the root issue.
    
    <application largeHeap="true" ... >

2. Recycle your bitmaps

After loading a bitmap, be sure to recycle it and free up memory:

        if (bitmap != null && !bitmap.isRecycled())
           bitmap.recycle();

3. Load sampled bitmaps into memory

Avoid loading the entire bitmap into memory at once by sampling a reduced size, using BitmapOptions and inSampleSize.

See [Android documentation][1] for example


  [1]: https://developer.android.com/training/displaying-bitmaps/load-bitmap.html#load-bitmap

## DexException


## UncaughtException
If you want to handle uncaught exceptions try to catch them all in onCreate method of you Application class:

    public class MyApp extends Application {
        @Override
        public void onCreate() {
            super.onCreate();
            try {
                Thread
                    .setDefaultUncaughtExceptionHandler(
                                new Thread.UncaughtExceptionHandler() {
    
                    @Override
                    public void uncaughtException(Thread thread, Throwable e) {
                        Log.e(TAG, 
                                "Uncaught Exception thread: "+thread.getName()+"
                                 "+e.getStackTrace());
                        handleUncaughtException (thread, e);
                    }
                });
            } catch (SecurityException e) {
                Log.e(TAG, 
                        "Could not set the Default Uncaught Exception Handler:"
                        +e.getStackTrace());
            }
        }
    
        private void handleUncaughtException (Thread thread, Throwable e){
            Log.e(TAG, "uncaughtException:");
            e.printStackTrace();
        }
    }

## Registering own Handler for unexpected exceptions
This is how you can react to exceptions which have not been catched, similar to the system's standard "Application XYZ has crashed"

```
import android.app.Application;
import android.util.Log;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Application class writing unexpected exceptions to a crash file before crashing.
 */
public class MyApplication extends Application {
    private static final String TAG = "ExceptionHandler";

    @Override
    public void onCreate() {
        super.onCreate();

        // Setup handler for uncaught exceptions.
        final Thread.UncaughtExceptionHandler defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
        Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(Thread thread, Throwable e) {
                try {
                    handleUncaughtException(e);
                    System.exit(1);
                } catch (Throwable e2) {
                    Log.e(TAG, "Exception in custom exception handler", e2);
                    defaultHandler.uncaughtException(thread, e);
                }
            }
        });
    }

    private void handleUncaughtException(Throwable e) throws IOException {
        Log.e(TAG, "Uncaught exception logged to local file", e);

        // Create a new unique file
        final DateFormat dateFormat =  new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss", Locale.US);
        String timestamp;
        File file = null;
        while (file == null || file.exists()) {
            timestamp = dateFormat.format(new Date());
            file = new File(getFilesDir(), "crashLog_" + timestamp + ".txt");
        }
        Log.i(TAG, "Trying to create log file " + file.getPath());
        file.createNewFile();

        // Write the stacktrace to the file
        FileWriter writer = null;
        try {
            writer = new FileWriter(file, true);
            for (StackTraceElement element : e.getStackTrace()) {
                writer.write(element.toString());
            }
        } finally {
            if (writer != null) writer.close();
        }

        // You can (and probably should) also display a dialog to notify the user
    }
}
```
Then register this Application class in your AndroidManifest.xml:
```
<application android:name="de.ioxp.arkmobile.MyApplication" >
```

