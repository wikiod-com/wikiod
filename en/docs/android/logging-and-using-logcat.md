---
title: "Logging and using Logcat"
slug: "logging-and-using-logcat"
draft: false
images: []
weight: 9732
type: docs
toc: true
---

## Syntax
- `Log.v(String tag, String msg, Throwable tr)`
- `Log.v(String tag, String msg)`
- `Log.d(String tag, String msg, Throwable tr)`
- `Log.d(String tag, String msg)`
- `Log.i(String tag, String msg, Throwable tr)`
- `Log.i(String tag, String msg)`
- `Log.w(String tag, String msg, Throwable tr)`
- `Log.w(String tag, String msg)`
- `Log.e(String tag, String msg, Throwable tr)`
- `Log.e(String tag, String msg)`



## Parameters
| Option | Description|
| ------ | ------ |
| -b (buffer) | Loads an alternate log buffer for viewing, such as events or radio. The main buffer is used by default. See Viewing Alternative Log Buffers. |
| -c | Clears (flushes) the entire log and exits. |
| -d | Dumps the log to the screen and exits. |
| -f (filename) | Writes log message output to (filename). The default is stdout. |
| -g | Prints the size of the specified log buffer and exits. |
| -n (count) | Sets the maximum number of rotated logs to (count). The default value is 4. Requires the -r option. |
| -r (kbytes) | Rotates the log file every (kbytes) of output. The default value is 16. Requires the -f option. |
| -s | Sets the default filter spec to silent. |
| -v (format) | Sets the output format for log messages. The default is brief format. |

# Definition

Logcat is a command-line tool that dumps a log of system messages, including stack traces when the device throws an error and messages that you have written from your app with the [Log][1] class.

# When to use

If you are considering using Java's System.out methods for printing to the console instead of using one of Android's Log methods, then you should know that they basically work the same. However, it is better to avoid using Java's methods because the extra information and formatting provided by Android's Log methods are more beneficial. Also, the System.out print methods are [redirected][3] to the `Log.i()` method.

# Useful links

- Android developer official documentation for [Log][1] and [logcat][2].
- Stackoveflow Question : http://stackoverflow.com/questions/7959263/android-log-v-log-d-log-i-log-w-log-e-when-to-use-each-one


  [1]: https://developer.android.com/reference/android/util/Log.html
  [2]: https://developer.android.com/studio/command-line/logcat.html#startingLogcat
  [3]: http://stackoverflow.com/a/2220559/1684720

## Filtering the logcat output
It is helpful to filter the logcat output because there are many messages which are not of interest. To filter the output, open the "Android Monitor" and click on the drop down on the top-right and select *Edit Filter Configuration*

[![Android Monitor][1]][1]

Now you can add custom filters to show messages which are of interest, as well as filter out well-known log lines which can safely be ignored. To ignore a part of the output you may define a [Regular Expression][2]. Here is an example of excluding matching tags:

    ^(?!(HideMe|AndThis))

This can be entered by following this example:

[![Create new logcat filter][3]][3]

The above is a regular expression which excludes inputs. If you wanted to add another tag to the *blacklist*, add it after a pipe `|` character. For example, if you wanted to blacklist "GC", you would use a filter like this:

    ^(?!(HideMe|AndThis|GC))


---
For more documentation and examples visit [Logging and using Logcat][4]



  [1]: http://i.stack.imgur.com/XxB3K.png
  [2]: https://www.wikiod.com/regex
  [3]: http://i.stack.imgur.com/dyzcQ.png
  [4]: https://www.wikiod.com/android/logging-and-using-logcat

## Logging
Any quality Android application will keep track of what it's doing through application logs.
These logs allow easy debugging help for the developer to diagnose what's going on with the application.
Full Android Documentation can be found [here][1], but a summary follows:
---

<h2>Basic Logging</h2>
The `Log` class is the main source of writing developer logs, by specifying a `tag` and a `message`. The tag is what you can use to filter log messages by to identify which lines come from your particular Activity. Simply call

    Log.v(String tag, String msg);
And the Android system will write a message to the logcat:

    07-28 12:00:00.759 24812-24839/my.packagename V/MyAnimator: Some log messages
     └ time stamp             |  app.package┘     |    └ any tag  |
         process & thread ids ┘          log level┘               └ the log message
     
> TIP: <br>Notice the process id and the thread id. If they are the same - the log is coming from the main/UI thread!

Any tag can be used, but it is common to use the class name as a tag:

    public static final String tag = MyAnimator.class.getSimpleName();


---
<h2>Log Levels</h2>
The Android logger has 6 different levels, each of which serve a certain purpose:
 - `ERROR`: `Log.e()`
    - Used to indicate critical failure, this is the level printed at when throwing an `Exception`.
 - `WARN`: `Log.w()`
    - Used to indicate a warning, mainly for recoverable failures
 - `INFO`: `Log.i()`
    - Used to indicate higher-level information about the state of the application
 - `DEBUG`: `Log.d()`
    - Used to log information that would be useful to know when debugging the application, but would get in the way when running the application
 - `VERBOSE`: `Log.v()`
    - Used to log information that reflects the small details about the state of the application
 - `ASSERT`: `Log.wtf()`
    - Used to log information about a condition that should never happen.
    - *wtf* stands for "What a Terrible Failure".
---
<h2>Motivation For Logging</h2>
The motivation for logging is to easily find errors, warnings, and other information by glancing at the chain of events from the application. For instance, imagine an application that reads lines from a text file, but incorrectly assumes that the file will never be empty. The log trace (of an app that doesn't log) would look something like this:

    E/MyApplication: Process: com.example.myapplication, PID: 25788
                              com.example.SomeRandomException: Expected string, got 'null' instead
Followed by a bunch of stack traces that would eventually lead to the offending line, where stepping through with a debugger would eventually lead to the problem

However, the log trace of an application with logging enabled could look something like this:

    V/MyApplication: Looking for file myFile.txt on the SD card
    D/MyApplication: Found file myFile.txt at path <path>
    V/MyApplication: Opening file myFile.txt
    D/MyApplication: Finished reading myFile.txt, found 0 lines
    V/MyApplication: Closing file myFile.txt
    ...
    E/MyApplication: Process: com.example.myapplication, PID: 25788
                              com.example.SomeRandomException: Expected string, got 'null' instead

A quick glance at the logs and it is obvious that the file was empty.

----
<h2>Things To Considering When Logging:</h2>
Although logging is a powerful tool that allows Android developers to gain a greater insight into the inner working of their application, logging does have some drawbacks.

<h2> Log Readability: </h2>
It is common for Android Applications to have several logs running synchronously. As such, it is very important that each log is easily readable and only contains relevant, necessary information.

<h2>Performance:</h2>
Logging does require a small amount of system resources. In general, this does not warrant concern, however, if overused, logging may have a negative impact on application performance. 

<h2>Security:</h2>
Recently, several Android Applications have been added to the Google Play marketplace that allow the user to view logs of all running applications. This unintended display of data may allow users to view confidential information. As a rule of thumb, always remove logs that contain on non-public data *before* publishing your application to the marketplace.

----
<h2>Conclusion:</h2>
Logging is an essential part of an Android application, because of the power it gives to developers. The ability to create a useful log trace is one of the most challenging aspects of software development, but Android's Log class helps to make it much easier. 

---
For more documentation and examples visit [Logging and using Logcat][2]





[1]:https://developer.android.com/reference/android/util/Log.html
[2]: https://www.wikiod.com/android/logging-and-using-logcat

## Using the Logcat
Logcat is a command-line tool that dumps a log of system messages, including stack traces when the device throws an error and messages that you have written from your app with the Log class.

The Logcat output can be displayed within Android Studio's Android Monitor or with adb command line.

**In Android Studio**

 Show by clicking the "Android Monitor" icon:
[![enter image description here][1]][1]
 Or by pressing <kbd>Alt</kbd>+<kbd>6</kbd> on Windows/Linux or <kbd>CMD</kbd>+<kbd>6</kbd> on Mac.

**via command line:**

Simple usage:

    $ adb logcat

With timestamps:

    $ adb logcat -v time

Filter on specific text:

    $ adb logcat -v time | grep 'searchtext'



There are many options and filters available to *command line logcat*, documented [here][2]. <br>
A simple but useful example is the following filter expression that displays all log messages with priority level "error", on all tags:

    $ adb logcat *:E


  [1]: http://i.stack.imgur.com/rKiCw.png
  [2]: https://developer.android.com/studio/command-line/logcat.html

## Log with link to source directly from Logcat
This is a nice trick to add a link to code, so it will be easy to jump to the code that issued the log.

With the following code, this call:

    MyLogger.logWithLink("MyTag","param="+param);

<br/>Will result in:

    07-26...012/com.myapp D/MyTag: MyFrag:onStart(param=3)  (MyFrag.java:2366) // << logcat converts this to a link to source!

This is the code (inside a class called MyLogger):

    static StringBuilder sb0 = new StringBuilder(); // reusable string object

    public static void logWithLink(String TAG, Object param) {
        StackTraceElement stack = Thread.currentThread().getStackTrace()[3];
        sb0.setLength(0);
        String c = stack.getFileName().substring(0, stack.getFileName().length() - 5); // removes the ".java"
        sb0.append(c).append(":");
        sb0.append(stack.getMethodName()).append('(');
        if (param != null) {
            sb0.append(param);
        }
        sb0.append(") ");
        sb0.append(" (").append(stack.getFileName()).append(':').append(stack.getLineNumber()).append(')');
        Log.d(TAG, sb0.toString());
    }

This is a basic example, it can be easily extended to issue a link to the caller (hint: the stack will be [4] instead of [3]), and you can also add other relevant information.


## Generating Logging code
`Android Studio`'s `Live templates` can offer quite a few shortcuts for quick logging.<br>
To use Live templates, all you need to do is to start typing the template name, and hit `TAB` or enter to insert the statement.

Examples:
- `logi` → turns into → `android.util.Log.i(TAG, "$METHOD_NAME$: $content$");`
     - `$METHOD_NAME$` will automatically be replaced with your method name, and the cursor will wait for the content to be filled.
- `loge` → same, for error
- etc. for the rest of the logging levels.

Full list of templates can be found in `Android Studio`'s settings (<kbd>ALT</kbd>+<kbd>s</kbd> and type "live"). And it is possible to add your custom templates as well.



If you find `Android Studio`'s `Live templates` not enough for your needs, 
you can consider [Android Postfix Plugin][1]

This is a very useful library which helps you to avoid writing the logging  line manually.

The syntax is absolutely simple:

**.log**  -  Logging. If there is constant variable "TAG", it use "TAG" . Else it use class name.

[![enter image description here][2]][2]


  [1]: https://github.com/takahirom/android-postfix-plugin
  [2]: http://i.stack.imgur.com/mE7vX.gif

## Android Studio usage
 1. Hide/show printed information:
[![Screenshot][1]][1]

 2. Control verbosity of the logging:
[![Log verbosity screenshot][2]][2]

 3. Disable/enable opening log window when starting run/debug application
[![Disable opening log screenshot][3]][3]


  [1]: http://i.stack.imgur.com/Q4b4G.jpg
  [2]: http://i.stack.imgur.com/TDyHa.jpg
  [3]: http://i.stack.imgur.com/ief7b.jpg

## Clear logs


