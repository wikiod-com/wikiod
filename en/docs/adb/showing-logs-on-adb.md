---
title: "Showing Logs on ADB"
slug: "showing-logs-on-adb"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Displaying and filtering with Logcat
Displaying all the logs from the default buffer on the Command Line can be accomplished by:

    adb logcat

This command will show you all the logs from the device's main buffer. Notice that if you use it for the first time, you'll get a lot of information, an enormous stream of data. So you may want to clear the logs first...

Cleaning the logs:

    adb logcat -c

*This will clean clear the logs, and start fresh.*

**Displaying Alternate Buffers**

There are two other buffers besides the main buffer that may be displayed as follows:

`adb logcat -b` *buffer_name*,

where *buffer_name* is one of the following:

 - `radio` - view the buffer that contains radio/telephony related messages.
 - `events` - view the buffer containing events-related messages.
 - `main` - view the main log buffer (default)

**Filtering Log Output**

Logcat logs got so called levels: 

> **V** — Verbose, **D** — Debug, **I** — Info, **W** — Warning, **E** — Error, **F** — Fatal, **S**
> — Silent

Those levels are specified when application uses those Log function:

    Log.v(); // Verbose
    Log.d(); // Debug
    Log.i(); // Info
    Log.w(); // Warning
    Log.e(); // Error

if your code Log call is:

    Log.i("MainActivtyTag", "Showing the very first fragment");

in logcat you'll see this output:

    07-27 11:34:21.027 I MainActivtyTag 66 : Showing the very first fragment

So, this is the log convention:

    <timestamp> <logLevel> <tag> <line> : <messge>

For instance, if you want to show all the logs that have Fatal (F) level:

    adb logcat *:F

\* *is a what called a wild card - stands for all package names*

**Filtering by application package name**

Since package names are guaranteed to be unique , you can filter logcat by your package name, of course you can combine it with the Level filter:

    adb logcat <package name>:<log level>


For exiting/interrupting process - press `Ctrl + X`






