---
title: "Looper"
slug: "looper"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

A [`Looper`][1] is an Android class used to run a message loop for a thread, which usually do not have one associated with them.

The most common `Looper` in Android is the main-loop, also commonly known as the main-thread. This instance is unique for an application and can be accessed statically with `Looper.getMainLooper()`.

If a `Looper` is associated with the current thread, it can be retrieved with `Looper.myLooper()`.


 [1]: https://developer.android.com/reference/android/os/Looper.html

## Create a simple LooperThread
A typical example of the implementation of a `Looper` thread given by the official documentation uses `Looper.prepare()` and `Looper.loop()` and associates a `Handler` with the loop between these calls.

    class LooperThread extends Thread {
        public Handler mHandler;

        public void run() {
            Looper.prepare();

            mHandler = new Handler() {
                public void handleMessage(Message msg) {
                    // process incoming messages here
                }
            };

            Looper.loop();
        }
    }



## Run a loop with a HandlerThread
A [`HandlerThread`][1] can be used to start a thread with a `Looper`. This looper then can be used to create a `Handler` for communications with it.

    HandlerThread thread = new HandlerThread("thread-name");
    thread.start();
    Handler handler = new Handler(thread.getLooper());


  [1]: https://developer.android.com/reference/android/os/HandlerThread.html

