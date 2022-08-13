---
title: "Multithreading"
slug: "multithreading"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

MFC supports worker threads and gui threads (threads with message loops). See https://msdn.microsoft.com/en-us/library/975t8ks0.aspx for more documentation.

## Simple AfxBeginThread Worker Thread Example
This example shows a call of AfxBeginThread that starts the worker thread and an example worker thread procedure for that thread.

    // example simple thread procedure.
    UINT __cdecl threadProc(LPVOID rawInput)
    {
        // convert it to the correct data type. It's common to pass entire structures this way.
        int* input = (int*)rawInput;
        // TODO: Add your worker code...
        MessageBox(0,"Inside thread!",0,0);
        // avoid memory leak.
        delete input;
        return 0;
    }
    // ...
    // somewhere that gets called when you want to start the thread...
    int* input = new int;
    *input = 9001;
    AfxBeginThread(threadProc, input);
    // after this, the message box should appear, and the rest of your code should continue 
    // running.
    

