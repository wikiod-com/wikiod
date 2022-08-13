---
title: "Using BackgroundWorker"
slug: "using-backgroundworker"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Basic implementation of Background worker class
You need to import System.ComponentModel for using background worker

    Imports System.ComponentModel

Then Declare a private variable

    Private bgWorker As New BackgroundWorker

You need to create two methods for background worker's DoWork and RunWorkerCompleted events and assign them. 

    Private Sub MyWorker_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs)
      'Add your codes here for the worker to execute
    
    End Sub

The below sub will be executed when the worker finishes the job

    Private Sub MyWorker_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs)
        'Add your codes for the worker to execute after finishing the work.
    
    End Sub

Then within your code add the below lines to start the background worker

>         bgWorker = New BackgroundWorker
>         AddHandler bgWorker.DoWork, AddressOf MyWorker_DoWork
>         AddHandler bgWorker.RunWorkerCompleted, AddressOf MyWorker_RunWorkerCompleted
>         bgWorker.RunWorkerAsync()

When you call RunWorkerAsync() function, MyWorker_DoWork will be executed.



