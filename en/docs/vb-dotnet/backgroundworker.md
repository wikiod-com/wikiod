---
title: "BackgroundWorker"
slug: "backgroundworker"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Accessing GUI components in BackgroundWorker
You cannot access any GUI components from the BackgroudWorker. For example if you try to do something like this

    Private Sub BackgroundWorker1_DoWork(sender As Object, e As DoWorkEventArgs)
        TextBox1.Text = "Done"
    End Sub

you will receive a runtime error saying that "Cross-thread operation not valid: Control 'TextBox1' accessed from a thread other than the thread it was created on."

This is because the BackgroundWorker runs your code on another thread in parallel with the main thread, and the GUI components are not thread-safe. You have to set your code to be run on the main thread using the `Invoke` method, giving it a delegate:

    Private Sub BackgroundWorker1_DoWork(sender As Object, e As DoWorkEventArgs)
        Me.Invoke(New MethodInvoker(Sub() Me.TextBox1.Text = "Done"))
    End Sub

Or you can use the ReportProgress method of the BackgroundWorker:

    Private Sub BackgroundWorker1_DoWork(sender As Object, e As DoWorkEventArgs)
        Me.BackgroundWorker1.ReportProgress(0, "Done")
    End Sub

    Private Sub BackgroundWorker1_ProgressChanged(sender As Object, e As ProgressChangedEventArgs)
        Me.TextBox1.Text = DirectCast(e.UserState, String)
    End Sub




## Using BackgroundWorker
Executing a task with the background worker.

Double Click on the `BackgroundWorker` control from the Toolbox

[![BackroundWorker Control in Toolbox][1]][1]

This is how the BackgroundWorker appears after adding it.

[![enter image description here][2]][2]

Double click on the added control to get the `BackgroundWorker1_DoWork` event and add the code to be executed when the BackgroundWorker is called. Something like this:

    Private Sub BackgroundWorker1_DoWork(ByVal sender As System.Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork
    
        'Do the time consuming background task here

    End Sub

Calling the BackgroundWorker to perform the task can be done at any event like `Button_Click`, `Textbox_TextChanged`, etc. as follows:

    BackgroundWorker1.RunWorkerAsync()

Modify the `RunWorkerCompleted` event to capture the task finished event of the BackgroundWorker as follows:

    Private Sub BackgroundWorker1_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorker1.RunWorkerCompleted
        MsgBox("Done")
    End Sub

This will display a message box saying `Done` when the worker finishes the task assigned to it.




  [1]: http://i.stack.imgur.com/QuRSr.jpg
  [2]: http://i.stack.imgur.com/KmxOU.jpg

