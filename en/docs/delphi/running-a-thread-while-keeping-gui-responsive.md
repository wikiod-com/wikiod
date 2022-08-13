---
title: "Running a thread while keeping GUI responsive"
slug: "running-a-thread-while-keeping-gui-responsive"
draft: false
images: []
weight: 9821
type: docs
toc: true
---

## Responsive GUI using threads for background work and PostMessage to report back from the threads
Keeping a GUI responsive while running a lengthy process requires either some very elaborate "callbacks" to allow the GUI to process its message queue, or the use of (background) (worker) threads.

Kicking off any number of threads to do some work usually isn't a problem. The fun starts when you want to make the GUI show intermediate and final results or report on the progress.

Showing anything in the GUI requires interacting with controls and/or the message queue/pump. That should always be done in the context of the main thread. Never in the context of any other thread.

There are many ways to handle this.

This example shows how you can do it using simple threads, allowing the GUI to access the thread instance after it is finished by setting `FreeOnTerminate` to `false`, and reporting when a thread is "done" using `PostMessage`.

Notes on race conditions:
References to the worker threads are kept in an array in the form. When a thread is finished, the corresponding reference in the array gets nil-ed.

This is a potential source of race conditions. As is the use of a "Running" boolean to make it easier to determine whether there are still any threads that need to finish.

You will need to decide whether you need to protect theses resource using locks or not.

In this example, as it stands, there is no need. They are only modified in two locations: the `StartThreads` method and the `HandleThreadResults` method. Both methods only ever run in the context of the main thread. As long as you keep it that way and don't start calling these methods from the context of different threads, there is no way for them to produce race conditions.

Thread
------

    type
      TWorker = class(TThread)
      private
        FFactor: Double;
        FResult: Double;
        FReportTo: THandle;
      protected
        procedure Execute; override;
      public
        constructor Create(const aFactor: Double; const aReportTo: THandle);
    
        property Factor: Double read FFactor;
        property Result: Double read FResult;
      end;

The constructor just sets the private members and sets FreeOnTerminate to False. This is essential as it will allow the main thread to query the thread instance for its result. 

The execute method does its calculation and then posts a message to the handle it received in its constructor to say its done:

    procedure TWorker.Execute;
    const
      Max = 100000000;var
      i : Integer;
    begin
      inherited;
    
      FResult := FFactor;
      for i := 1 to Max do
        FResult := Sqrt(FResult);
    
      PostMessage(FReportTo, UM_WORKERDONE, Self.Handle, 0);
    end;

The use of `PostMessage` is essential in this example. `PostMessage` "just" puts a message on the queue of the main thread's message pump and doesn't wait for it to be handled. It is asynchronous in nature. If you were to use `SendMessage` you'd be coding yourself into a pickle. `SendMessage` puts the message on the queue and waits until it has been processed. In short, it is synchronous.

The declarations for the custom UM_WORKERDONE message are declared as:

    const
      UM_WORKERDONE = WM_APP + 1;
    type
      TUMWorkerDone = packed record
        Msg: Cardinal;
        ThreadHandle: Integer;
        unused: Integer;
        Result: LRESULT;
      end;

The `UM_WORKERDONE` const uses `WM_APP` as a starting point for its value to ensure that it doesn't interfere with any values used by Windows or the Delphi VCL (as [recommended][1] by MicroSoft).


Form
----

Any form can be used to start threads. All you need to do is add the following members to it:

    private
      FRunning: Boolean;
      FThreads: array of record
        Instance: TThread;
        Handle: THandle;
      end;
      procedure StartThreads(const aNumber: Integer);
      procedure HandleThreadResult(var Message: TUMWorkerDone); message UM_WORKERDONE;

Oh, and the example code assumes the existence of a `Memo1: TMemo;` in the form's declarations, which it uses for "logging and reporting".

The `FRunning` can be used to prevent the GUI from starting being clicked while the work is going on. `FThreads` is used to hold the instance pointer and the handle of the created threads.

The procedure to start the threads has a pretty straightforward implementation. It starts with a check whether there already is a set of threads being waited on. If so, it just exits. If not, it sets the flag to true and starts the threads providing each with its own handle so they know where to post their "done" message.

    procedure TForm1.StartThreads(const aNumber: Integer);
    var
      i: Integer;
    begin
      if FRunning then
        Exit;
        
      FRunning := True;
    
      Memo1.Lines.Add(Format('Starting %d worker threads', [aNumber]));
      SetLength(FThreads, aNumber);
      for i := 0 to aNumber - 1 do
      begin
        FThreads[i].Instance := TWorker.Create(pi * (i+1), Self.Handle);
        FThreads[i].Handle := FThreads[i].Instance.Handle;
      end;
    end;

The thread's handle is also put in the array because that is what we receive in the messages that tell us a thread is done and having it outside the thread's instance makes it slightly easier to access. Having the handle available outside the thread's instance also allows us to use `FreeOnTerminate` set to `True` if we didn't need the instance to get its results (for example if they had been stored in a database). In that case there would of course be no need to keep a reference to the instance.
   
The fun is in the HandleThreadResult implementation:

    procedure TForm1.HandleThreadResult(var Message: TUMWorkerDone);
    var
      i: Integer;
      ThreadIdx: Integer;
      Thread: TWorker;
      Done: Boolean;
    begin
      // Find thread in array
      ThreadIdx := -1;
      for i := Low(FThreads) to High(FThreads) do
        if FThreads[i].Handle = Cardinal(Message.ThreadHandle) then
        begin
          ThreadIdx := i;
          Break;
        end;
    
      // Report results and free the thread, nilling its pointer and handle 
      // so we can detect when all threads are done.
      if ThreadIdx > -1 then
      begin
        Thread := TWorker(FThreads[i].Instance);
        Memo1.Lines.Add(Format('Thread %d returned %f', [ThreadIdx, Thread.Result]));
        FreeAndNil(FThreads[i].Instance);
        FThreads[i].Handle := nil;
      end;
    
      // See whether all threads have finished.
      Done := True;
      for i := Low(FThreads) to High(FThreads) do
        if Assigned(FThreads[i].Instance) then
        begin
          Done := False;
          Break;
        end;
      if Done then
      begin
        Memo1.Lines.Add('Work done');
        FRunning := False;
      end;
    end;

This method first looks up the thread using the handle received in the message. If a match was found, it retrieves and reports the thread's result using the instance (`FreeOnTerminate` was `False`, remember?), and then finishes up: freeing the instance and setting both the instance reference and the handle to nil, indicating this thread is no longer relevant.

Finally it checks to see if any of the threads is still running. If none is found, "all done" is reported and the `FRunning` flag set to `False` so a new batch of work can be started.


  [1]: https://msdn.microsoft.com/ru-ru/library/windows/desktop/ms644930(v=vs.85).aspx "recommended"

