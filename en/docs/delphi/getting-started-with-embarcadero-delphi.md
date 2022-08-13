---
title: "Getting started with Embarcadero Delphi"
slug: "getting-started-with-embarcadero-delphi"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
This program, saved to a file named *HelloWorld.dpr*, compiles to a console application that prints "Hello World" to the console:

    program HelloWorld;

    {$APPTYPE CONSOLE}

    begin
      WriteLn('Hello World');
    end.

## Show 'Hello World' using the VCL
This progam uses VCL, the default UI components library of Delphi, to print "Hello World" into a message box. The VCL wrapps most of the commonly used WinAPI components. This way, they can be used much easier, e.g. without the need to work with Window Handles.

To include a dependency (like `Vcl.Dialogs` in this case), add the `uses` block including a comma-separated list of units ending with an semicolon.
   
    program HelloWindows;

    uses
      Vcl.Dialogs;

    begin
      ShowMessage('Hello Windows');
    end.

## Cross-platform Hello World using FireMonkey
<!-- if version [gte XE2] -->

    program CrossPlatformHelloWorld;
    
    uses
      FMX.Dialogs;
    
    {$R *.res}
    
    begin
      ShowMessage('Hello world!');
    end.

Most of the Delphi supported platforms (Win32/Win64/OSX32/Android32/iOS32/iOS64) also support a console so the `WriteLn` example fits them well. 

For the platforms that require a GUI (any iOS device and some Android devices), the above FireMonkey example works well.
<!-- end version if -->

## Show 'Hello World' Using WinAPI MessageBox
This program uses the Windows API (WinAPI) to print "Hello World" into a message box.

To include a dependency (like `Windows` in this case), add the uses block including a comma-separated list of units ending with an semicolon.

    program HelloWorld;
    
    uses
      Windows;
    
    begin
      MessageBox(0, 'Hello World!', 'Hello World!', 0);
    end.

