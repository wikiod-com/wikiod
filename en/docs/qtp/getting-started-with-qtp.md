---
title: "Getting started with QTP"
slug: "getting-started-with-qtp"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Running the installer will set up the application along with any and all available add-ons to enable additional functionality (for example, the Web add-on is required in order to allow QTP to control browser objects, the Terminal add-on allows it to control terminal emulator applications, the Java application allows it to control Java applications and so on).

It also lets the user specify the License setup from two options - Single Seat or Concurrent.  

Single Seat is used where there is only a single licence for a single user.  

Multiple users/licenses can be set up as Concurrent, where a central license server is configured to manage the number of users utilising the system at the same time.  The number of users at any given time cannot exceed the number of available licenses.

## Hello World
QTP uses `VBScript` as its programming language.  Therefore a Hello World example can be given in pure VBScript:

    MsgBox "Hello World!"

Executing this script (by pressing F5 or clicking the Play button on the taskbar) will produce a message box with the text "Hello World!" in it.

