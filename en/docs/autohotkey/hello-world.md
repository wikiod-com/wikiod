---
title: "Hello World"
slug: "hello-world"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Hello World examples
## Show a "Hello World!" in message box.

    MsgBox, Hello World!

## Show "Hello World!" stored in variable MyString in message box.

    MyString := "Hello World!"
    MsgBox, %MyString%

## Show a "Hello World!" in tooltip.

    #Persistent
    Tooltip, Hello World!

## Show a "Hello World!" message in the traybar edit.
    #Persistent
    TrayTip,, Hello World!

## Show "Hello World!" in a GUI window.
    Gui, Add, Text,, Hello World!
    Gui, Add, Edit,, Hello World!
    Gui, Show
    return

    GuiClose() {
        ExitApp
    }

## Prints "Hello, World!" to Standard Output (stdout).

    FileAppend, % "Hello, World!", *

## Simulate typing "Hello, World!" when pressing enter

    Enter::Send, Hello World{!}

## Create a new file called HelloWorld.txt

    FileAppend,, HelloWorld.txt

## When typing the word "Hello" followed by a space or enter it will be replaced by "Hello World"

    ::Hello::Hello World


