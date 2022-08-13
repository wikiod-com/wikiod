---
title: "Input Field"
slug: "input-field"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

To get a user's input and store it in a variable, you can use the InputBox command. The script will not continue executing commands until the user either presses 'OK' or 'Cancel'.

'OK' will close the window and save the user's input
'Cancel' will close the window, discarding the user's input

## Parameters
| InputBox, OutputVar [, Title, Prompt, HIDE, Width, Height, X, Y, Timeout, Default] | What each Option Means |
| ------ | ------ |
| OutputVar   | The variable the user's input will be saved to   |
| Title   | The name of the input box   |
| Prompt   | Text inside of the input box   |
| HIDE   | Displays the user's input as asterisks to hide the input - type HIDE to enable   |
| Width   | The width of the input box   |
| Height   | The height of the input box   |
| X   | The amount of pixels from the left edge of the screen that the top-left corner of the input box will be   |
| Y   | The amount of pixels from the top edge of the screen that the top-left corner of the input box will be   |
| Timeout   | Automatically closes the input box and saves the user's input after this time in miliseconds   |
| Default   | The text that will appear in the input box's editable field when it is opened   |

An input box is a GUI item, so it will be treated as a GUI item.

A list of errorlevels for this command:

| Errorlevel | What it Means |
| ------ | ------ |
| 0   | The user pressed the 'OK' button  |
| 1   | The user pressed the 'Cancel' button   |
| 2   | The input box timed out   |

You can find the page for this command on the AutoHotkey documentation here: 
https://autohotkey.com/docs/commands/InputBox.htm

## Basic Usage Example
    InputBox, userinput
This will store what the user types into the input box in the variable named *userinput*

## Passwords
    InputBox, password, Enter your Password,, HIDE,, 100

    Loop, {
      if (errorlevel = 1)
    return

      if (password = "password") {
    MsgBox, The password is correct.
        return
      } else if (password != "password") {
    MsgBox, The password is incorrect.
    InputBox, password, Enter your Password,, HIDE,, 100
      }
    }
    
This will check if the user has typed "password" in the input box. If the user types the correct value, it will say "The password is correct." and close the input box. If the user types the incorrect value, it will say "The password is incorrect." and reopen the input box. If the errorlevel is 1 (the user pressed cancel), it will terminate the script.

