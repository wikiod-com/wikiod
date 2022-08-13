---
title: "Built-in Variables and Functions"
slug: "built-in-variables-and-functions"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

AutoHotkey comes with many built-in functions and variables which can be used anywhere inside a script.  
For a full list including explanations, see:

 - [List of built-in variables](https://autohotkey.com/docs/Variables.htm#BuiltIn)
 - [List of built-in functions](https://autohotkey.com/docs/Functions.htm#BuiltIn)



## Determining the User Idle Time
    if(A_TimeIdlePhysical > 60000) { ; 60,000 milliseconds
        WinClose, ahk_class Chrome_WidgetWin_1
        MsgBox, Google Chrome was closed due to user inactivity.
    }

This check could be done periodically, e.g. using `SetTimer`.

## Auto-insert current weekday's name
This example inserts/sends the current day of the week's full name (e.g. *Sunday*) whenever <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>D</kbd> is pressed:

    ^!d::Send, %A_DDDD%

## Extract string parts using RegEx
    myDebt := 9000
    index  := RegExMatch("You owe me $42", "\$(\d+)", dollars)
    if(index > 0) { ; indices are usually 1-based in AHK
        myDebt += dollars1
        MsgBox, Current debt: %myDebt%
    }

Result:

> Current debt: 9042



## Trim a string
    myString := "  hello, Trim()! "
    trimmed  := Trim(myString)
    FileAppend, % trimmed "`n", TrimmedStrings.txt

Note that `Trim()` will not manipulate the original string, but return a new one which should be stored or output somewhere.

