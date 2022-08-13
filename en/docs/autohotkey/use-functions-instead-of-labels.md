---
title: "Use functions instead of labels"
slug: "use-functions-instead-of-labels"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

AutoHotkey used to heavily rely on labels until version 1.1.20. It's reliance on labels had very serious disadvantages. The main one being that labels usually execute in the global scope meaning that any variable defined within a label will be globally available. This sounds great until you realize that for example you can't just use other peoples libraries without making sure that their variables don't interfere with yours.  
Working in the global scope when not necessary is simply bad practice. 

So this is where functions come in. As of version 1.1.20, every AutoHotkey command that accepts a label-name as a parameter, now alternatively accepts a function-name.  

## Hotkeys with functions instead of labels
Examples of using functions with hotkeys:

    Hotkey, a, MyFunction ; Calls MyFunction() when a is pressed
    
    MyFunction() {
        MsgBox You pressed %A_ThisHotkey%.
    }

Or:

    a::MyFunction()
    
    MyFunction() {
        MsgBox You pressed %A_ThisHotkey%.
    }

## Very basic example demonstrating function use on SetTimer. 
    ;Sends the keystroke for the letter "a" every 3 seconds.
    #Persistent
    SetTimer, SendLetterA, 3000
    return
    
    SendLetterA() {
        Send, a
    }

## Gui with functions instead of labels
Example showing how to create Guis using functions instead of labels.

    Gui, Add, Button, gCtrlEvent vButton1, Button 1
    Gui, Add, Button, gCtrlEvent vButton2, Button 2
    Gui, Add, Button, gGoButton, Go Button
    Gui, Add, Edit, vEditField, Example text
    Gui, Show,, Functions instead of labels
    
    CtrlEvent(CtrlHwnd:=0, GuiEvent:="", EventInfo:="", ErrLvl:="") {
        GuiControlGet, controlName, Name, %CtrlHwnd%
        MsgBox, %controlName% has been clicked!
    }
    GoButton(CtrlHwnd:=0, GuiEvent:="", EventInfo:="", ErrLvl:="") {
        GuiControlGet, EditField
        MsgBox, Go has been clicked! The content of the edit field is "%EditField%"!
    }
    
    GuiClose(hWnd) {
        WinGetTitle, windowTitle, ahk_id %hWnd%
        MsgBox, The Gui with title "%windowTitle%" has been closed!
        ExitApp
    }

## More complicated Gui example with multiple listviews using the same event callback function
This script demonstrates how to receive complicated GUI events from different controls in the same event callback function. We'll be using two ListView controls for that.  
Now every time an action is detected on one of those ListView controls, we want a precise description of what happened and have that logged into an edit control in the same GUI. 

    Gui, Add, ListView, gListCtrlEvent vMyFirstListView AltSubmit -ReadOnly R10 w310, ColumnTitle1|ColumnTitle2|ColumnTitle3
    Gui, Add, ListView, gListCtrlEvent vMySecondListView AltSubmit -ReadOnly R10 w310, ColumnTitle1|ColumnTitle2|ColumnTitle3
    Gui, Add, Text, w310, Action Log
    Gui, Add, Edit, vLog R7 w310, 
    Gui, Show,, Functions instead of labels
    
    ; Create example entries for the first ListView
    Gui, ListView, MyFirstListView
    Loop, 10 {
        LV_Add("", "Column-1 | Row-" A_Index ,  "Column-2 | Row-" A_Index,  "Column-3 | Row-" A_Index)
    }
    LV_ModifyCol()
    
    ; Create example entries for the second ListView
    Gui, ListView, MySecondListView
    Loop, 10 {
        LV_Add("", "Column-1 | Row-" A_Index ,  "Column-2 | Row-" A_Index,  "Column-3 | Row-" A_Index)
    }
    LV_ModifyCol()
    
    
    ListCtrlEvent(ctrlHwnd:=0, guiEvent:="", eventInfo:="", errLvl:="") {
        GuiControlGet, ctrlName, Name, %CtrlHwnd%
        whatHappened := "Action detected!`n"
        whatHappened .= "Control handle: " ctrlHwnd "`n"
        whatHappened .= "Control name: " ctrlName "`n"
        
        If (guiEvent = "DoubleClick") {
            whatHappened .= "`nThe user has double-clicked within the control."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "R") {
            whatHappened .= "`nThe user has double-right-clicked within the control."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "ColClick") {
            whatHappened .= "`nThe user has clicked a column header."
            whatHappened .= "`n> Column number: " eventInfo
        } Else If (guiEvent = "D") {
            whatHappened .= "`nThe user has attempted to start dragging a row or icon."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "d") {
            whatHappened .= "`nThe user has attempted to start right-click-dragging a row or icon."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "e") {
            whatHappened .= "`nThe user has finished editing the first field of a row."
            whatHappened .= "`n> Row number: " eventInfo
        } Else If (guiEvent = "Normal") {
            whatHappened .= "`nThe user has left-clicked a row."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "RightClick") {
            whatHappened .= "`nThe user has right-clicked a row."
            whatHappened .= "`n> Focused row number: " eventInfo
        } Else If (guiEvent = "A") {
            whatHappened .= "`nA row has been activated."
            whatHappened .= "`n> Row number: " eventInfo
        } Else If (guiEvent = "C") {
            whatHappened .= "`nThe ListView has released mouse capture."
        } Else If (guiEvent = "E") {
            whatHappened .= "`nThe user has begun editing the first field of a row."
            whatHappened .= "`n> Row number: " eventInfo
        } Else If (guiEvent = "F") {
            whatHappened .= "`nThe ListView has received keyboard focus."
        } Else If (guiEvent = "f") {
            whatHappened .= "`nThe ListView has lost keyboard focus."
        } Else If (guiEvent = "I") {
            whatHappened .= "`nItem changed. (A row has changed by becoming selected/deselected, checked/unchecked, etc.)"
            whatHappened .= "`n> Row number: " eventInfo
        } Else If (guiEvent = "K") {
            whatHappened .= "`nThe user has pressed a key while the ListView has focus."
            whatHappened .= "`n> Key pressed: " GetKeyName(Format("vk{:x}", eventInfo))
        } Else If (guiEvent = "M") {
            whatHappened .= "`nItem changed. (A row has changed by becoming selected/deselected, checked/unchecked, etc.)"
            whatHappened .= "`n> Row number: " eventInfo
        } Else If (guiEvent = "S") {
            whatHappened .= "`nMarquee. The user has started to drag a selection-rectangle around a group of rows or icons."
        } Else If (guiEvent = "s") {
            whatHappened .= "`nThe user has finished scrolling the ListView."
        }
        GuiControlGet, Log
        GuiControl,, Log, % whatHappened "`n---------------------`n" Log
    }
    
    GuiClose(hWnd) {
        WinGetTitle, windowTitle, ahk_id %hWnd%
        MsgBox, The Gui with title "%windowTitle%" is going to be closed! This script will exit afterwards!
        ExitApp
    }

## Advanced use of SetTimer: Calling the same function with different parameters
This is an example of something that would have been straight up impossible with labels. If you execute the same label multiple times at the same time and they rely on variables that are being defined within them, they very likely interfere and cause unexpected behavior. 

Here is how to do it with functions:

    ; This script will switch between showing "Hello 1" and "Hello 2"
    
    #Persistent
    DisplayMessage_Hello1 := Func("DisplayMessage").bind("Hello 1")
    SetTimer, %DisplayMessage_Hello1%, 2000
    
    Sleep, 1000
    
    DisplayMessage_Hello2 := Func("DisplayMessage").bind("Hello 2")
    SetTimer, %DisplayMessage_Hello2%, 2000
    
    DisplayMessage(messageToDisplay) {
        TrayTip ; remove other traytips
        TrayTip, Message to display:, %messageToDisplay%
    }

Here is **how to not do it** (with labels):  

    ;This script will never display the message "Hello 1". It will always show "Hello 2".
    
    #Persistent
    messageToDisplay := "Hello 1"
    SetTimer, DisplayMessage, 2000
    
    Sleep, 1000
    
    messageToDisplay := "Hello 2"
    SetTimer, DisplayMessage, 2000
    
    DisplayMessage:
        TrayTip ; remove other traytips
        TrayTip, Message to display:, %messageToDisplay%
    Return

## Tray menu actions with functions
    #Persistent
    
    Menu, Tray, NoStandard ; remove default tray menu entries
    Menu, Tray, Add, MyDefaultAction, OnDefaultTrayAction ; add a new tray menu entry
    Menu, Tray, Add, Exit, Exit ; add another tray menu entry
    Menu, Tray, Default, MyDefaultAction ;When doubleclicking the tray icon, run the tray menu entry called "MyDefaultAction".
    
    
    OnDefaultTrayAction() {
        MsgBox, You double clicked the tray icon of this script or you clicked the MyDefaultAction entry!
    }
    
    Exit() {
        MsgBox, You clicked the Exit entry! The script will close itself now.
        ExitApp
    }

## General example of functions vs labels
I'll demonstrate the basic usage of using functions vs using labels+gosub.  
In this example we'll implement simple functionality to add two numbers and store them in a variable.

With functions:

    c := Add(3, 2) ; function call
    MsgBox, Result: %c%
    
    Add(a, b) { ; This is a function. Put it wherever you want, it doesn't matter.
        ; the a and b inside of this function are set by the function call above
        Return a+b ; the function will return the result of the expression "a+b"
    }

With labels (please don't do that):

    a := 3 
    b := 2 
    GoSub, Add ; execute the label "Add" then jump back to the next line here
    MsgBox, Result: %c%
    Return ; without this, the label would be executed again for no reason.
    
    Add: ; This is a label. Please put them at the bottom of your script and use "Return" in a line above.
        c := a+b
    Return

## OnClipboardChange with fucntion/label
The following code is taken from he official AutoHotkey documentation:

**Function implementation:**

    #Persistent
    OnClipboardChange("ClipChanged")
    return
    
    ClipChanged(Type) {
        ToolTip Clipboard data type: %Type%
        Sleep 1000
        ToolTip  ; Turn off the tip.
    }

**Label implementation:**

    #Persistent
    return
    
    OnClipboardChange:
    ToolTip Clipboard data type: %A_EventInfo%
    Sleep 1000
    ToolTip  ; Turn off the tip.
    return

