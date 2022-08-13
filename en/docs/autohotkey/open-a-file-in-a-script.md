---
title: "Open a File in a Script"
slug: "open-a-file-in-a-script"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Different ways to open a file to work with in a script.

## Open a File through Windows Explorer
Inside the script, use the first line to store the very first variable (in this example, `%1%`) with a name to deal with. Example: `OpenWithFile = %1%`

Once you open a file with this script through Windows (Right click on any file on MS Windows and choose 'Open with...' then select the **compiled** version of the script such as script.exe) the name of the choosed file will be stored in this variable and, so, the script will be able to work with it. Example:

    OpenWithFile = %1%
    if OpenWithFile !=
    {
    FileRead, content, %OpenWithFile%
    msgbox %content%
    return
    }

## Open a File through SelectFile dialog box
The following example creates a Gui with a single button wich brings the SelectFile dialog box.

    Gui, Loader: New 
    Gui, Loader: Add, Button, Default Center w220 vLOAD, LOAD
    Gui, Loader: Show, AutoSize Center, Loader
    
    return
    
    LoaderButtonLOAD:
    FileSelectFile, LoadedFile, , , ,
    
    if ErrorLevel=1
    {
    return
    }
    
    else
    {
        FileRead, content, %LoadedFile%
        msgbox %content%    
    }
    return

## Open a File through Windows Drag n' Drop
This examples creates a new empty Gui sensible to Drag n' Drop event:

    Gui, Dropper: New
    Gui, Dropper: Font, s10 w700
    Gui, Dropper: Add, Text, y80 vText1, Drag the files here
    Gui, Dropper: Show, w200 h200 Center, Dropper
    
    return
    
    DropperGuiDropFiles:
    DroppedFile:=A_GuiEvent
    
        FileRead, content, %DroppedFile%
        msgbox %content%    
    
    return

