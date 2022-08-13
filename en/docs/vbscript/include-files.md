---
title: "Include files"
slug: "include-files"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

When running VbScript in Windows shell, there is no built in function to include a file, therefore, to organize your code in different files you'll need to create a method to do that.

A few things to keep in mind when using the `IncludeFile(p_Path)` method :
- There is no limitation of file type that can be included but the included files content must be VbScript.
- If there is a syntax error in the included file, you will not get the line/column of the error.
- You must define and initialize `std_internal_LibFiles` before the first call to `IncludeFile(p_Path)`
- You can use `IncludeFile(p_Path)` anywhere in your code, including other methods.

## Creating an "include file" method
So the main goal of this function is to :
- Be standalone because it needs to be written in the main VbScript file and cannot be in an included file (because it defines the include function)
- Provide enough information if something goes wrong (ie. the file that was being included, the error that occurred, ...)
- Include a file once and only once to avoid include loops.


    ' *************************************************************************************************
    '! Includes a VbScript file
    '! @param p_Path    The path of the file to include
    ' *************************************************************************************************
    Sub IncludeFile(p_Path)
        ' only loads the file once
        If std_internal_LibFiles.Exists(p_Path) Then
            Exit Sub
        End If
        
        ' registers the file as loaded to avoid to load it multiple times
        std_internal_LibFiles.Add p_Path, p_Path

        Dim objFso, objFile, strFileContent, strErrorMessage
        Set objFso = CreateObject("Scripting.FileSystemObject")
        
        ' opens the file for reading
        On Error Resume Next
        Set objFile = objFso.OpenTextFile(p_Path)
        If Err.Number <> 0 Then
            ' saves the error before reseting it
            strErrorMessage = Err.Description & " (" &  Err.Source & " " & Err.Number & ")"
            On Error Goto 0
            Err.Raise -1, "ERR_OpenFile", "Cannot read '" & p_Path & "' : " & strErrorMessage
        End If
        
        ' reads all the content of the file
        strFileContent = objFile.ReadAll
        If Err.Number <> 0 Then
            ' saves the error before reseting it
            strErrorMessage = Err.Description & " (" &  Err.Source & " " & Err.Number & ")"
            On Error Goto 0
            Err.Raise -1, "ERR_ReadFile", "Cannot read '" & p_Path & "' : " & strErrorMessage
        End If
        
        ' this allows to run vbscript contained in a string
        ExecuteGlobal strFileContent
        If Err.Number <> 0 Then
            ' saves the error before reseting it
            strErrorMessage = Err.Description & " (" &  Err.Source & " " & Err.Number & ")"
            On Error Goto 0
            Err.Raise -1, "ERR_Include", "An error occurred while including '" & p_Path & "' : " & vbCrlf & strErrorMessage
        End If
    End Sub


## Including files
To include a file in another file, just use the one liner :

    IncludeFile "myOtherFile.vbs"

## Global initialization
Before we use the IncludeFile method, we need to :
- Declare `std_internal_LibFiles` globally
- Initialize it with a new dictionary


    Dim std_internal_LibFiles
    Set std_internal_LibFiles = CreateObject("Scripting.Dictionary")

