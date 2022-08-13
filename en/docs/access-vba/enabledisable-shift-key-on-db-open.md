---
title: "EnableDisable Shift Key on DB Open"
slug: "enabledisable-shift-key-on-db-open"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

This code will turn off the capability for a user to hold down the Shift key when opening a database to skip the default form opening and allow the user access to the Navigation Pane and VB Editor.  In DB’s that you do not want users to have access to either of these (along with disabling the use of Special Keys in the Current Database Options), this code will help to keep the database locked down.  

Generally, the below code is placed in it's own Module that can be named basEnableDisableShift module, but this is just a suggestion and you can place it in any module that you may already have.

In order to disable the Shift key, within your VB Editor screen, enter 'DisableShift' within your Immediate window and hit Enter.  You will then receive a message advising that the Shift Key has been Disabled.  

To re-enable the Shift key, you will once again need to return to the VB Editor screen, enter 'EnableShift' within your Immediate window and hit Enter.  You will again receive a message in your Immediate window advising the Shift Key has been Enabled.

***NOTE:***  This is not a fool proof way to enable and disable the Shift Key, but if you are deploying a database to users who are not proficient with MS Access and VBA, it should be helpful in preventing users from accessing the VB Editor and/or Navigation Pane within your database.

## Disable Shift Function Code
    Function DisableShift()
    'This function disable the shift at startup. This action causes
    'the Autoexec macro and Startup properties to always be executed.
    
    On Error GoTo errDisableShift
    
        Dim db As DAO.Database
        Dim prop As DAO.Property
        Const conPropNotFound = 3270
    
        Set db = CurrentDb()
    
        'This next line disables the shift key on startup.
        db.Properties("AllowByPassKey") = False
    
        'The function is successful.
        Debug.Print "Disabled Shift Key - Successful"
        Exit Function
    
    errDisableShift:
        'The first part of this error routine creates the "AllowByPassKey
        'property if it does not exist.
        If Err = conPropNotFound Then
            Set prop = db.CreateProperty("AllowByPassKey", _
            dbBoolean, False)
            db.Properties.Append prop
            Resume Next
            Else
                MsgBox "Function 'ap_DisableShift' did not complete successfully."
                GoTo ExitHere
        End If
    
    ExitHere:
        Set prop = Nothing
        Set db = Nothing
        Exit Function
    
    End Function

## Enable Shift Function Code
    Function EnableShift()
    'This function enables the SHIFT key at startup. This action causes
    'the Autoexec macro and the Startup properties to be bypassed
    'if the user holds down the SHIFT key when the user opens the database.
    
    On Error GoTo errEnableShift
    
        Dim db As DAO.Database
        Dim prop As DAO.Property
        Const conPropNotFound = 3270
    
        Set db = CurrentDb()
    
        'This next line of code disables the SHIFT key on startup.
        db.Properties("AllowByPassKey") = True
    
        'function successful
        Debug.Print "Enabled Shift Key - Successful"
        GoTo ExitHere
    
    errEnableShift:
        'The first part of this error routine creates the "AllowByPassKey
        'property if it does not exist.
        If Err = conPropNotFound Then
            Set prop = db.CreateProperty("AllowByPassKey", _
            dbBoolean, True)
            db.Properties.Append prop
            Resume Next
            Else
                MsgBox "Function 'ap_DisableShift' did not complete successfully."
                GoTo ExitHere
        End If
    
    ExitHere:
        Set prop = Nothing
        Set db = Nothing
        Exit Function
    
    End Function



