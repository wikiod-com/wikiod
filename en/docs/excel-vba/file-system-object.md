---
title: "File System Object"
slug: "file-system-object"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## File, folder, drive exists
File exists:
===========

    Sub FileExists()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        If fso.FileExists("D:\test.txt") = True Then
           MsgBox "The file is exists."
        Else
           MsgBox "The file isn't exists."
        End If
    End Sub

Folder exists:
==============

    Sub FolderExists()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        If fso.FolderExists("D:\testFolder") = True Then
           MsgBox "The folder is exists."
        Else
           MsgBox "The folder isn't exists."
        End If
    End Sub

Drive exists:
=============

    Sub DriveExists()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        If fso.DriveExists("D:\") = True Then
           MsgBox "The drive is exists."
        Else
           MsgBox "The drive isn't exists."
        End If
    End Sub

## Basic file operations
Copy:
=====

    Sub CopyFile()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.CopyFile "c:\Documents and Settings\Makro.txt", "c:\Documents and Settings\Macros\"
    End Sub

Move:
=====

    Sub MoveFile()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.MoveFile "c:\*.txt", "c:\Documents and Settings\"
    End Sub

Delete:
=======

    Sub DeleteFile()
        Dim fso
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.DeleteFile "c:\Documents and Settings\Macros\Makro.txt"
    End Sub




## Basic folder operations
Create:
=======

    Sub CreateFolder()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.CreateFolder "c:\Documents and Settings\NewFolder"
    End Sub

Copy:
=====

    Sub CopyFolder()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.CopyFolder "C:\Documents and Settings\NewFolder", "C:\"
    End Sub

Move:
=====

    Sub MoveFolder()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.MoveFolder "C:\Documents and Settings\NewFolder", "C:\"
    End Sub

Delete:
=======

    Sub DeleteFolder()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        fso.DeleteFolder "C:\Documents and Settings\NewFolder"
    End Sub






## Other operations
Get file name:
==============

    Sub GetFileName()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        MsgBox fso.GetFileName("c:\Documents and Settings\Makro.txt")
    End Sub

***Result:** Makro.txt*

Get base name:
==============

    Sub GetBaseName()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        MsgBox fso.GetBaseName("c:\Documents and Settings\Makro.txt")
    End Sub

***Result:** Makro*

Get extension name:
===================
    Sub GetExtensionName()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        MsgBox fso.GetExtensionName("c:\Documents and Settings\Makro.txt")
    End Sub

***Result:** txt*

Get drive name:
===============

    Sub GetDriveName()
        Dim fso as Scripting.FileSystemObject
        Set fso = CreateObject("Scripting.FileSystemObject")
        MsgBox fso.GetDriveName("c:\Documents and Settings\Makro.txt")
    End Sub

***Result:** c:*

