---
title: "FileSystem Objects"
slug: "filesystem-objects"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Moving a File/Folder
**Methods Used:**

    .MoveFile(Source, Dest)
    .MoveFolder(Source, Dest)

The following code illustrates the use of **MoveFile** method to Move a file to a new location. The same thing can be achieved for the folders by using the **MoveFolder** method.

**Code:**

    Dim objFso, strSourcePath, strDestPath
    strSourcePath = "C:\Users\GS\Desktop\Source.txt"
    strDestPath = "C:\Users\GS\Desktop\Folder\Dest.txt"
    Set objFso = CreateObject("Scripting.FileSystemObject")
    If objFso.FileExists(strSourcePath) then
        objFso.MoveFile strSourcePath, strDestPath
    End If
    Set objFso = Nothing

NOTE: We do not have any method of a filesystem object which allows us to rename a file. However, this can be achieved by **MoveFile** method by moving the file to the same location with a different name as shown below:

    Dim objFso, strSourcePath, strDestPath
    strSourcePath = "C:\Users\GS\Desktop\OldName.txt"
    strDestPath = "C:\Users\GS\Desktop\NewName.txt"       'Location is same but the name is different
    Set objFso = CreateObject("Scripting.FileSystemObject")
    If objFso.FileExists(strSourcePath) then
        objFso.MoveFile strSourcePath, strDestPath
    End If
    Set objFso = Nothing

## Object reference to a File
**Methods Used:**

    .GetFile(strPath) - Returns an object referring to a file.

We can set an object reference to a file using the **getFile** method and perform different operations on them.

**Code:**

    Dim strFilePath, objFso, objFile
    strFilePath = "C:\Users\GS\Desktop\LogsFolder\file.txt"
    Set objFso = CreateObject("Scripting.FileSystemObject")
    Set objFile = objFso.getFile(strFilePath)
    
    'Accessing the File's Properties
    Msgbox objFile.Name                            'Returns the File's Name
    Msgbox objFile.Size                            'Returns the File's size in Bytes  
    Msgbox objFile.DateCreated                     'Returns the File's creation date 
    Msgbox objFile.DateLastModified                'Returns the File's last modified date
    Msgbox objFile.Path                            'Returns the File's absolute path

    'Using the File's Methods
    objFile.Delete True                            'Forcefully deletes the File
    objFile.Copy strDestPath, True                 'Copies the file to path contained in variable strDestPath
    objFile.Move strDestPath                       'Moves the file to the path contained in the variable strDestPath
    objFile.OpenAsTextStream mode                  'Opens the file as a text stream in either Read mode(mode=1), write mode(mode=2) or Append mode(mode=8)
    Set objFile = Nothing
    Set objFso = Nothing




## Checking for the existence of a file/folder/drive
**Methods used:**

    .DriveExists(strDrive) returns (True/False)
    .FileExists(strFile) returns (True/False)
    .FolderExists(strFolder) returns (True/False)

The following code checks for the existence of a file using the "**FileExists**" method of a file system object. For checking the existence of Folder or a drive, one can use the method "**FolderExists**" or "**DriveExists**" respectively.    

**Code:**

    Dim strPath, objFso
    strPath = "C:\Users\GS\Desktop\tasks.txt"        'Enter the absolute path of the File/Folder/Drive
    Set objFso = CreateObject("Scripting.FileSystemObject")
    
    'Checking for the File's existence
    If objFso.FileExists(strPath) then               'returns True if the file exists, else False
        Msgbox "File Exists!"
    Else
        Msgbox "File does not Exist!"
    End If
    Set objFso = Nothing



## Deleting an existing folder and creating a new Folder
**Methods used:**

    .DeleteFolder(FileSpec, Force (True/False))
    .CreateFolder(Path)
    .DeleteFile(FileSpec, Force (True/False))

The following example illustrates the Deletion and creation of a folder using the methods "**DeleteFolder**" and "**CreateFolder**".

**Code:**

    Dim strFolderPath, objFso
    strFolderPath = "C:\Users\GS\Desktop\testFolder"
    Set objFso = CreateObject("Scripting.Filesystemobject")
    
    'Checking for the folder's existence and deleting it, if found
    If objFso.FolderExists(strFolderPath) then
        objFso.DeleteFolder strFolderPath, True                   'True indicates forceful deletion
    End If
    
    'Creating a new Folder
    objFso.CreateFolder strFolderPath
    
    Set objFso = Nothing

Similarly, One can Delete a File using the "**DeleteFile**" method:

    Dim strFilePath:strFilePath = "C:\Users\GS\Desktop\tasks.txt"
    If objFso.FileExists(strFilePath) then
        objFso.DeleteFile strFilePath, True                      'true indicates forceful deletion
    End If

## Copying a File/Folder
**Methods Used:**

    .CopyFile(Source, Dest [,Overwrite (True/False)]
    .CopyFolder(Source, Dest [,Overwrite (True/False)]

The following code illustrates the use of **CopyFile** method to copy a file to a new location. The same thing can be achieved for the folders by using the **CopyFolder** method.

**Code:**

    Dim objFso, strSourcePath, strDestPath
    strSourcePath = "C:\Users\GS\Desktop\Source.txt"
    strDestPath = "C:\Users\GS\Desktop\Dest.txt"
    Set objFso = CreateObject("Scripting.FileSystemObject")
    If objFso.FileExists(strSourcePath) then
        objFso.CopyFile strSourcePath, strDestPath, True              'True indicates the overwritting of the file at the destination path i.e, if the file already exists, it will be overwritten
    End If
    Set objFso = Nothing

## Object reference to a folder
**Methods used:**

    .GetFolder(strPath) - Returns an object referring to the path

We can set an object reference to a folder using the **getFolder** method and perform different operations on them.

**Code:**

    Dim strFolderPath, objFso, objFolder
    strFolderPath = "C:\Users\GS\Desktop\LogsFolder"
    Set objFso = CreateObject("Scripting.FileSystemObject")
    Set objFolder = objFso.getFolder(strFolderPath)
    
    'Accessing the Folder's Properties
    Msgbox objFolder.Name                            'Returns the Folder's Name
    Msgbox objFolder.Size                            'Returns the Folder's size in Bytes  
    Msgbox objFolder.DateCreated                     'Returns the Folder's creation date 
    Msgbox objFolder.DateLastModified                'Returns the Folder's last modified date
    Msgbox objFolder.Path                            'Returns the Folder's Absolute Path
    
    Dim objChildFolders
    Set objChildFolders = objFolder.SubFolders       'Returns the collection of all subfolder 

    Dim objChildFiles
    Set objChildFiles = objFolder.Files              'Returns the collection of all files contained in the folder  

    'Using the Folder's methods
    objFolder.Copy strDestPAth, True                 'Copies the folder to path contained in strDestPath and overwrite Flag=True
    objFolder.Delete True                            'Deletes the Folder; True indicates forceful Deletion
    objFolder.Move strDestPath                       'Moves the Folder to the path contained in strDestPath variable 
    objFolder.CreateTextFile strFileName, True       'Created a new text file inside the folder and overwrites the existing file(if it exists)
    Set objChildFiles = Nothing
    Set objChildFolders = Nothing
    Set objFolder = Nothing
    Set objFso = Nothing

