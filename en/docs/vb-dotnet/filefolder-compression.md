---
title: "FileFolder Compression"
slug: "filefolder-compression"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Creating zip archive from directory
    System.IO.Compression.ZipFile.CreateFromDirectory("myfolder", "archive.zip")

Create archive.zip file containing files which are in `myfolder`. In example paths are relative to program working directory. You can specify absolute paths.

## Extracting zip archive to directory
    System.IO.Compression.ZipFile.ExtractToDirectory("archive.zip", "myfolder")

Extracts archive.zip to myfolder directory. In example paths are relative to program working directory. You can specify absolute paths.

## Create zip archive dynamicaly


    ' Create filestream to file
    Using fileStream = New IO.FileStream("archive.zip", IO.FileMode.Create)
        ' open zip archive from stream
        Using archive = New System.IO.Compression.ZipArchive(fileStream, IO.Compression.ZipArchiveMode.Create)
            ' create file_in_archive.txt in archive
            Dim zipfile = archive.CreateEntry("file_in_archive.txt")

            ' write Hello world to file_in_archive.txt in archive
            Using sw As New IO.StreamWriter(zipfile.Open())
                sw.WriteLine("Hello world")
            End Using

        End Using
    End Using



## Adding File Compression to your project

  1. In *Solution Explorer* go to your project, right click on *References* then *Add reference&hellip;* 
  2. Search for Compression and select *System.IO.Compression.FileSystem* then press OK.
  3. Add `Imports System.IO.Compression` to the top of your code file (before any class or module, with the other `Imports` statements).

    Option Explicit On
    Option Strict On

    Imports System.IO.Compression

    Public Class Foo

        ...

    End Class
Plese note that this class (ZipArchive) is only available from .NET verison 4.5 onwards

