---
title: "File InputOutput"
slug: "file-inputoutput"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Parameters
| Parameter| Details |
| ------ | ------ |
| string path   | Path of the file to check. (relative or fully qualified) |

Returns true if the file exists, false otherwise.

## C# File.Exists()
    using System;
    using System.IO;
                    
    public class Program
    {
        public static void Main()
        {
            string filePath = "somePath";
        
            if(File.Exists(filePath))
            {
                Console.WriteLine("Exists");
            }
            else
            {
                Console.WriteLine("Does not exist");    
            }
        }
    }

Can also be used in a ternary operator.

    Console.WriteLine(File.Exists(pathToFile) ? "Exists" : "Does not exist");

## VB WriteAllText
<!-- language-all: lang-vb -->
    Imports System.IO

    Dim filename As String = "c:\path\to\file.txt"
    File.WriteAllText(filename, "Text to write" & vbCrLf)

## VB StreamWriter
<!-- language-all: lang-vb -->
    Dim filename As String = "c:\path\to\file.txt"
    If System.IO.File.Exists(filename) Then
        Dim writer As New System.IO.StreamWriter(filename)
        writer.Write("Text to write" & vbCrLf) 'Add a newline
        writer.close()
    End If

## C# StreamWriter
<!-- language-all: c# -->
    using System.Text;
    using System.IO;
    
    string filename = "c:\path\to\file.txt";
    //'using' structure allows for proper disposal of stream.
    using (StreamWriter writer = new StreamWriter(filename"))
    {
        writer.WriteLine("Text to Write\n");
    }

## C# WriteAllText()
    using System.IO;
    using System.Text;
    
    string filename = "c:\path\to\file.txt";
    File.writeAllText(filename, "Text to write\n");

