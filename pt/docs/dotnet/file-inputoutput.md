---
title: "Entradasaída de arquivo"
slug: "entradasaida-de-arquivo"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Parâmetros
| Parâmetro| Detalhes |
| ------ | ------ |
| caminho da string | Caminho do arquivo a ser verificado. (relativo ou totalmente qualificado) |

Retorna true se o arquivo existir, false caso contrário.

## Arquivo C#.Existe()
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

Também pode ser usado em um operador ternário.

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

