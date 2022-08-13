---
title: "Entradasalida de archivos"
slug: "entradasalida-de-archivos"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Parámetros
| Parámetro| Detalles |
| ------ | ------ |
| ruta de cadena | Ruta del archivo a comprobar. (pariente o totalmente calificado) |

Devuelve verdadero si el archivo existe, falso en caso contrario.

## C# Archivo.Existe()
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

También se puede utilizar en un operador ternario.

    Console.WriteLine(File.Exists(pathToFile) ? "Exists" : "Does not exist");

## VB WriteAllText
<!-- idioma-todo: lang-vb -->
    Imports System.IO

    Dim filename As String = "c:\path\to\file.txt"
    File.WriteAllText(filename, "Text to write" & vbCrLf)

## VB StreamWriter
<!-- idioma-todo: lang-vb -->
    Dim filename As String = "c:\path\to\file.txt"
    If System.IO.File.Exists(filename) Then
        Dim writer As New System.IO.StreamWriter(filename)
        writer.Write("Text to write" & vbCrLf) 'Add a newline
        writer.close()
    End If

## C# Stream Writer
<!-- lenguaje-todo: c# -->
    using System.Text;
    using System.IO;
    
    string filename = "c:\path\to\file.txt";
    //'using' structure allows for proper disposal of stream.
    using (StreamWriter writer = new StreamWriter(filename"))
    {
        writer.WriteLine("Text to Write\n");
    }

## C# EscribirTodoElTexto()
    using System.IO;
    using System.Text;
    
    string filename = "c:\path\to\file.txt";
    File.writeAllText(filename, "Text to write\n");

