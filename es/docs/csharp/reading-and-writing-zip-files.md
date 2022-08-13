---
title: "Leer y escribir archivos .zip"
slug: "leer-y-escribir-archivos-zip"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Sintaxis
1. ZipArchive OpenRead estático público (string archiveFileName)

## Parámetros


| Parámetro | Detalles |
| ------ | ------ |
| archiveFileName | La ruta al archivo que se va a abrir, especificada como ruta relativa o absoluta. Una ruta relativa se interpreta como relativa al directorio de trabajo actual. | |


## Escribir en un archivo zip
Para escribir un nuevo archivo .zip:

    System.IO.Compression
    System.IO.Compression.FileSystem

    using (FileStream zipToOpen = new FileStream(@"C:\temp", FileMode.Open)) 
    {
        using (ZipArchive archive = new ZipArchive(zipToOpen, ZipArchiveMode.Update)) 
        {
            ZipArchiveEntry readmeEntry = archive.CreateEntry("Readme.txt");
            using (StreamWriter writer = new StreamWriter(readmeEntry.Open())) 
            {
                writer.WriteLine("Information about this package.");
                writer.WriteLine("========================");
            }
        }
    }

## Escritura de archivos zip en la memoria
El siguiente ejemplo devolverá los datos `byte[]` de un archivo comprimido que contiene los archivos proporcionados, sin necesidad de acceder al sistema de archivos.


    public static byte[] ZipFiles(Dictionary<string, byte[]> files)
    {
        using (MemoryStream ms = new MemoryStream())
        {
            using (ZipArchive archive = new ZipArchive(ms, ZipArchiveMode.Update))
            {
                foreach (var file in files)
                {
                    ZipArchiveEntry orderEntry = archive.CreateEntry(file.Key); //create a file with this name
                    using (BinaryWriter writer = new BinaryWriter(orderEntry.Open()))
                    {
                        writer.Write(file.Value); //write the binary data
                    }
                }
            }
            //ZipArchive must be disposed before the MemoryStream has data
            return ms.ToArray();
        }
    }

## Obtener archivos de un archivo Zip
Este ejemplo obtiene una lista de archivos de los datos binarios del archivo zip proporcionado:

    public static Dictionary<string, byte[]> GetFiles(byte[] zippedFile) 
    {
        using (MemoryStream ms = new MemoryStream(zippedFile))
        using (ZipArchive archive = new ZipArchive(ms, ZipArchiveMode.Read)) 
        {
            return archive.Entries.ToDictionary(x => x.FullName, x => ReadStream(x.Open()));
        }
    }

    private static byte[] ReadStream(Stream stream) 
    {
        using (var ms = new MemoryStream()) 
        {
            stream.CopyTo(ms);
            return ms.ToArray();
        }
    }

## El siguiente ejemplo muestra cómo abrir un archivo zip y extraer todos los archivos .txt a una carpeta
    using System;
    using System.IO;
    using System.IO.Compression;
    
    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                string zipPath = @"c:\example\start.zip";
                string extractPath = @"c:\example\extract";
    
                using (ZipArchive archive = ZipFile.OpenRead(zipPath))
                {
                    foreach (ZipArchiveEntry entry in archive.Entries)
                    {
                        if (entry.FullName.EndsWith(".txt", StringComparison.OrdinalIgnoreCase))
                        {
                            entry.ExtractToFile(Path.Combine(extractPath, entry.FullName));
                        }
                    }
                } 
            }
        }
    }

