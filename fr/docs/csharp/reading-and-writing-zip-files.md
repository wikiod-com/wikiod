---
title: "Lire et écrire des fichiers .zip"
slug: "lire-et-ecrire-des-fichiers-zip"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Syntaxe
1. public statique ZipArchive OpenRead (chaîne archiveFileName)

## Paramètres


| Paramètre | Détails |
| ------ | ------ |
| archiveFileName | Chemin d'accès à l'archive à ouvrir, spécifié sous forme de chemin relatif ou absolu. Un chemin relatif est interprété comme relatif au répertoire de travail courant. | |


## Écrire dans un fichier zip
Pour écrire un nouveau fichier .zip :

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

## Écrire des fichiers Zip en mémoire
L'exemple suivant renverra les données `byte[]` d'un fichier compressé contenant les fichiers qui lui sont fournis, sans avoir besoin d'accéder au système de fichiers.


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

## Obtenir des fichiers à partir d'un fichier Zip
Cet exemple obtient une liste de fichiers à partir des données binaires de l'archive zip fournie :

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

## L'exemple suivant montre comment ouvrir une archive zip et extraire tous les fichiers .txt dans un dossier
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

