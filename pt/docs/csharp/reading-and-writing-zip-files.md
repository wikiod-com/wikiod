---
title: "Lendo e gravando arquivos .zip"
slug: "lendo-e-gravando-arquivos-zip"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Sintaxe
1. ZipArchive estático público OpenRead(string archiveFileName)

## Parâmetros


| Parâmetro | Detalhes |
| ------ | ------ |
| archiveFileName | O caminho para o arquivo a ser aberto, especificado como um caminho relativo ou absoluto. Um caminho relativo é interpretado como relativo ao diretório de trabalho atual. | |


## Gravando em um arquivo zip
Para escrever um novo arquivo .zip:

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

## Gravando arquivos zip na memória
O exemplo a seguir retornará os dados `byte[]` de um arquivo compactado contendo os arquivos fornecidos a ele, sem a necessidade de acesso ao sistema de arquivos.


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

## Obter arquivos de um arquivo Zip
Este exemplo obtém uma lista de arquivos dos dados binários do arquivo zip fornecidos:

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

## O exemplo a seguir mostra como abrir um arquivo zip e extrair todos os arquivos .txt para uma pasta
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

