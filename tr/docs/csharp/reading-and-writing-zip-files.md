---
title: ".zip dosyalarını okuma ve yazma"
slug: "zip-dosyalarn-okuma-ve-yazma"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Sözdizimi
1. genel statik ZipArchive OpenRead(string arşivDosyaAdı)

## Parametreler


| parametre | Ayrıntılar |
| ------ | ------ |
| arşivDosyaAdı | Göreceli veya mutlak bir yol olarak belirtilen, açılacak arşivin yolu. Göreceli bir yol, geçerli çalışma dizinine göreli olarak yorumlanır. | |


## Zip dosyasına yazma
Yeni bir .zip dosyası yazmak için:

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

## Bellekte Zip Dosyaları Yazma
Aşağıdaki örnek, kendisine sağlanan dosyaları içeren sıkıştırılmış bir dosyanın "bayt[]" verilerini, dosya sistemine erişmeye ihtiyaç duymadan döndürür.


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

## Zip dosyasından dosya alın
Bu örnek, sağlanan zip arşivi ikili verilerinden dosyaların bir listesini alır:

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

## Aşağıdaki örnek, bir zip arşivinin nasıl açılacağını ve tüm .txt dosyalarının bir klasöre nasıl çıkarılacağını gösterir.
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

