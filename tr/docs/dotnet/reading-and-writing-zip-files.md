---
title: "Zip dosyalarını okuma ve yazma"
slug: "zip-dosyalarn-okuma-ve-yazma"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

**ZipFile** sınıfı, **System.IO.Compression** ad alanında bulunur. Zip dosyalarından okumak ve bunlara yazmak için kullanılabilir.

* FileStream yerine MemoryStream de kullanabilirsiniz.

* İstisnalar

| İstisna | Şart
| --------------------------- | ------ 
| ArgumentException | Akış zaten kapatılmış veya akışın özellikleri modla eşleşmiyor (örneğin: salt okunur bir akışa yazmaya çalışmak)
| ArgumentNullException | giriş *akış* boş
| ArgumentOutOfRangeException | *mode* geçersiz bir değere sahip
| InvalidDataException | Aşağıdaki listeye bakın

Bir **InvalidDataException** oluşturulduğunda bunun 3 nedeni olabilir:

* Akışın içeriği bir zip arşivi olarak yorumlanamadı
* *mode* Günceldir ve arşivde bir giriş eksik veya bozuk ve okunamıyor
* *mode* Güncelleme'dir ve bir giriş belleğe sığmayacak kadar büyük

Tüm bilgiler [bu MSDN sayfasından](https://msdn.microsoft.com/en-us/library/system.io.compression.ziparchive(v=vs.110).aspx) alınmıştır.

## ZIP içeriklerini listeleme
Bu snippet, bir zip arşivinin tüm dosya adlarını listeler. Dosya adları zip köküne göredir.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        for (int i = 0; i < archive.Entries.Count; i++)
        {
            Console.WriteLine($"{i}: {archive.Entries[i]}");
        }
    }

## Dosyaları ZIP dosyalarından çıkarma
Tüm dosyaları bir dizine çıkarmak çok kolaydır:

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        archive.ExtractToDirectory(AppDomain.CurrentDomain.BaseDirectory);
    }

Dosya zaten mevcut olduğunda, bir **System.IO.IOException** atılır.

Belirli dosyaların ayıklanması:




    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
    {
        // Get a root entry file
        archive.GetEntry("test.txt").ExtractToFile("test_extracted_getentries.txt", true);

        // Enter a path if you want to extract files from a subdirectory
        archive.GetEntry("sub/subtest.txt").ExtractToFile("test_sub.txt", true);

        // You can also use the Entries property to find files
        archive.Entries.FirstOrDefault(f => f.Name == "test.txt")?.ExtractToFile("test_extracted_linq.txt", true);

        // This will throw a System.ArgumentNullException because the file cannot be found
        archive.GetEntry("nonexistingfile.txt").ExtractToFile("fail.txt", true);
    }

Bu yöntemlerden herhangi biri aynı sonucu verecektir.

## ZIP dosyasını güncelleme
Bir ZIP dosyasını güncellemek için dosyanın bunun yerine ZipArchiveMode.Update ile açılması gerekir.

    using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
    using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Update))
    {
        // Add file to root
        archive.CreateEntryFromFile("test.txt", "test.txt");

        // Add file to subfolder
        archive.CreateEntryFromFile("test.txt", "symbols/test.txt");
    }

Arşivdeki bir dosyaya doğrudan yazma seçeneği de vardır:

    var entry = archive.CreateEntry("createentry.txt");
    using(var writer = new StreamWriter(entry.Open()))
    {
        writer.WriteLine("Test line");
    }

