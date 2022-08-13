---
title: "Dosya Sistemi İzleyici"
slug: "dosya-sistemi-izleyici"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Sözdizimi
- genel FileSystemWatcher()
- genel FileSystemWatcher(dize yolu)
- genel FileSystemWatcher(dize yolu, dize filtresi)

## Parametreler
|yol |filtre|
|---|---|
|Standart veya Evrensel Adlandırma Kuralı (UNC) gösteriminde izlenecek dizin.||İzlenecek dosya türü. Örneğin, "*.txt", tüm metin dosyalarındaki değişiklikleri izler.|


## Dosya Hazır
FileSystemWatcher ile başlayan birçok kişinin yaptığı yaygın bir hata, FileWatcher olayının dosya oluşturulur oluşturulmaz başlatıldığını hesaba katmamasıdır.
Ancak, dosyanın tamamlanması biraz zaman alabilir.

*Örnek*:

Örneğin 1 GB boyutunda bir dosya alın. apr ask dosyası başka bir program tarafından oluşturuldu (Explorer.exe onu bir yerden kopyalıyor) ancak bu işlemin tamamlanması dakikalar alacak. Olay, oluşturma süresine yükseltilir ve dosyanın kopyalanmaya hazır olmasını beklemeniz gerekir.

Bu, dosyanın hazır olup olmadığını kontrol etmek için bir yöntemdir.



     public static bool IsFileReady(String sFilename)
    {
        // If the file can be opened for exclusive access it means that the file
        // is no longer locked by another process.
        try
        {
            using (FileStream inputStream = File.Open(sFilename, FileMode.Open, FileAccess.Read, FileShare.None))
            {
                if (inputStream.Length > 0)
                {
                    return true;
                }
                else
                {
                    return false;
                }

            }
        }
        catch (Exception)
        {
            return false;
        }
    }

## Temel Dosya İzleyici
Aşağıdaki örnek, çalışma zamanında belirtilen dizini izlemek için bir 'FileSystemWatcher' oluşturur. Bileşen, **LastWrite** ve **LastAccess** zamanındaki değişiklikleri, dizindeki metin dosyalarının oluşturulmasını, silinmesini veya yeniden adlandırılmasını izlemek üzere ayarlanmıştır. Bir dosya değiştirilir, oluşturulur veya silinirse dosyanın yolu konsola yazdırılır. Bir dosya yeniden adlandırıldığında, eski ve yeni yollar konsola yazdırılır.

Bu örnek için System.Diagnostics ve System.IO ad alanlarını kullanın.

    FileSystemWatcher watcher;

    private void watch()
    {
      // Create a new FileSystemWatcher and set its properties.
      watcher = new FileSystemWatcher();
      watcher.Path = path;

     /* Watch for changes in LastAccess and LastWrite times, and
           the renaming of files or directories. */
      watcher.NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite
                             | NotifyFilters.FileName | NotifyFilters.DirectoryName;

      // Only watch text files.      
      watcher.Filter = "*.txt*";

      // Add event handler.
      watcher.Changed += new FileSystemEventHandler(OnChanged);
      // Begin watching.      
      watcher.EnableRaisingEvents = true;
    }

    // Define the event handler.
    private void OnChanged(object source, FileSystemEventArgs e)
    {
      //Copies file to another directory or another action.
      Console.WriteLine("File: " +  e.FullPath + " " + e.ChangeType);
    }

