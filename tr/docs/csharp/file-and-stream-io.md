---
title: "Dosya ve Akış GÇ"
slug: "dosya-ve-aks-gc"
draft: false
images: []
weight: 9903
type: docs
toc: true
---

Dosyaları yönetir.

## Sözdizimi
- `yeni System.IO.StreamWriter(dize yolu)`
- `yeni System.IO.StreamWriter(dize yolu, bool ekleme)`
- `System.IO.StreamWriter.WriteLine(dize metni)`
- `System.IO.StreamWriter.WriteAsync(dize metni)`
- `System.IO.Stream.Close()`
- `System.IO.File.ReadAllText(dize yolu)`
- `System.IO.File.ReadAllLines(dize yolu)`
- `System.IO.File.ReadLines(dize yolu)`
- `System.IO.File.WriteAllText(dize yolu, dize metni)`
- `System.IO.File.WriteAllLines(dize yolu, IEnumerable<string> içeriği)`
- `System.IO.File.Copy(dize kaynağı, dize hedefi)`
- `System.IO.File.Create(dize yolu)`
- `System.IO.File.Delete(dize yolu)`
- `System.IO.File.Move(dize kaynağı, dize hedefi)`
- `System.IO.Directory.GetFiles(dize yolu)`

## Parametreler
| parametre | Ayrıntılar |
| ------ | ------ |
| yol | Dosyanın konumu. |
| ekle | Dosya varsa, true dosyanın sonuna veri ekler (ekler), false dosyanın üzerine yazar. |
| metin | Yazılacak veya saklanacak metin. |
|içerik | Yazılacak dizeler topluluğu. |
| kaynak | Kullanmak istediğiniz dosyanın konumu. |
| hedef | Bir dosyanın gitmesini istediğiniz konum. |

- Her zaman "Akış" nesnelerini kapattığınızdan emin olun. Bu, yukarıda gösterildiği gibi bir "kullanma" bloğu ile veya elle "myStream.Close()" çağrılarak yapılabilir.
- Dosyayı oluşturmaya çalıştığınız yolda mevcut kullanıcının gerekli izinlere sahip olduğundan emin olun.
- Ters eğik çizgi içeren bir yol dizesi bildirirken [Verbatim dizeleri](https://www.wikiod.com/tr/docs/c%23/16/verbatim-strings) kullanılmalıdır: `@"C:\MyFolder\MyFile .txt"`

## System.IO.File sınıfını kullanarak bir dosyadan okuma
**[System.IO.File.ReadAllText](https://msdn.microsoft.com/en-us/library/system.io.file.readalltext(v=vs.110).aspx)* dosyasını kullanabilirsiniz. * bir dosyanın tüm içeriğini bir dizgeye okuma işlevi.

    string text = System.IO.File.ReadAllText(@"C:\MyFolder\MyTextFile.txt");

Ayrıca bir dosyayı **[System.IO.File.ReadAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.readlines(v) kullanarak satır dizisi olarak da okuyabilirsiniz. =vs.110).aspx)** işlevi:
    
    string[] lines = System.IO.File.ReadAllLines(@"C:\MyFolder\MyTextFile.txt");

## Bir IEnumerable aracılığıyla bir dosyayı tembelce satır satır okuma
Büyük dosyalarla çalışırken, bir dosyadaki tüm satırları bir IEnumerable<string>'e okumak için 'System.IO.File.ReadLines' yöntemini kullanabilirsiniz. Bu, tüm dosyayı bir kerede belleğe yüklememesi ve büyük dosyalarla çalışırken daha verimli hale getirmesi dışında `System.IO.File.ReadAllLines` ile benzerdir.

    IEnumerable<string> AllLines = File.ReadLines("file_name.txt", Encoding.Default);

_File.ReadLines'ın ikinci parametresi isteğe bağlıdır. Kodlamayı belirtmeniz gerektiğinde kullanabilirsiniz._

'ToArray', 'ToList' veya başka bir benzer işlevin çağrılmasının, tüm satırları bir kerede yüklenmeye zorlayacağını, yani 'ReadLines' kullanmanın yararının geçersiz olduğunu belirtmek önemlidir. Bu yöntemi kullanıyorsanız, bir "foreach" döngüsü veya LINQ kullanarak "IEnumerable" üzerinde numaralandırmak en iyisidir.


## System.IO.StreamWriter sınıfını kullanarak bir dosyaya satır yazma
**[System.IO.StreamWriter](https://msdn.microsoft.com/en-us/library/system.io.streamwriter(v=vs.110).aspx)** sınıfı:
>Belirli bir kodlamada bir akışa karakter yazmak için bir TextWriter uygular.

'WriteLine' yöntemini kullanarak içeriği bir dosyaya satır satır yazabilirsiniz.

StreamWriter nesnesinin kapsam dışına çıkar çıkmaz atılmasını ve böylece dosyanın kapanmasını sağlayan 'using' anahtar sözcüğünün kullanımına dikkat edin.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt"))
    {
        foreach (string line in lines)
        {
            sw.WriteLine(line);
        }
    }

StreamWriter'ın yapıcısında ikinci bir "bool" parametresi alabileceğini ve dosyanın üzerine yazmak yerine bir dosyaya "Ekle"ye izin verebileceğini unutmayın:

    bool appendExistingFile = true;
    using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt", appendExistingFile ))
    {
        sw.WriteLine("This line will be appended to the existing file");
    }

## System.IO.File sınıfını kullanarak bir dosyaya yazma
**[System.IO.File.WriteAllText](https://msdn.microsoft.com/en-us/library/system.io.file.writealltext(v=vs.110).aspx)* dosyasını kullanabilirsiniz. * bir dosyaya dize yazma işlevi.

    string text = "String that will be stored in the file";
    System.IO.File.WriteAllText(@"C:\MyFolder\OutputFile.txt", text);

Ayrıca **[System.IO.File.WriteAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.writealllines(v=vs.110).aspx) öğesini de kullanabilirsiniz. ** ikinci parametre olarak bir "IEnumerable<String>" alan işlev (önceki örnekte tek bir dizenin aksine). Bu, bir dizi satırdan içerik yazmanıza olanak tanır.

    string[] lines = { "My first string", "My second string", "and even a third string" };
    System.IO.File.WriteAllLines(@"C:\MyFolder\OutputFile.txt", lines);

## Dosya kopyala
**Dosya statik sınıfı**

`File` statik sınıfı bu amaç için kolaylıkla kullanılabilir.

    File.Copy(@"sourcePath\abc.txt", @"destinationPath\abc.txt");
    File.Copy(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Açıklama:** Bu yöntemle dosya kopyalanır, yani kaynaktan okunur ve ardından hedef yola yazılır. Bu kaynak tüketen bir işlemdir, dosya boyutuna göre zaman alır ve iş parçacıkları kullanmazsanız programınızın donmasına neden olabilir.

## StreamWriter kullanarak bir dosyaya zaman uyumsuz metin yazma


## Dosya oluştur
**Dosya statik sınıfı**

'File' statik sınıfının 'Create' yöntemini kullanarak dosyalar oluşturabiliriz. Method, dosyayı verilen yolda oluşturur, aynı zamanda dosyayı açar ve bize dosyanın 'FileStream'ini verir. İşiniz bittiğinde dosyayı kapattığınızdan emin olun.

ex1:

    var fileStream1 = File.Create("samplePath");
    /// you can write to the fileStream1
    fileStream1.Close();

ex2:

    using(var fileStream1 = File.Create("samplePath"))
    {
        /// you can write to the fileStream1
    }

ex3:

    File.Create("samplePath").Close();

**FileStream sınıfı**

[burada][1] gerçekten iyi belgelenmiş olan bu sınıf yapıcısının birçok aşırı yüklemesi vardır. Aşağıdaki örnek, bu sınıfın en çok kullanılan işlevlerini kapsayan örnek içindir.

    var fileStream2 = new FileStream("samplePath", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);

[FileMode][2], [FileAccess][3] ve [FileShare][4] için numaralandırmaları bu bağlantılardan kontrol edebilirsiniz. Temel olarak ne anlama geldikleri şunlardır:

*FileMode:* Yanıtlar "Dosya oluşturulmalı mı? Açılmalı mı? Eğer yoksa oluştur, sonra aç?" tür sorular.

*FileAccess:* Yanıtlar "Dosyayı okuyabilir miyim, dosyaya yazabilir miyim yoksa her ikisini birden mi yapmalıyım?" tür sorular.

*FileShare:* Cevapları "Ben aynı anda kullanırken diğer kullanıcılar dosyayı okuyabilmeli, yazabilmeli mi?" tür sorular.


[1]: https://msdn.microsoft.com/en-us/library/system.io.filestream(v=vs.110).aspx
[2]: https://msdn.microsoft.com/en-us/library/system.io.filemode(v=vs.110).aspx
[3]: https://msdn.microsoft.com/en-us/library/4z36sx0f(v=vs.110).aspx
[4]: https://msdn.microsoft.com/en-us/library/system.io.fileshare(v=vs.110).aspx

## Dosyayı taşı
**Dosya statik sınıfı**

File static sınıfı bu amaç için kolaylıkla kullanılabilir.

    File.Move(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

**Açıklama1:** Yalnızca dosyanın dizinini değiştirir (dosya aynı birimde taşınırsa). Bu işlem, dosya boyutuna göreli zaman almaz.

**Açıklama2:** Varolan bir dosya hedef yolda geçersiz kılınamaz.

## Dosyayı sil
    string path = @"c:\path\to\file.txt";
    File.Delete(path);

Dosya yoksa "Sil" istisna oluşturmazken, örneğin istisna atar. belirtilen yol geçersizse veya arayan gerekli izinlere sahip değilse. Çağrıları her zaman [try-catch bloğu][1] içindeki "Sil"e sarmalı ve beklenen tüm istisnaları ele almalısınız. Olası yarış koşulları durumunda, mantığı [kilit ifadesi][2] içine sarın.


[1]: https://www.wikiod.com/tr/docs/c%23/26/keywords/148/try-catch-finally-throw#t=201608021340222162938
[2]: https://www.wikiod.com/tr/docs/c%23/1495/lock-statement/4865/simple-usage#t=201608021343504970522

## Dosyalar ve Dizinler
**Dizin içindeki tüm dosyaları alın**
    
     var FileSearchRes = Directory.GetFiles(@Path, "*.*", SearchOption.AllDirectories);

Belirtilen dizindeki tüm dosyaları temsil eden bir "FileInfo" dizisini döndürür.

**Belirli uzantıya sahip Dosyaları Alın**

     var FileSearchRes = Directory.GetFiles(@Path, "*.pdf", SearchOption.AllDirectories);

Belirtilen dizindeki tüm dosyaları belirtilen uzantıyla temsil eden bir "FileInfo" dizisini döndürür.

