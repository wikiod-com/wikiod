---
title: "Aktarım"
slug: "aktarm"
draft: false
images: []
weight: 9952
type: docs
toc: true
---

## Akışları Kullanma
Akış, veri aktarımı için düşük seviyeli bir araç sağlayan bir nesnedir. Kendileri veri kapsayıcıları olarak hareket etmezler.

Uğraştığımız veriler bayt dizisi (`byte []`) biçimindedir. Okuma ve yazma işlevlerinin tümü bayt yönelimlidir, örn. `WriteByte()`.

Tamsayılar, dizgiler vb. ile uğraşmak için hiçbir işlev yoktur. Bu, akışı çok genel amaçlı yapar, ancak örneğin yalnızca metin aktarmak istiyorsanız, üzerinde çalışmak daha az basittir. Akışlar, özellikle büyük miktarda veriyle uğraşırken çok yardımcı olabilir.

Yazılması/okunması gereken yere (yani destek deposu) bağlı olarak farklı türde Akış kullanmamız gerekecek. Örneğin, kaynak bir dosyaysa, `FileStream` kullanmamız gerekir:

    string filePath = @"c:\Users\exampleuser\Documents\userinputlog.txt";
    using (FileStream fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
    {
        // do stuff here...
    
        fs.Close();
    }

Benzer şekilde, yedekleme deposu bellek ise "MemoryStream" kullanılır:

    // Read all bytes in from a file on the disk.
    byte[] file = File.ReadAllBytes(“C:\\file.txt”);

    // Create a memory stream from those bytes.
    using (MemoryStream memory = new MemoryStream(file))
    {
       // do stuff here...
    }

Benzer şekilde, ağ erişimi için `System.Net.Sockets.NetworkStream` kullanılır.

Tüm Akışlar, 'System.IO.Stream' genel sınıfından türetilir. Veriler akışlardan doğrudan okunamaz veya yazılamaz. .NET Framework, yerel türler ile düşük düzeyli akış arabirimi arasında dönüştürme yapan ve sizin için verileri akışa veya akıştan aktaran 'StreamReader', 'StreamWriter', 'BinaryReader' ve 'BinaryWriter' gibi yardımcı sınıflar sağlar.

Akış okuma ve yazma işlemleri 'StreamReader' ve 'StreamWriter' aracılığıyla yapılabilir. Bunları kapatırken dikkatli olunmalıdır. Varsayılan olarak, kapatma, içerilen akışı da kapatacak ve daha sonraki kullanımlar için kullanılamaz hale getirecektir. Bu varsayılan davranış, "bool LeaveOpen" parametresine sahip bir [constructor][1] kullanılarak ve değeri "true" olarak ayarlanarak değiştirilebilir.


"Akış Yazarı":

    FileStream fs = new FileStream("sample.txt", FileMode.Create);
    StreamWriter sw = new StreamWriter(fs);
    string NextLine = "This is the appended line.";
    sw.Write(NextLine);
    sw.Close();
    //fs.Close(); There is no need to close fs. Closing sw will also close the stream it contains.

'AkışOkuyucu':

    using (var ms = new MemoryStream())
    {
        StreamWriter sw = new StreamWriter(ms);
        sw.Write(123);
        //sw.Close();     This will close ms and when we try to use ms later it will cause an exception
        sw.Flush();     //You can send the remaining data to stream. Closing will do this automatically
        // We need to set the position to 0 in order to read 
        // from the beginning.
        ms.Position = 0;
        StreamReader sr = new StreamReader(ms);
        var myStr = sr.ReadToEnd();
        sr.Close();
        ms.Close();
    }

'Stream', 'StreamReader', 'StreamWriter' vb. Sınıflar 'IDisposable' arabirimini uyguladığından, bu sınıfların nesneleri üzerinde 'Dispose()' yöntemini çağırabiliriz.


[1]: https://msdn.microsoft.com/en-us/library/gg712952(v=vs.110).aspx

