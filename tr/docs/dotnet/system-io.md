---
title: "Sistem.IO"
slug: "sistemio"
draft: false
images: []
weight: 9957
type: docs
toc: true
---

## StreamReader kullanarak bir metin dosyasını okuma


## System.IO.SerialPorts kullanan Seri Bağlantı Noktaları
Bağlı seri bağlantı noktaları üzerinde yineleme
-------------------------------------

    using System.IO.Ports;
    string[] ports = SerialPort.GetPortNames();
    for (int i = 0; i < ports.Length; i++)
    {
        Console.WriteLine(ports[i]);
    }

----------


System.IO.SerialPort nesnesini başlatma
-------------------------------------------

    using System.IO.Ports;
    SerialPort port = new SerialPort();
    SerialPort port = new SerialPort("COM 1"); ;
    SerialPort port = new SerialPort("COM 1", 9600);
**NOT**: Bunlar, SerialPort türü için oluşturucunun yedi aşırı yüklemesinden yalnızca üçüdür.

----------

SerialPort üzerinden veri okuma/yazma
-------------------------------------------

En basit yol, 'SerialPort.Read' ve 'SerialPort.Write' yöntemlerini kullanmaktır.
Ancak, SerialPort üzerinden veri akışı yapmak için kullanabileceğiniz bir `System.IO.Stream` nesnesi de alabilirsiniz. Bunu yapmak için `SerialPort.BaseStream` kullanın.

**Okuma**

    int length = port.BytesToRead;
    //Note that you can swap out a byte-array for a char-array if you prefer.
    byte[] buffer = new byte[length];
    port.Read(buffer, 0, length);

Ayrıca mevcut tüm verileri okuyabilirsiniz:

    string curData = port.ReadExisting();

Veya gelen verilerde karşılaşılan ilk yeni satırı okuyun:

    string line = port.ReadLine();

**Yazı**

SerialPort üzerinden veri yazmanın en kolay yolu şudur:

    port.Write("here is some text to be sent over the serial port.");

Ancak, gerektiğinde bu şekilde verileri de gönderebilirsiniz:

    //Note that you can swap out the byte-array with a char-array if you so choose.
    byte[] data = new byte[1] { 255 };
    port.Write(data, 0, data.Length);

## System.IO.File Kullanarak Veri Okuma/Yazma
İlk olarak, bir dosyadan veri çıkarmanın üç farklı yolunu görelim.

    string fileText = File.ReadAllText(file);
    string[] fileLines = File.ReadAllLines(file);
    byte[] fileBytes = File.ReadAllBytes(file);

- İlk satırda dosyadaki tüm verileri string olarak okuyoruz.
- İkinci satırda dosyadaki verileri string-array olarak okuyoruz.
Dosyadaki her satır, dizide bir öğe haline gelir.
- Üçüncüsünde dosyadan baytları okuruz.

----------

Şimdi, bir dosyaya veri **eklemenin** üç farklı yöntemini görelim.
Belirttiğiniz dosya mevcut değilse, verileri eklemeye çalışmadan önce her yöntem dosyayı otomatik olarak oluşturur.

     File.AppendAllText(file, "Here is some data that is\nappended to the file.");
     File.AppendAllLines(file, new string[2] { "Here is some data that is", "appended to the file." });
     using (StreamWriter stream = File.AppendText(file))
     {
         stream.WriteLine("Here is some data that is");
         stream.Write("appended to the file.");
     }

- İlk satırda sadece belirtilen dosyanın sonuna bir string ekliyoruz.
- İkinci satırda dizinin her elemanını dosyadaki yeni bir satıra ekliyoruz.
- Son olarak üçüncü satırda, kendisine yazılan her türlü veriyi ekleyecek bir akış yazarı açmak için `File.AppendText` kullanıyoruz.
----------
Ve son olarak, bir dosyaya **veri yazmanın** üç farklı yöntemini görelim.
*ekleme* ve *yazma* arasındaki fark, yazmanın dosyadaki verileri **üzerine yazması**, eklerken dosyadaki verilere **ekleme** olmasıdır.
Belirttiğiniz dosya mevcut değilse, her yöntem, verileri dosyaya yazmaya çalışmadan önce dosyayı otomatik olarak oluşturacaktır.

    File.WriteAllText(file, "here is some data\nin this file.");
    File.WriteAllLines(file, new string[2] { "here is some data", "in this file" });
    File.WriteAllBytes(file, new byte[2] { 0, 255 });
- İlk satır dosyaya bir dize yazar.
- İkinci satır, dizideki her dizeyi dosyadaki kendi satırına yazar.
- Ve üçüncü satır, dosyaya bir bayt dizisi yazmanıza izin verir.

