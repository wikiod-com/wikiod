---
title: "Bir Birlik türü oluşturmak için C# Yapıları nasıl kullanılır (C Birliklerine benzer)"
slug: "bir-birlik-turu-olusturmak-icin-c-yaplar-nasl-kullanlr-c-birliklerine-benzer"
draft: false
images: []
weight: 9950
type: docs
toc: true
---

Birleşim türleri, aynı bellek alanında "örtüşebilen" birkaç farklı türü içermek için, özellikle C dilinde olmak üzere birçok dilde kullanılır. Başka bir deyişle, farklı uzunluklara ve türlere sahip olsalar bile hepsi aynı bellek ofsetinde başlayan farklı alanlar içerebilirler. Bu, hem bellekten tasarruf etme hem de otomatik dönüştürme yapma avantajına sahiptir.

Lütfen, Struct'un yapıcısındaki yorumları not edin. Alanların başlatıldığı sıra son derece önemlidir. Önce diğer tüm alanları başlatmak ve ardından değiştirmek istediğiniz değeri son ifade olarak ayarlamak istiyorsunuz. Alanlar çakıştığı için, önemli olan son değer ayarıdır.

## C#'da C-Tarzı Birleştirmeler
Birleşim türleri, "örtüşebilen" birkaç farklı türü içermek için C dili gibi çeşitli dillerde kullanılır. Başka bir deyişle, farklı uzunluklara ve türlere sahip olsalar bile hepsi aynı bellek ofsetinde başlayan farklı alanlar içerebilirler. Bu, hem bellekten tasarruf etme hem de otomatik dönüştürme yapma avantajına sahiptir. Örnek olarak bir IP adresi düşünün. Dahili olarak, bir IP adresi bir tamsayı olarak temsil edilir, ancak bazen Byte1.Byte2.Byte3.Byte4'te olduğu gibi farklı Byte bileşenine erişmek isteriz. Bu, Int32 veya long gibi ilkel değerler veya sizin tanımladığınız diğer yapılar olsun, herhangi bir değer türü için çalışır.

Aynı etkiyi Explicit Layout Structs kullanarak C#'ta da elde edebiliriz.

    using System;
    using System.Runtime.InteropServices;

    // The struct needs to be annotated as "Explicit Layout"
    [StructLayout(LayoutKind.Explicit)]
    struct IpAddress
    {
        // The "FieldOffset" means that this Integer starts, an offset in bytes.
        // sizeof(int) 4, sizeof(byte) = 1
        [FieldOffset(0)] public int Address;
        [FieldOffset(0)] public byte Byte1;
        [FieldOffset(1)] public byte Byte2;
        [FieldOffset(2)] public byte Byte3;
        [FieldOffset(3)] public byte Byte4;

        public IpAddress(int address) : this()
        {
            // When we init the Int, the Bytes will change too.
            Address = address;
        }

        // Now we can use the explicit layout to access the 
        // bytes separately, without doing any conversion.
        public override string ToString() => $"{Byte1}.{Byte2}.{Byte3}.{Byte4}";
    }
    
Struct'u bu şekilde tanımladıktan sonra, istediğimiz gibi kullanabiliriz.
C'de bir Union kullanın. Örneğin, bir IP adresi oluşturalım.
Rastgele Tamsayı ve ardından adresteki ilk belirteci değiştirin
'A.B.C.D' yerine '100.B.C.D' olarak değiştirerek '100'e:
                     
    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"{ip} = {ip.Address}");
    ip.Byte1 = 100;
    Console.WriteLine($"{ip} = {ip.Address}");

Çıktı:

    75.49.5.32 = 537211211
    100.49.5.32 = 537211236

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/CnrgBi

## C#'daki Birlik Türleri Yapı alanları da içerebilir
C#'daki Explicit Layout yapıları (Unions) ilkellerin yanı sıra başka Struct'ları da içerebilir. Bir alan, Referans değil, Değer türü olduğu sürece, Bir Birlik içinde yer alabilir:

    using System;
    using System.Runtime.InteropServices;

    // The struct needs to be annotated as "Explicit Layout"
    [StructLayout(LayoutKind.Explicit)]
    struct IpAddress
    {
        // Same definition of IpAddress, from the example above
    }

    // Now let's see if we can fit a whole URL into a long

    // Let's define a short enum to hold protocols
    enum Protocol : short { Http, Https, Ftp, Sftp, Tcp }

    // The Service struct will hold the Address, the Port and the Protocol
    [StructLayout(LayoutKind.Explicit)]
    struct Service
    {
        [FieldOffset(0)] public IpAddress Address;
        [FieldOffset(4)] public ushort Port;
        [FieldOffset(6)] public Protocol AppProtocol;
        [FieldOffset(0)] public long Payload;

        public Service(IpAddress address, ushort port, Protocol protocol)
        {
            Payload = 0;
            Address = address;
            Port  = port;
            AppProtocol = protocol;
        }

        public Service(long payload)
        {
            Address = new IpAddress(0);
            Port = 80;
            AppProtocol = Protocol.Http;
            Payload = payload;
        }

        public Service Copy() => new Service(Payload);

        public override string ToString() => $"{AppProtocol}//{Address}:{Port}/";
    }
    
Artık tüm Hizmet Birliğinin uzun (8 bayt) boyutuna sığdığını doğrulayabiliriz.

    var ip = new IpAddress(new Random().Next());
    Console.WriteLine($"Size: {Marshal.SizeOf(ip)} bytes. Value: {ip.Address} = {ip}.");

    var s1 = new Service(ip, 8080, Protocol.Https);
    var s2 = new Service(s1.Payload);
    s2.Address.Byte1 = 100;
    s2.AppProtocol = Protocol.Ftp;

    Console.WriteLine($"Size: {Marshal.SizeOf(s1)} bytes. Value: {s1.Address} = {s1}.");
    Console.WriteLine($"Size: {Marshal.SizeOf(s2)} bytes. Value: {s2.Address} = {s2}.");

[Demoyu Görüntüle][1]


[1]: https://dotnetfiddle.net/cROlki

