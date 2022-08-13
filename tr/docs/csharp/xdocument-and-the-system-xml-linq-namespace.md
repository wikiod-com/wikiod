---
title: "XDocument ve System.Xml.Linq ad alanı"
slug: "xdocument-ve-systemxmllinq-ad-alan"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Bir XML belgesi oluşturun
Amaç, aşağıdaki XML belgesini oluşturmaktır:

    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit ID="F0001">
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit ID="F0002">
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Kod:

    XNamespace xns = "http://www.fruitauthority.fake";
    XDeclaration xDeclaration = new XDeclaration("1.0", "utf-8", "yes");
    XDocument xDoc = new XDocument(xDeclaration);
    XElement xRoot = new XElement(xns + "FruitBasket");
    xDoc.Add(xRoot);
    
    XElement xelFruit1 = new XElement(xns + "Fruit");
    XAttribute idAttribute1 = new XAttribute("ID", "F0001");
    xelFruit1.Add(idAttribute1);
    XElement xelFruitName1 = new XElement(xns + "FruitName", "Banana");
    XElement xelFruitColor1 = new XElement(xns + "FruitColor", "Yellow");
    xelFruit1.Add(xelFruitName1);
    xelFruit1.Add(xelFruitColor1);
    xRoot.Add(xelFruit1);
    
    XElement xelFruit2 = new XElement(xns + "Fruit");
    XAttribute idAttribute2 = new XAttribute("ID", "F0002");
    xelFruit2.Add(idAttribute2);
    XElement xelFruitName2 = new XElement(xns + "FruitName", "Apple");
    XElement xelFruitColor2 = new XElement(xns + "FruitColor", "Red");
    xelFruit2.Add(xelFruitName2);
    xelFruit2.Add(xelFruitColor2);
    xRoot.Add(xelFruit2);


## XML Dosyasını Değiştir
Bir XML dosyasını 'XDocument' ile değiştirmek için, dosyayı 'XDocument' türünde bir değişkene yüklersiniz, onu bellekte değiştirirsiniz, sonra kaydedersiniz, orijinal dosyanın üzerine yazarsınız.
Yaygın bir hata, bellekteki XML'i değiştirmek ve diskteki dosyanın değişmesini beklemektir.

Bir XML dosyası verildiğinde:

    <?xml version="1.0" encoding="utf-8"?>
    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Muzun rengini kahverengi olarak değiştirmek istiyorsunuz:
1. Diskteki dosyanın yolunu bilmemiz gerekiyor.
2. Bir "XDocument.Load" aşırı yüklemesi bir URI (dosya yolu) alır.
3. xml dosyası bir ad alanı kullandığından, ad alanı AND öğe adıyla sorgulamalıyız.
4. Boş değerler olasılığını yerleştirmek için C# 6 sözdizimini kullanan bir Linq sorgusu. Bu sorguda kullanılan her `.`, koşul hiçbir öğe bulamazsa boş bir küme döndürme potansiyeline sahiptir. C# 6'dan önce bunu birden fazla adımda yapar ve yol boyunca null olup olmadığını kontrol ederdiniz. Sonuç, Muzu içeren "<Fruit>" öğesidir. Aslında bir 'IEnumerable<XElement>', bu nedenle bir sonraki adım 'FirstOfDefault()' kullanır.
5. Şimdi, az önce bulduğumuz Fruit öğesinden FruitColor öğesini çıkarıyoruz. Burada sadece bir tane olduğunu varsayıyoruz veya sadece ilkini önemsiyoruz.
6. Eğer null değilse, FruitColor'ı "Brown" olarak ayarlıyoruz.
7. Son olarak, diskteki orijinal dosyanın üzerine yazarak `XDocument` dosyasını kaydederiz.


    // 1.
    string xmlFilePath = "c:\\users\\public\\fruit.xml";
    
    // 2.
    XDocument xdoc = XDocument.Load(xmlFilePath);
    
    // 3.
    XNamespace ns = "http://www.fruitauthority.fake";
    
    //4. 
    var elBanana = xdoc.Descendants()?.
        Elements(ns + "FruitName")?.
        Where(x => x.Value == "Banana")?.
        Ancestors(ns + "Fruit");
    
    // 5.
    var elColor = elBanana.Elements(ns + "FruitColor").FirstOrDefault();
    
    // 6.
    if (elColor != null)
    {
        elColor.Value = "Brown";
    }
    
    // 7.
    xdoc.Save(xmlFilePath);

Dosya şimdi şöyle görünür:

    <?xml version="1.0" encoding="utf-8"?>
    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Brown</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>
















## Akıcı sözdizimi kullanarak bir XML belgesi oluşturun
Hedef:

    <FruitBasket xmlns="http://www.fruitauthority.fake">
      <Fruit>
        <FruitName>Banana</FruitName>
        <FruitColor>Yellow</FruitColor>
      </Fruit>
      <Fruit>
        <FruitName>Apple</FruitName>
        <FruitColor>Red</FruitColor>
      </Fruit>
    </FruitBasket>

Kod:

    XNamespace xns = "http://www.fruitauthority.fake";
    XDocument xDoc = 
        new XDocument(new XDeclaration("1.0", "utf-8", "yes"),
            new XElement(xns + "FruitBasket",
                new XElement(xns + "Fruit",
                    new XElement(xns + "FruitName", "Banana"),
                    new XElement(xns + "FruitColor", "Yellow")),
                new XElement(xns + "Fruit",
                    new XElement(xns + "FruitName", "Apple"),
                    new XElement(xns + "FruitColor", "Red"))
                    ));

