---
title: "rehber"
slug: "rehber"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

GUID (veya UUID), 'Globally Unique Identifier' (veya 'Universally Unique Identifier') için bir kısaltmadır. Kaynakları tanımlamak için kullanılan 128 bitlik bir tam sayıdır.

"Kılavuzlar" *UUID*'ler, *Evrensel Olarak Benzersiz Tanımlayıcılar* olarak da bilinen *Global Benzersiz Tanımlayıcılardır*.

128 bitlik sözde rasgele değerlerdir. O kadar çok geçerli "Kılavuz" vardır (Dünyadaki her insanın her hücresi için yaklaşık 10^18 "Kılavuz") ki, eğer iyi bir sözde rastgele algoritma tarafından üretilirlerse, tüm pratikte tüm evrende benzersiz olarak kabul edilebilirler. anlamına geliyor.

`Guid`ler genellikle veritabanlarında birincil anahtar olarak kullanılır. Avantajları, (neredeyse) benzersiz olduğu garanti edilen yeni bir kimlik almak için veritabanını aramanız gerekmemesidir.

## Bir Kılavuzun dize temsilini alma
Yerleşik 'ToString' yöntemi kullanılarak bir Guid'in dize temsili elde edilebilir.

    string myGuidString = myGuid.ToString();

İhtiyaçlarınıza bağlı olarak, `ToString` çağrısına bir format tipi argümanı ekleyerek Guid'i de formatlayabilirsiniz.

    var guid = new Guid("7febf16f-651b-43b0-a5e3-0da8da49e90d");

    // None          "7febf16f651b43b0a5e30da8da49e90d"
    Console.WriteLine(guid.ToString("N"));

    // Hyphens       "7febf16f-651b-43b0-a5e3-0da8da49e90d"
    Console.WriteLine(guid.ToString("D"));

    // Braces        "{7febf16f-651b-43b0-a5e3-0da8da49e90d}"
    Console.WriteLine(guid.ToString("B"));

    // Parentheses   "(7febf16f-651b-43b0-a5e3-0da8da49e90d)"
    Console.WriteLine(guid.ToString("P"));

    // Hex           "{0x7febf16f,0x651b,0x43b0{0xa5,0xe3,0x0d,0xa8,0xda,0x49,0xe9,0x0d}}"
    Console.WriteLine(guid.ToString("X"));


## Kılavuz Oluşturma
Bir Guid örneği oluşturmanın en yaygın yolları şunlardır:

- Boş bir kılavuz oluşturma (`00000000-0000-0000-0000-000000000000`):


    Guid g = Guid.Empty;
    Guid g2 = new Guid();

- Yeni (sözde) bir Kılavuz oluşturma:


    Guid g = Guid.NewGuid();

- Belirli bir değere sahip Kılavuzlar Oluşturma:


    Guid g = new Guid("0b214de7-8958-4956-8eed-28f9ba2c47c6");
    Guid g2 = new Guid("0b214de7895849568eed28f9ba2c47c6");
    Guid g3 = Guid.Parse("0b214de7-8958-4956-8eed-28f9ba2c47c6");


## Null yapılabilir bir GUID bildirme
Diğer değer türleri gibi, GUID de boş değer alabilen boş bir türe sahiptir.

Beyanname :

    Guid? myGuidVar = null;

Bu, özellikle bir tablodaki değerin NULL olma olasılığı olduğunda veri tabanından veri alırken kullanışlıdır.

