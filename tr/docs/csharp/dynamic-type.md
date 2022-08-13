---
title: "dinamik tip"
slug: "dinamik-tip"
draft: false
images: []
weight: 9934
type: docs
toc: true
---

'Dynamic' anahtar sözcüğü, derleme zamanında türü bilinmeyen bir değişken bildirir. Bir "dinamik" değişken herhangi bir değeri içerebilir ve değerin türü çalışma zamanı sırasında değişebilir.

".NET'te Metaprogramlama" kitabında belirtildiği gibi, C# 'dinamik' anahtar kelime için bir destek tipine sahip değildir:

> "Dynamic" anahtar sözcüğü tarafından etkinleştirilen işlevsellik, yerel yürütme kapsamının site kapsayıcısında "CallSite" nesnelerini yayan ve kullanan akıllı bir derleyici eylemleri kümesidir. Derleyici, programcıların dinamik nesne olarak algıladıkları şeyi yönetir.
bu "CallSite" örnekleri aracılığıyla referanslar. Derleme zamanında dinamik işlem gören parametreler, dönüş türleri, alanlar ve özellikler, dinamik kullanım için oluşturulduklarını belirtmek için bazı meta verilerle işaretlenebilir, ancak bunlar için temel veri türü her zaman 'System.Object' olacaktır.

 

## Özelliklerle dinamik bir nesne oluşturma
    using System;
    using System.Dynamic;
    
    dynamic info = new ExpandoObject();
    info.Id = 123;
    info.Another = 456;
    
    Console.WriteLine(info.Another);
    // 456
    
    Console.WriteLine(info.DoesntExist);
    // Throws RuntimeBinderException

## Dinamik bir değişken oluşturma
    dynamic foo = 123;
    Console.WriteLine(foo + 234);
    // 357    Console.WriteLine(foo.ToUpper())
    // RuntimeBinderException, since int doesn't have a ToUpper method

    foo = "123";
    Console.WriteLine(foo + 234);
    // 123234
    Console.WriteLine(foo.ToUpper()):
    // NOW A STRING

## Dönen dinamik
    using System;

    public static void Main()
    {
        var value = GetValue();
        Console.WriteLine(value);
        // dynamics are useful!
    }
    
    private static dynamic GetValue()
    {
        return "dynamics are useful!";
    }

## Derleme Zamanında Bilinmeyen Belirli Türleri İşleme


