---
title: "Değişmezlik"
slug: "degismezlik"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## System.String sınıfı


## Dizeler ve değişmezlik
Değişmez türler, değiştirildiğinde bellekte var olan nesneyi değiştirmek yerine bellekte nesnenin yeni bir sürümünü oluşturan türlerdir. Bunun en basit örneği, yerleşik "string" türüdür.

"Merhaba" kelimesinin üzerine "dünya" ekleyen aşağıdaki kodu alarak

    string myString = "hello";
    myString += " world";

Bu durumda bellekte olan şey, ikinci satırdaki `string`e eklediğinizde yeni bir nesnenin yaratılmasıdır. Bunu büyük bir döngünün parçası olarak yaparsanız, bunun uygulamanızda performans sorunlarına neden olma potansiyeli vardır.

Bir "dize"nin değişken eşdeğeri bir "StringBuilder"dır

Aşağıdaki kodu alarak

    StringBuilder myStringBuilder = new StringBuilder("hello");
    myStringBuilder.append(" world");

Bunu çalıştırdığınızda, bellekte `StringBuilder` nesnesinin kendisini değiştiriyorsunuz.

