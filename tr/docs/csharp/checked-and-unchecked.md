---
title: "İşaretli ve İşaretsiz"
slug: "isaretli-ve-isaretsiz"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

## Sözdizimi
- kontrol edildi(a + b) // kontrol edilen ifade
- işaretlenmemiş(a + b) // işaretlenmemiş ifade
- kontrol edildi { c = a + b; c += 5; } // kontrol edilen blok
- işaretlenmemiş { c = a + b; c += 5; } // işaretlenmemiş blok

## İşaretlendi ve İşaretlenmemiş
C# deyimleri, işaretli veya işaretlenmemiş bağlamda yürütülür. Kontrol edilen bir bağlamda, aritmetik taşma bir istisna oluşturur. Denetlenmeyen bir bağlamda, aritmetik taşma yok sayılır ve sonuç kesilir.

    short m = 32767;   
    short n = 32767;
    int result1 =  checked((short)(m + n));   //will throw an OverflowException
    int result2 =  unchecked((short)(m + n)); // will return -2

Bunlardan hiçbiri belirtilmezse, varsayılan bağlam, derleyici seçenekleri gibi diğer faktörlere bağlı olacaktır.

## Kapsam olarak İşaretli ve İşaretsiz
Anahtar sözcükler, birden çok işlemin işaretini (kontrolünü kaldırmak) için kapsamlar da oluşturabilir.

    short m = 32767;
    short n = 32767;
    checked
    {
        int result1 = (short)(m + n); //will throw an OverflowException
    }
    unchecked
    {
        int result2 = (short)(m + n); // will return -2
    }

