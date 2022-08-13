---
title: "C# ile Nesne Yönelimli Programlama"
slug: "c-ile-nesne-yonelimli-programlama"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Bu konu bize OOP yaklaşımına göre nasıl program yazabileceğimizi anlatmaya çalışıyor. Ama biz Nesne Yönelimli Programlama paradigmasını öğretmeye çalışmıyoruz.
Aşağıdaki konuları ele alacağız:
Sınıflar, Özellikler, Kalıtım, Polimorfizm, Arayüzler vb.

## Sınıflar:
Sınıf bildiren iskelet:

<>:Gerekli

[]:İsteğe bağlı

    [private/public/protected/internal] class <Desired Class Name> [:[Inherited class][,][[Interface Name 1],[Interface Name 2],...]
    {
        //Your code
    }
Tüm sözdizimini anlayamıyorsanız endişelenmeyin, bunun tüm bölümlerine aşina olacağız. İlk örnek için aşağıdaki sınıfı düşünün:

    class MyClass
    {
        int i = 100;
        public void getMyValue()
        {
            Console.WriteLine(this.i);//Will print number 100 in output
        }
    }

bu sınıfta 'int' tipinde ve varsayılan özel [Erişim Değiştiricileri](https://msdn.microsoft.com/en-us/library/ms173121.aspx) ve 'getMyValue()' yöntemiyle 'i' değişkeni oluşturuyoruz genel erişim değiştiricileri ile.

