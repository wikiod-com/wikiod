---
title: "ObservableCollection<T>"
slug: "observablecollectiont"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## ObservableCollection'ı Başlat<T>
"ObservableCollection", "List<T>" gibi "T" türünde bir koleksiyondur, yani "T" türündeki nesneleri tutar.

Belgelerden şunu okuyoruz:

> "ObservableCollection", dinamik bir veri koleksiyonunu temsil eder.
> öğeler eklendiğinde, kaldırıldığında veya
> tüm liste yenilenir.

Diğer koleksiyonlardan temel farkı, 'ObservableCollection'ın 'INotifyCollectionChanged' ve 'INotifyPropertyChanged' arabirimlerini uygulaması ve yeni bir nesne eklendiğinde veya kaldırıldığında ve koleksiyon temizlendiğinde hemen bildirim olayını başlatmasıdır.

Bu özellikle, ek kod yazmak zorunda kalmadan bir uygulamanın kullanıcı arabirimini ve arka ucunu birbirine bağlamak için kullanışlıdır, çünkü gözlemlenebilir bir koleksiyona bir nesne eklendiğinde veya koleksiyondan bir nesne kaldırıldığında, kullanıcı arabirimi otomatik olarak güncellenir.

Kullanmak için ilk adım dahil etmektir

    using System.Collections.ObjectModel

Örneğin "string" türünde bir koleksiyonun boş bir örneğini oluşturabilirsiniz.

    ObservableCollection<string> collection = new ObservableCollection<string>();

veya verilerle dolu bir örnek

     ObservableCollection<string> collection = new ObservableCollection<string>()
     {
      "First_String", "Second_String"
     };

Tüm IList koleksiyonlarında olduğu gibi, dizinin 0'dan başladığını unutmayın ([IList<T>.Item Property][1]).


[1]: https://msdn.microsoft.com/en-us/library/ewthkb10(v=vs.110).aspx "IList&lt;T&gt;.Item Özelliği"

