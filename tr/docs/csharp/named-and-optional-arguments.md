---
title: "Adlandırılmış ve İsteğe Bağlı Bağımsız Değişkenler"
slug: "adlandrlms-ve-istege-bagl-bagmsz-degiskenler"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

**Adlandırılmış Argümanlar**

*Ref: MSDN* Adlandırılmış bağımsız değişkenler, bağımsız değişkeni parametre listesindeki konumu yerine parametrenin adıyla ilişkilendirerek belirli bir parametre için bir bağımsız değişken belirtmenizi sağlar.

MSDN tarafından söylendiği gibi, Adlandırılmış bir argüman,

- Argümanı işleve ilişkilendirerek iletmenizi sağlar.
parametrenin adı.
- Olmadığımız parametrelerin konumunu hatırlamaya gerek yok
her zaman farkında.
- Parametre listesindeki parametrelerin sırasına bakmanıza gerek yok
fonksiyon denir.
- Adına göre her argüman için parametre belirleyebiliriz.

**İsteğe Bağlı Argümanlar**

*Ref: MSDN* Bir yöntemin, yapıcının, dizin oluşturucunun veya temsilcinin tanımı, parametrelerinin gerekli olduğunu veya isteğe bağlı olduğunu belirtebilir. Herhangi bir çağrı, gerekli tüm parametreler için bağımsız değişkenler sağlamalıdır, ancak isteğe bağlı parametreler için bağımsız değişkenler atlayabilir.

İsteğe Bağlı Bir Argüman olan MSDN tarafından söylendiği gibi,

- Eğer bu argüman bir İsteğe Bağlı ise, çağrıdaki argümanı atlayabiliriz.
Argüman
- Her İsteğe Bağlı Argümanın kendi varsayılan değeri vardır
- Değeri sağlamazsak varsayılan değer alır
- İsteğe Bağlı Bağımsız Değişkenin varsayılan değeri,
- Sabit ifade.
- Enum veya struct gibi bir değer türü olmalıdır.
- default(valueType) formunun bir ifadesi olmalıdır
- Parametre listesinin sonunda ayarlanmalıdır.

## İsteğe Bağlı Bağımsız Değişkenler
Önceki, isteğe bağlı argümanlarla işlev tanımımızdır.

    private static double FindAreaWithOptional(int length, int width=56)
           {
               try
               {
                   return (length * width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Burada genişlik değerini isteğe bağlı olarak belirledik ve 56 değerini verdik. Not ederseniz, IntelliSense'in kendisi aşağıdaki resimde gösterildiği gibi isteğe bağlı argümanı size gösterir.

[![buraya resim açıklamasını girin][1]][1]

    Console.WriteLine("Area with Optional Argument : ");
    area = FindAreaWithOptional(120);
    Console.WriteLine(area);
    Console.Read();

Derleme sırasında herhangi bir hata almadığımızı ve size aşağıdaki gibi bir çıktı vereceğini unutmayın.

[![buraya resim açıklamasını girin][2]][2]



**İsteğe Bağlı Özelliği Kullanma.**

İsteğe bağlı bağımsız değişkeni uygulamanın başka bir yolu da "[Opsiyonel]" anahtar sözcüğünü kullanmaktır. İsteğe bağlı bağımsız değişkenin değerini iletmezseniz, bu veri türünün varsayılan değeri bu bağımsız değişkene atanır. "Optional" anahtar sözcüğü, "Runtime.InteropServices" ad alanında bulunur.

    using System.Runtime.InteropServices;  
    private static double FindAreaWithOptional(int length, [Optional]int width)
       {
           try
           {
               return (length * width);
           }
           catch (Exception)
           {
               throw new NotImplementedException();
           }
       } 

    area = FindAreaWithOptional(120);  //area=0
Ve fonksiyonu çağırdığımızda 0 alırız çünkü ikinci argüman iletilmez ve int'nin varsayılan değeri 0'dır ve dolayısıyla ürün 0'dır.
    


[1]: http://i.stack.imgur.com/Uaszw.png
[2]: http://i.stack.imgur.com/3BWQA.png

## Adlandırılmış Bağımsız Değişkenler
Aşağıdakileri düşünün, işlev çağrımızdır.

    FindArea(120, 56);
Bunda ilk argümanımız uzunluk (yani 120) ve ikinci argüman genişliktir (yani 56). Ve alanı bu fonksiyona göre hesaplıyoruz. Ve aşağıdaki fonksiyon tanımıdır.

    private static double FindArea(int length, int width)
           {
               try
               {
                   return (length* width);
               }
               catch (Exception)
               {
                   throw new NotImplementedException();
               }
           }

Böylece ilk fonksiyon çağrısında, argümanları konumuna göre ilettik. Doğru?

    double area;
    Console.WriteLine("Area with positioned argument is: ");
    area = FindArea(120, 56);
    Console.WriteLine(area);
    Console.Read();
Bunu çalıştırırsanız aşağıdaki gibi bir çıktı alırsınız.

[![buraya resim açıklamasını girin][1]][1]

Şimdi burada adlandırılmış argümanların özellikleri geliyor. Lütfen önceki işlev çağrısına bakın.


    Console.WriteLine("Area with Named argument is: ");
    area = FindArea(length: 120, width: 56);
    Console.WriteLine(area);
    Console.Read();

Burada metod çağrısında isimlendirilmiş argümanları veriyoruz.

    area = FindArea(length: 120, width: 56);
Şimdi bu programı çalıştırırsanız, aynı sonucu alacaksınız. Adlandırılmış argümanları kullanıyorsak, yöntem çağrısında tam olarak adları verebiliriz.

    Console.WriteLine("Area with Named argument vice versa is: ");
    area = FindArea(width: 120, length: 56);
    Console.WriteLine(area);
    Console.Read();

Adlandırılmış bir argümanın önemli kullanımından biri, bunu programınızda kullandığınızda kodunuzun okunabilirliğini artırmasıdır. Sadece argümanınızın ne olması gerektiğini ya da ne olduğunu söylüyor?

Konumsal argümanları da verebilirsiniz. Bu, hem konumsal argümanın hem de adlandırılmış argümanın bir kombinasyonu anlamına gelir.

    Console.WriteLine("Area with Named argument Positional Argument : ");
                area = FindArea(120, width: 56);
                Console.WriteLine(area);
                Console.Read();

Yukarıdaki örnekte, parametre genişliği için uzunluk olarak 120'yi ve adlandırılmış argüman olarak 56'yı geçtik.

Bazı sınırlamalar da var. Şimdi adlandırılmış bir argümanın sınırlamasını tartışacağız.

**Adlandırılmış Argüman kullanmanın sınırlandırılması**

Adlandırılmış bağımsız değişken belirtimi, tüm sabit bağımsız değişkenler belirtildikten sonra görünmelidir.

Sabit bir argümandan önce adlandırılmış bir argüman kullanırsanız, aşağıdaki gibi bir derleme zamanı hatası alırsınız.

[![buraya resim açıklamasını girin][2]][2]

Adlandırılmış bağımsız değişken belirtimi, tüm sabit bağımsız değişkenler belirtildikten sonra görünmelidir


[1]: http://i.stack.imgur.com/aCYyR.png
[2]: http://i.stack.imgur.com/n8z4Y.png

