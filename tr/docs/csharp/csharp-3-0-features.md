---
title: "C# 3.0 Özellikleri"
slug: "c-30-ozellikleri"
draft: false
images: []
weight: 9976
type: docs
toc: true
---

C# sürüm 3.0, .Net sürüm 3.5'in bir parçası olarak yayınlandı. Bu sürümle eklenen özelliklerin çoğu LINQ'yu (Dil Tümleşik Sorgular) destekliyordu.

Eklenen özelliklerin listesi:

- LINQ
- Lambda ifadeleri
- Uzatma yöntemleri
- Anonim türler
- Örtülü olarak yazılan değişkenler
- Nesne ve Koleksiyon Başlatıcılar
- Otomatik olarak uygulanan özellikler
- İfade ağaçları

## Örtülü olarak yazılan değişkenler (var)
'var' anahtar sözcüğü, bir programcının derleme zamanında örtük olarak bir değişken yazmasına izin verir. 'var' bildirimleri, açıkça bildirilen değişkenlerle aynı türe sahiptir.

    var squaredNumber = 10 * 10;
    var squaredNumberDouble = 10.0 * 10.0;
    var builder = new StringBuilder();
    var anonymousObject = new
    { 
        One = SquaredNumber,
        Two = SquaredNumberDouble,
        Three = Builder
    }

Yukarıdaki değişkenlerin türleri sırasıyla 'int', 'double', 'StringBuilder' ve anonim bir türdür.

Bir "var" değişkeninin dinamik olarak yazılmadığına dikkat etmek önemlidir. Bir "StringBuilder" örneğine bir "int" ayarlamaya çalıştığınız için "SquaredNumber = Builder" geçerli değil

## Dille Tümleşik Sorgular (LINQ)
    //Example 1
    int[] array = { 1, 5, 2, 10, 7 };

    // Select squares of all odd numbers in the array sorted in descending order
    IEnumerable<int> query = from x in array
                             where x % 2 == 1
                             orderby x descending
                             select x * x;
    // Result: 49, 25, 1
[C# 3.0, LINQ alt bölümü hakkındaki Wikipedia makalesinden örnek][1]

Örnek 1, SQL sorgularına benzer görünecek şekilde tasarlanmış sorgu sözdizimini kullanır.

    //Example 2
    IEnumerable<int> query = array.Where(x => x % 2 == 1)
        .OrderByDescending(x => x)
        .Select(x => x * x);
    // Result: 49, 25, 1 using 'array' as defined in previous example
[C# 3.0, LINQ alt bölümü hakkındaki Wikipedia makalesinden örnek][1]

Örnek 2, örnek 1 ile aynı sonucu elde etmek için yöntem sözdizimini kullanır.

C#'ta LINQ sorgu sözdiziminin, LINQ yöntemi sözdizimi için [sözdizimsel şeker][2] olduğuna dikkat etmek önemlidir. Derleyici, sorguları derleme zamanında yöntem çağrılarına çevirir. Bazı sorguların yöntem sözdiziminde ifade edilmesi gerekir. [MSDN'den][3] - "Örneğin, belirli bir koşulla eşleşen öğe sayısını alan bir sorguyu ifade etmek için bir yöntem çağrısı kullanmalısınız."


[1]: https://en.wikipedia.org/wiki/C_Sharp_3.0#LINQ_.28language-integrated_query.29
[2]: https://en.wikipedia.org/wiki/Syntactic_sugar
[3]: https://msdn.microsoft.com/en-us/library/bb397947.aspx

## Lambda ifadeleri
Lambda İfadeleri, örtük olarak yazılan parametrelere ve dönüş değerlerine izin veren [anonim yöntemlerin][1] bir uzantısıdır. Sözdizimleri, anonim yöntemlerden daha az ayrıntılıdır ve işlevsel bir programlama stili izler.

    using System;
    using System.Collections.Generic;
    using System.Linq;
                        
    public class Program
    {
        public static void Main()
        {
            var numberList = new List<int> {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
            var sumOfSquares = numberList.Select( number => number * number )
                .Aggregate( (int first, int second) => { return first + second; } );
            Console.WriteLine( sumOfSquares );
        }
    }

Yukarıdaki kod, 1'den 10'a kadar olan sayıların karelerinin toplamını konsola çıkaracaktır.

İlk lambda ifadesi listedeki sayıların karesini alır. Sadece 1 parametre olduğu için parantez atlanabilir. Dilerseniz parantez ekleyebilirsiniz:

    .Select( (number) => number * number);

veya parametreyi açıkça yazın ancak parantez gerekir:

    .Select( (int number) => number * number);

Lambda gövdesi bir ifadedir ve örtük bir dönüşü vardır. İsterseniz bir ifade gövdesi de kullanabilirsiniz. Bu, daha karmaşık lambdalar için kullanışlıdır.

    .Select( number => { return number * number; } );

Select yöntemi, hesaplanan değerlerle yeni bir IEnumerable<int> döndürür.

İkinci lambda ifadesi, select yönteminden döndürülen listedeki sayıları toplar. Birden fazla parametre olduğundan parantez gereklidir. Parametrelerin türleri açıkça yazılmıştır, ancak bu gerekli değildir. Aşağıdaki yöntem eşdeğerdir.

    .Aggregate( (first, second) => { return first + second; } );

Bu da olduğu gibi:

    .Aggregate( (int first, int second) => first + second );

[1]: https://www.wikiod.com/tr/docs/c%23/60/methods/9338/anonymous-method#t=201608051345408629175

## Anonim türler
Anonim türler, bir tür salt okunur özelliği, önce açıkça bir tür tanımlamaya gerek kalmadan tek bir nesneye yerleştirmek için uygun bir yol sağlar. Tür adı derleyici tarafından oluşturulur ve kaynak kodu düzeyinde kullanılamaz. Her özelliğin türü derleyici tarafından belirlenir.

"new" anahtar sözcüğünü ve ardından bir küme ayracı _(`{`)_ kullanarak anonim türler oluşturabilirsiniz. Kıvrımlı parantezlerin içinde, aşağıdaki koddaki gibi özellikler tanımlayabilirsiniz.

    var v = new { Amount = 108, Message = "Hello" };

Bir dizi anonim tür oluşturmak da mümkündür. Aşağıdaki koda bakın:

    var a = new[] { 
        new { 
            Fruit = "Apple", 
            Color = "Red" 
        },
        new {
            Fruit = "Banana",
            Color = "Yellow"
        }
    };

Veya LINQ sorgularıyla kullanın:

    var productQuery = from prod in products
                       select new { prod.Color, prod.Price };

