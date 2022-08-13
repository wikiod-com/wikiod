---
title: "Dize Birleştirme"
slug: "dize-birlestirme"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Dinamik bir dize oluşturuyorsanız, her +/`Concat` her yürütüldüğünde yeni bir dize nesnesi oluşturduğundan, + veya `Concat` yöntemini kullanarak dizeleri birleştirmek yerine `StringBuilder` sınıfını seçmek iyi bir uygulamadır.

## + Operatör
    string s1 = "string1";
    string s2 = "string2";
    
    string s3 = s1 + s2; // "string1string2"

## System.Text.StringBuilder kullanarak dizeleri birleştirin
Bir [StringBuilder][1] kullanarak dizeleri birleştirme, `+` kullanarak basit dize birleştirmeye göre performans avantajları sunabilir. Bu, belleğin tahsis edilme biçiminden kaynaklanmaktadır. Dizeler, her bir birleştirme ile yeniden tahsis edilir, StringBuilders, belleği yalnızca mevcut blok tükendiğinde yeniden tahsis ederek bloklara ayırır. Bu, çok sayıda küçük birleştirme yaparken büyük bir fark yaratabilir.

    StringBuilder sb = new StringBuilder();
    for (int i = 1; i <= 5; i++)
    {
        sb.Append(i);
        sb.Append(" ");
    }
    Console.WriteLine(sb.ToString()); // "1 2 3 4 5 "

Append()'e yapılan çağrılar zincirleme bağlanabilir, çünkü 'StringBuilder'a bir başvuru döndürür:

    StringBuilder sb = new StringBuilder();
    sb.Append("some string ")
      .Append("another string");


[1]: https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx

## String.Join kullanarak dize dizisi öğelerini birleştirme
`String.Join` yöntemi, bir dize dizisindeki birden çok öğeyi birleştirmek için kullanılabilir.

    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";

    string result = String.Join(separator, value, 1, 2);
    Console.WriteLine(result);

> Şu çıktıyı üretir: "portakal, üzüm"

Bu örnek, ayırıcının ve değerin üstünde başlangıç ​​dizinini ve sayımı belirten `String.Join(String, String[], Int32, Int32)` aşırı yüklemesini kullanır.

startIndex'i kullanmak ve aşırı yüklemeleri saymak istemiyorsanız, verilen tüm dizelere katılabilirsiniz. Bunun gibi:
    
    string[] value = {"apple", "orange", "grape", "pear"};
    string separator = ", ";
    string result = String.Join(separator, value);
    Console.WriteLine(result);

üretecek olan;

> elma, portakal, üzüm, armut

## $ kullanarak iki dizenin birleştirilmesi
$, birden çok dizeyi birleştirmek için kolay ve özlü bir yöntem sağlar.

    var str1 = "text1";
    var str2 = " ";
    var str3 = "text3";
    string result2 = $"{str1}{str2}{str3}"; //"text1 text3"

