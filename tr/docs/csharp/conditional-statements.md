---
title: "Koşullu İfadeler"
slug: "kosullu-ifadeler"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

## If-Else İfadesi
Genel olarak programlama, kodun farklı girdiler veya koşullar altında nasıl çalıştığını açıklamak için genellikle kod içinde bir "karar" veya "dal" gerektirir. C# programlama dilinde (ve bu konuda çoğu programlama dilinde), programınız içinde bir dal oluşturmanın en basit ve bazen en kullanışlı yolu bir 'If-Else' ifadesidir.

100'e kadar bir puanı temsil edecek bir int parametresi alan bir yöntemimiz (a.k.a. bir işlev) olduğunu varsayalım ve yöntem, başarılı olup olmadığımızı söyleyen bir mesaj yazdıracaktır.

    static void PrintPassOrFail(int score)
    {
        if (score >= 50) // If score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If score is not greater or equal to 50
        {
            Console.WriteLine("Fail!");
        }
    }

Bu metoda bakarken, `If` ifadesinin içinde bu kod satırını (`score >= 50`) fark edebilirsiniz. Bu bir "boolean" koşulu olarak görülebilir; burada koşul "true" değerine eşit olarak değerlendirilirse, "if" "{ }" arasındaki kodun çalıştırılması sağlanır.

Örneğin, bu yöntem şöyle çağrıldıysa:
`PrintPassOrFail(60);`, yöntemin çıktısı, 60 parametresinin değeri 50'ye eşit veya daha büyük olduğundan ***Geçti!*** yazan bir Konsol Baskısı olacaktır.

Ancak, yöntem şu şekilde çağrıldıysa: `PrintPassOrFail(30);`, yöntemin çıktısı ***Fail!*** şeklinde yazdırılırdı. Bunun nedeni, 30 değerinin 50'den büyük veya 50'ye eşit olmamasıdır, bu nedenle "If" ifadesi yerine "else" "{ }" arasındaki kodun çalıştırılmasıdır.

Bu örnekte, *puanın* 100'e kadar çıkması gerektiğini söyledik ve bu hiç hesaba katılmadı. *Skorun* 100'ü geçmemesini veya muhtemelen 0'ın altına düşmesini hesaba katmak için **Else If-Else İfadesi** örneğine bakın.

## If-Else If-Else İfadesi
**If-Else İfadesi** örneğinden yola çıkarak, şimdi "Else If" ifadesini tanıtmanın zamanı geldi. "Else If" ifadesi, **If-Else If-Else** yapısındaki "If" ifadesinin hemen ardından gelir, ancak özünde "If" ifadesi ile benzer bir sözdizimine sahiptir. Basit bir **If-Else** ifadesinin yapabileceğinden daha fazla dal eklemek için kullanılır.

**If-Else İfadesinden** alınan örnekte, puanın 100'e kadar çıktığını belirten örnek; ancak buna karşı hiçbir zaman kontrol olmadı. Bunu düzeltmek için, **If-Else İfadesinden** yöntemi şu şekilde değiştirelim:

    static void PrintPassOrFail(int score)
    {
        if (score > 100) // If score is greater than 100
        {
            Console.WriteLine("Error: score is greater than 100!");
        }
        else if (score < 0) // Else If score is less than 0
        {
            Console.WriteLine("Error: score is less than 0!");
        }
        else if (score >= 50) // Else if score is greater or equal to 50
        {
            Console.WriteLine("Pass!");
        }
        else // If none above, then score must be between 0 and 49
        {
            Console.WriteLine("Fail!");
        }
    }

Tüm bu ifadeler, bir koşul karşılanana kadar yukarıdan aşağıya doğru sırayla çalışacaktır. Yöntemin bu yeni güncellemesinde, artık *sınır dışı* olan puanlara uyum sağlamak için iki yeni dal ekledik.

Örneğin, şimdi kodumuzdaki yöntemi `PrintPassOFail(110);` olarak çağırsaydık, çıktı şöyle bir Konsol Baskısı olurdu ***Hata: puan 100'den büyük!***; ve kodumuzdaki metodu `PrintPassOrFail(-20);` gibi çağırırsak, çıktı ***Hata: puan 0'dan küçük!*** derdi.

## İfade koşulları standart boole ifadeleri ve değerleri ise
Aşağıdaki ifade

    if (conditionA && conditionB && conditionC) //...
tam olarak eşdeğerdir

    bool conditions = conditionA && conditionB && conditionC;
    if (conditions) // ...
başka bir deyişle, "if" ifadesinin içindeki koşullar sadece sıradan bir Boole ifadesi oluşturur.

Koşullu ifadeler yazarken yapılan yaygın bir hata, açıkça "doğru" ve "yanlış" ile karşılaştırmaktır:

    if (conditionA == true && conditionB == false && conditionC == true) // ...

Bu şu şekilde yeniden yazılabilir

    if (conditionA && !conditionB && conditionC)

## İfadeleri değiştir
Switch ifadesi, bir değişkenin bir değerler listesine göre eşitlik açısından test edilmesini sağlar. Her değer bir durum olarak adlandırılır ve her bir anahtar durumu için açık olan değişken kontrol edilir.

Tek bir değişken için birden çok olası değeri test ederken, bir "switch" ifadesi genellikle "if...else if... else.." ifadelerinden daha kısa ve anlaşılırdır.

    
Sözdizimi aşağıdaki gibidir

    switch(expression) {
       case constant-expression:
          statement(s);
          break;
       case constant-expression:
          statement(s);
          break;
      
       // you can have any number of case statements
       default : // Optional
          statement(s);
          break;
    }

switch deyimini kullanırken dikkate alınması gereken birkaç şey var.

- Bir switch deyiminde kullanılan ifade, integral veya numaralandırılmış bir türe sahip olmalı veya sınıfın tek bir integral veya numaralandırılmış türe dönüştürme işlevine sahip olduğu bir sınıf türünde olmalıdır.
- Bir anahtar içinde istediğiniz sayıda vaka ifadesine sahip olabilirsiniz. Her vakayı, karşılaştırılacak değer ve iki nokta üst üste gelir. Karşılaştırılacak değerlerin her bir switch deyiminde benzersiz olması gerekir.
- Bir switch deyiminin isteğe bağlı bir varsayılan durumu olabilir. Varsayılan durum, durumların hiçbiri doğru olmadığında bir görevi gerçekleştirmek için kullanılabilir.
- Boş bir ifade olmadığı sürece her durum bir 'break' ifadesi ile bitmelidir. Bu durumda yürütme, altındaki durumda devam eder. Break ifadesi, bir "return", "throw" veya "goto case" ifadesi kullanıldığında da atlanabilir.


Akıllıca notlarla örnek verilebilir

    char grade = 'B';

    switch (grade)
    {
        case 'A':
            Console.WriteLine("Excellent!");
            break;
        case 'B':
        case 'C':
            Console.WriteLine("Well done");
            break;
        case 'D':
            Console.WriteLine("You passed");
            break;
        case 'F':
            Console.WriteLine("Better try again");
            break;
        default:
            Console.WriteLine("Invalid grade");
            break;
    }

