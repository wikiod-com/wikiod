---
title: "Dizeyi diğer türlere dönüştürürken FormatException'ı işleme"
slug: "dizeyi-diger-turlere-donustururken-formatexception-isleme"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Dizeyi tam sayıya dönüştürme
Bir "dize"yi bir "tamsayı"ya açıkça dönüştürmek için kullanılabilecek çeşitli yöntemler vardır, örneğin:
1. `Convert.ToInt16();`

2. `Convert.ToInt32();`

3. `Convert.ToInt64();`

4. `int.Parse();`

Ancak, giriş dizesi sayısal olmayan karakterler içeriyorsa, tüm bu yöntemler bir "FormatException" oluşturur. Bunun için, bu gibi durumlarda onları ele almak için ek bir istisna işleme(`try..catch`) yazmamız gerekiyor.

<saat/>
 
**Örneklerle Açıklama:**

Öyleyse girdimiz şöyle olsun:

    string inputString = "10.2";


**Örnek 1:** `Convert.ToInt32()`

    int convertedInt = Convert.ToInt32(inputString); // Failed to Convert 
    // Throws an Exception "Input string was not in a correct format."

***Not:** Aynısı belirtilen diğer yöntemler için de geçerlidir - `Convert.ToInt16();` ve `Convert.ToInt64();`*
 

**Örnek 2:** `int.Parse()`

    int convertedInt = int.Parse(inputString); // Same result "Input string was not in a correct format.

***Bunu nasıl atlatırız?***

Daha önce de belirtildiği gibi, istisnaları ele almak için genellikle aşağıda gösterildiği gibi bir 'try..catch'e ihtiyacımız var:

    try
    {
        string inputString = "10.2";
        int convertedInt = int.Parse(inputString);
    }
    catch (Exception Ex)
    {
        //Display some message, that the conversion has failed.         
    }
Ancak, her yerde `try..catch` kullanmak iyi bir uygulama olmayacaktır ve giriş yanlışsa `0` vermek istediğimiz bazı senaryolar olabilir, _(Yukarıdaki yöntemi takip edersek atamamız gerekir. catch bloğundan "0"dan "convertedInt"e)._
Bu tür senaryoları ele almak için `.TryParse()` adlı özel bir yöntemi kullanabiliriz.

'.TryParse()' yöntemi, size 'out' parametresinin çıktısını verecek olan dahili bir İstisna işlemeye sahiptir ve dönüştürme durumunu _(dönüşüm başarılıysa 'true'; 'false') belirten bir Boole değeri döndürür. başarısız olursa)._ Dönüş değerine göre dönüşüm durumunu belirleyebiliriz. Bir Örnek görelim:

**Kullanım 1:** Dönüş değerini bir Boole değişkeninde saklayın

     int convertedInt; // Be the required integer
     bool isSuccessConversion = int.TryParse(inputString, out convertedInt);
Dönüşüm durumunu kontrol etmek için Yürütme işleminden sonra `isSuccessConversion` değişkenini kontrol edebiliriz. Yanlış ise, "convertedInt" değeri "0" olacaktır (dönüşüm hatası için "0" istiyorsanız, dönüş değerini kontrol etmeniz gerekmez)._

**Kullanım 2:** Dönüş değerini 'if' ile kontrol edin

    if (int.TryParse(inputString, out convertedInt))
    {
        // convertedInt will have the converted value
        // Proceed with that
    }
    else 
    {
     // Display an error message
    }
**Kullanım 3:** Dönüş değerini kontrol etmeden
aşağıdakileri kullanabilirsiniz, eğer dönüş değeri _(dönüştürülmüş veya çevrilmemiş, `0` uygun olacaktır) _ ile ilgilenmiyorsanız _

    int.TryParse(inputString, out convertedInt);
    // use the value of convertedInt
    // But it will be 0 if not converted

