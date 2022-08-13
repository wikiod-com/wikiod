---
title: "Ortak Dizi İşlemleri"
slug: "ortak-dizi-islemleri"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Bir dizeyi biçimlendirme
Dizedeki bir veya daha fazla öğeyi belirtilen nesnenin dize temsiliyle değiştirmek için `String.Format()` yöntemini kullanın:

    String.Format("Hello {0} Foo {1}", "World", "Bar") //Hello World Foo Bar

## Bir dizeyi sabit bir uzunlukta doldurma
    string s = "Foo";
    string paddedLeft = s.PadLeft(5);        // paddedLeft = "  Foo" (pads with spaces by default)
    string paddedRight = s.PadRight(6, '+'); // paddedRight = "Foo+++"
    string noPadded = s.PadLeft(2);          // noPadded = "Foo" (original string is never shortened)


## Bir dizeyi doğru şekilde ters çevirme
Çoğu zaman insanlar bir diziyi tersine çevirmek zorunda kaldıklarında, bunu aşağı yukarı şu şekilde yaparlar:

    char[] a = s.ToCharArray();
    System.Array.Reverse(a);
    string r = new string(a);

Ancak, bu insanların anlamadığı şey, bunun aslında yanlış olduğudur. <br />
Ve eksik NULL kontrolü yüzünden demek istemiyorum.

Aslında yanlıştır çünkü bir Glif/GraphemeCluster birkaç kod noktasından (diğer bir deyişle karakterlerden) oluşabilir.

Bunun neden böyle olduğunu anlamak için öncelikle "karakter" teriminin gerçekte ne anlama geldiğinin farkında olmamız gerekir.

[Referans:][1]
> Karakter, pek çok anlama gelebilen aşırı yüklenmiş bir terimdir.
> 
> Bir kod noktası, bilginin atomik birimidir. Metin bir dizidir
> kod noktaları. Her kod noktası, kod tarafından anlam verilen bir sayıdır.
> Unicode standardı.
> 
> Bir grafik, görüntülenen bir veya daha fazla kod noktası dizisidir.
> okuyucunun tek bir birim olarak tanıdığı tek bir grafik birim olarak
> yazı sisteminin öğesi. Örneğin, hem a hem de ä
> grafikemler, ancak birden çok kod noktasından oluşabilirler (ör. ä
> iki kod noktası, biri temel a karakteri ve ardından biri
> ishal; ama aynı zamanda alternatif, eski, tek bir kod noktası da var
> bu grafiği temsil eden). Bazı kod noktaları hiçbir zaman herhangi birinin parçası değildir.
> grafem (ör. sıfır genişlikli marangoz olmayan veya yönsel geçersiz kılmalar).
> 
> Bir glif, genellikle bir yazı tipinde (bir koleksiyon olan) saklanan bir görüntüdür.
> glifler), grafikleri veya bunların kısımlarını temsil etmek için kullanılır. yazı tipleri
> birden çok glifi tek bir temsilde oluşturun, örneğin,
> yukarıdaki ä tek bir kod noktasıdır, bir yazı tipi bunu şu şekilde oluşturmayı seçebilir:
> iki ayrı, uzamsal olarak kaplanmış glif. OTF için, yazı tipinin GSUB'si ve
> GPOS tabloları, yapılacak ikame ve konumlandırma bilgilerini içerir.
> bu iş. Bir yazı tipi, aynı yazı tipi için birden çok alternatif glif içerebilir.
> grafik de.

Yani C#'ta bir karakter aslında bir CodePoint'tir.

Bunun anlamı, "Les Misérables" gibi geçerli bir dizeyi tersine çevirirseniz, şöyle görünebilir

    string s = "Les Mise\u0301rables";

bir karakter dizisi olarak şunları elde edersiniz:

> selbaŕesiM sel

Gördüğünüz gibi vurgu e karakteri yerine R karakterinde. <br />
Her ne kadar char dizisini iki kere ters çevirirseniz string.reverse.reverse orijinal dizgiyi verecek olsa da, bu tür bir tersine çevirme kesinlikle orijinal dizgenin tersi DEĞİLDİR.


Yalnızca her GraphemeCluster'ı tersine çevirmeniz gerekir. <br />
Bu nedenle, doğru yapılırsa, şöyle bir dizeyi tersine çevirirsiniz:


        private static System.Collections.Generic.List<string> GraphemeClusters(string s)
        {
            System.Collections.Generic.List<string> ls = new System.Collections.Generic.List<string>();
    
            System.Globalization.TextElementEnumerator enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(s);
            while (enumerator.MoveNext())
            {
                ls.Add((string)enumerator.Current);
            }
    
            return ls;
        }
    
    
        // this 
        private static string ReverseGraphemeClusters(string s)
        {
            if(string.IsNullOrEmpty(s) || s.Length == 1)
                 return s;
            
            System.Collections.Generic.List<string> ls = GraphemeClusters(s);
            ls.Reverse();
    
            return string.Join("", ls.ToArray());
        }
    
        public static void TestMe()
        {
            string s = "Les Mise\u0301rables";
            // s = "noël";
            string r = ReverseGraphemeClusters(s);
    
            // This would be wrong:
            // char[] a = s.ToCharArray();
            // System.Array.Reverse(a);
            // string r = new string(a);
    
            System.Console.WriteLine(r);
        }

Ve - oh sevinç - bunu böyle doğru yaparsanız fark edeceksiniz, Asya/Güney-Asya/Doğu-Asya dilleri (ve Fransızca/İsveççe/Norveç, vb.) için de işe yarayacaktır...


[1]: https://stackoverflow.com/questions/27331819/whats-the-difference-between-a-character-a-code-point-a-glyph-and-a-grapheme

## Bir dizgenin sağ tarafından x karakter alma
Visual Basic, bir dizenin Sol, Sağ ve Ortasından karakterler döndüren Sol, Sağ ve Orta işlevlere sahiptir. Bu yöntemler C#'da mevcut değildir, ancak `Substring()` ile uygulanabilir. Aşağıdaki gibi bir uzatma yöntemi olarak uygulanabilirler:


       public static class StringExtensions
       {
          /// <summary>
          /// VB Left function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Left-most numchars characters</returns>
          public static string Left( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( 0, numchars );
          }
        
          /// <summary>
          /// VB Right function
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="numchars"></param>
          /// <returns>Right-most numchars characters</returns>
          public static string Right( this string stringparam, int numchars )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (numchars > stringparam.Length)
                numchars = stringparam.Length;
        
             return stringparam.Substring( stringparam.Length - numchars );
          }
        
          /// <summary>
          /// VB Mid function - to end of string
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex )
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
             
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1 );
          }
        
          /// <summary>
          /// VB Mid function - for number of characters
          /// </summary>
          /// <param name="stringparam"></param>
          /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
          /// <param name="numchars">number of characters to return</param>
          /// <returns>Balance of string beginning at startindex character</returns>
          public static string Mid( this string stringparam, int startindex, int numchars)
          {
             // Handle possible Null or numeric stringparam being passed
             stringparam += string.Empty;
        
             // Handle possible negative startindex being passed
             startindex = Math.Abs( startindex );
        
             // Handle possible negative numchars being passed
             numchars = Math.Abs( numchars );
        
             // Validate numchars parameter        
             if (startindex > stringparam.Length)
                startindex = stringparam.Length;
        
             // C# strings are zero-based, convert passed startindex
             return stringparam.Substring( startindex - 1, numchars );
    
           }
        }
Bu uzatma yöntemi aşağıdaki gibi kullanılabilir:

    string myLongString = "Hello World!";
    string myShortString = myLongString.Right(6);  // "World!"
    string myLeftString = myLongString.Left(5);    // "Hello"
    string myMidString1 = myLongString.Left(4);    // "lo World"
    string myMidString2 = myLongString.Left(2,3);    // "ell"








## String.IsNullOrEmpty() ve String.IsNullOrWhiteSpace() kullanarak boş String olup olmadığını kontrol etme
    string nullString = null;
    string emptyString = "";
    string whitespaceString = "    ";
    string tabString = "\t";
    string newlineString = "\n";
    string nonEmptyString = "abc123";
    
    bool result;

    result = String.IsNullOrEmpty(nullString);            // true
    result = String.IsNullOrEmpty(emptyString);           // true
    result = String.IsNullOrEmpty(whitespaceString);      // false
    result = String.IsNullOrEmpty(tabString);             // false
    result = String.IsNullOrEmpty(newlineString);         // false
    result = String.IsNullOrEmpty(nonEmptyString);        // false

    result = String.IsNullOrWhiteSpace(nullString);       // true
    result = String.IsNullOrWhiteSpace(emptyString);      // true
    result = String.IsNullOrWhiteSpace(tabString);        // true
    result = String.IsNullOrWhiteSpace(newlineString);    // true
    result = String.IsNullOrWhiteSpace(whitespaceString); // true
    result = String.IsNullOrWhiteSpace(nonEmptyString);   // false

## İstenmeyen Karakterleri Dizelerin Başından ve/veya Sonundan Kırpma.
`Dize.Trim()`
--------

    string x = "   Hello World!    ";
    string y = x.Trim(); // "Hello World!"

    string q = "{(Hi!*";
    string r = q.Trim( '(', '*', '{' ); // "Hi!"


`String.TrimStart()` ve `String.TrimEnd()`
--------------------------------------------

    string q = "{(Hi*";
    string r = q.TrimStart( '{' ); // "(Hi*"
    string s = q.TrimEnd( '*' );   // "{(Hi" 


## Diziden bir dize oluşturun
`String.Join` yöntemi, diziden/karakter listesinden veya dizeden bir dizi oluşturmamıza yardımcı olacaktır. Bu metot iki parametre kabul eder. Birincisi, dizideki her bir elemanı ayırmanıza yardımcı olacak sınırlayıcı veya ayırıcıdır. İkinci parametre ise Dizinin kendisidir.

**"char dizisinden" gelen dize:**

    string delimiter=",";
    char[] charArray = new[] { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charArray);
**Çıktı** : 'a,b,c' 'sınırlayıcı'yı '""' olarak değiştirirsek çıktı 'abc' olur.

**"Karakter Listesi"nden gelen dize:**

    string delimiter = "|";
    List<char> charList = new List<char>() { 'a', 'b', 'c' };
    string inputString = String.Join(delimiter, charList);

**Çıktı** : `a|b|c`

**`Dize Listesinden` dize:**

    string delimiter = " ";
    List<string> stringList = new List<string>() { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringList);

**Çıktı** : 'Ram erkektir'

**"dizi dizisinden" dize:**

    string delimiter = "_";
    string[] stringArray = new [] { "Ram", "is", "a","boy" };
    string inputString = String.Join(delimiter, stringArray);

**Çıktı** : `Ram_is_a_boy`


## ToString kullanarak biçimlendirme
Genellikle biçimlendirme amacıyla `String.Format` yöntemini kullanırız,`.ToString` genellikle diğer türleri dizeye dönüştürmek için kullanılır. Dönüştürme yapılırken ToString yöntemi ile birlikte formatı belirleyebiliriz, böylece ek bir Formatlamadan kaçınabiliriz. Farklı türlerle nasıl çalıştığını açıklayayım;

**Biçimlendirilmiş dizeye tam sayı:**

    int intValue = 10;
    string zeroPaddedInteger = intValue.ToString("000"); // Output will be "010"
    string customFormat = intValue.ToString("Input value is 0"); // output will be   "Input value is 10" 
**biçimlendirilmiş dizeye çift:**

    double doubleValue = 10.456;
    string roundedDouble = doubleValue.ToString("0.00"); // output 10.46
    string integerPart = doubleValue.ToString("00");    // output 10
    string customFormat = doubleValue.ToString("Input value is 0.0");  // Input value is 10.5

**ToString kullanarak DateTime'ı biçimlendirme**

    DateTime currentDate = DateTime.Now; //  {7/21/2016 7:23:15 PM}
    string dateTimeString = currentDate.ToString("dd-MM-yyyy HH:mm:ss"); // "21-07-2016 19:23:15"
    string dateOnlyString = currentDate.ToString("dd-MM-yyyy"); // "21-07-2016"
    string dateWithMonthInWords = currentDate.ToString("dd-MMMM-yyyy HH:mm:ss"); // "21-July-2016 19:23:15"




## Ondalık Sayıyı İkili, Sekizli ve Onaltılı Biçime Dönüştür
1. Ondalık sayıyı ikili biçime dönüştürmek için **temel 2**'yi kullanın.

        Int32 Number = 15;
        Console.WriteLine(Convert.ToString(Number, 2));  //OUTPUT : 1111

2. Ondalık sayıyı sekizli biçime dönüştürmek için **temel 8** kullanın

        int Number = 15;
        Console.WriteLine(Convert.ToString(Number, 8));  //OUTPUT : 17

3. Ondalık sayıyı onaltılık biçime dönüştürmek için **taban 16**'yı kullanın.

        var Number = 15;
        Console.WriteLine(Convert.ToString(Number, 16));  //OUTPUT : f



## Bir Dizeyi belirli bir karaktere göre bölme
    string helloWorld = "hello world, how is it going?";
    string[] parts1 = helloWorld.Split(',');

    //parts1: ["hello world", " how is it going?"]

    string[] parts2 = helloWorld.Split(' ');

    //parts2: ["hello", "world,", "how", "is", "it", "going?"]


## Belirli bir dizenin Alt dizelerini alma
    string helloWorld = "Hello World!";
    string world = helloWorld.Substring(6); //world = "World!"
    string hello = helloWorld.Substring(0,5); // hello = "Hello"

"Alt dize", belirli bir dizinden veya iki dizin (her ikisi de dahil) arasındaki dizeyi döndürür.

## Bir dizenin belirli bir diziyle başlayıp başlamadığını belirleyin
    string HelloWorld = "Hello World";
    HelloWorld.StartsWith("Hello"); // true
    HelloWorld.StartsWith("Foo"); // false


**Bir dize içinde bir dize bulma**

Kullanmak
[`System.String.Contains`][1] bir dize içinde belirli bir dizenin olup olmadığını öğrenebilirsiniz. Yöntem, bir boolean döndürür, eğer dize false ise true.

    string s = "Hello World";
    bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 


[1]: https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx

## Bir dizi diziyi yenisiyle birleştirme
    var parts = new[] { "Foo", "Bar", "Fizz", "Buzz"};
    var joined = string.Join(", ", parts);

    //joined = "Foo, Bar, Fizz, Buzz"

## Belirli bir dizinde bir karakter alma ve dizeyi numaralandırma
Herhangi bir yerde bir dizgeden istediğiniz sayıda karakter almak için 'Substring' yöntemini kullanabilirsiniz. Ancak, yalnızca tek bir karakter istiyorsanız, dizide yaptığınız gibi herhangi bir dizinde tek bir karakter elde etmek için dize dizinleyiciyi kullanabilirsiniz:

    string s = "hello";
    char c = s[1]; //Returns 'e'

Bir "dize" türü döndüren "Substring" yönteminin aksine, dönüş türünün "char" olduğuna dikkat edin.

Dizin karakterlerini yinelemek için dizin oluşturucuyu da kullanabilirsiniz:

    string s = "hello";
    foreach (char c in s)
        Console.WriteLine(c);
    /********* This will print each character on a new line:
    h
    e
    l
    l
    o
    **********/

## Bir Dizeyi başka bir dizeyle bölme
    string str = "this--is--a--complete--sentence";
    string[] tokens = str.Split(new[] { "--" }, StringSplitOptions.None);

Sonuç:

>[ "bu", "is", "a", "tamamlandı", "cümle" ]

## Bir dize içindeki bir dizeyi değiştirme
[`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx) yöntemini kullanarak, bir dizenin bir kısmını bir başkasıyla değiştirebilirsiniz. sicim.

    string s = "Hello World";
     s = s.Replace("World", "Universe"); // s = "Hello Universe"
Arama dizesinin tüm oluşumları değiştirilir.

Bu yöntem, [`String.Empty`](https://msdn.microsoft.com/en-us/library/system.string.empty(v=vs.110) kullanılarak bir dizenin bir kısmını kaldırmak için de kullanılabilir. .aspx) alanı:

    string s = "Hello World";
    s = s.Replace("ell", String.Empty); // s = "Ho World"


## Bir String içindeki karakterlerin büyük/küçük harf durumunu değiştirme
[`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) sınıfı, büyük harf ve küçük harf arasında dönüştürme yapmak için bir dizi yöntemi destekler bir dizedeki karakterler.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) dönüştürülen bir String nesnesini döndürmek için kullanılır küçük harfe.


- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) dönüştürülen bir String nesnesini döndürmek için kullanılır büyük harfe.

**Not:** Bu yöntemlerin *değişmez* sürümlerini kullanmanın nedeni, kültüre özgü beklenmeyen harflerin üretilmesini önlemektir. Bu, [burada ayrıntılı olarak](http://stackoverflow.com/a/19778131/1379664) açıklanmıştır.

Örnek:

    string s = "My String";
    s = s.ToLowerInvariant(); // "my string"
    s = s.ToUpperInvariant(); // "MY STRING"


Belirli bir **[Kültür](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** belirtmeyi * seçebileceğinizi* unutmayın. [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) ve [String.ToUpper kullanarak küçük ve büyük harfe dönüştürürken (CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) yöntemleri buna göre.



## Bir dizi diziyi tek bir dizede birleştirin
[`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) yöntemi, kullanarak bir dize dizisindeki tüm öğeleri birleştirmeye izin verir her öğe arasında belirli bir ayırıcı:

    string[] words = {"One", "Two", "Three", "Four"};
    string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"


## Dize Birleştirme
Dize Birleştirme, [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) yöntemi kullanılarak yapılabilir. veya (çok daha kolay) `+` operatörünü kullanarak:

    string first = "Hello ";
    string second = "World";

    string concat = first + second; // concat = "Hello World"
    concat = String.Concat(first, second); // concat = "Hello World"

C# 6'da bu şu şekilde yapılabilir:

    string concat = $"{first},{second}";



