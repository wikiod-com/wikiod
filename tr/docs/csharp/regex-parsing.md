---
title: "Normal İfade Ayrıştırma"
slug: "normal-ifade-ayrstrma"
draft: false
images: []
weight: 9971
type: docs
toc: true
---

## Sözdizimi
- `new Regex(pattern);` //*Tanımlanmış bir modelle yeni bir örnek oluşturur.*
- `Regex.Match(input);` //*Aramayı başlatır ve Eşleşmeyi döndürür.*
- `Regex.Matches(input);` //*Aramayı başlatır ve bir MatchCollection döndürür*


## Parametreler
| İsim | Ayrıntılar|
| ------ | ------ |
| Desen | Arama için kullanılması gereken "dize" kalıbı. Daha fazla bilgi için: [msdn][1]|
| Normal İfade Seçenekleri *[İsteğe Bağlı]* | Buradaki ortak seçenekler 'Tek satır' ve 'Çok satırlı'. Nokta (.) gibi, "Çok Satırlı Mod"da "Yeni Satır"ı (\n) kapsamayan, ancak "Tek Satır Modu"ndaki desen öğelerinin davranışını değiştiriyorlar. Varsayılan davranış: [msdn][2] |
| Zaman Aşımı *[İsteğe Bağlı]* | Kalıpların daha karmaşık hale geldiği yerlerde arama daha fazla zaman alabilir. Bu, ağ programlamadan bilindiği gibi arama için geçen zaman aşımıdır.|


[1]: https://msdn.microsoft.com/en-us/library/ae5bf541(v=vs.90).aspx
[2]: https://msdn.microsoft.com/en-US/library/yd1hzczs(v=vs.110).aspx#Default

**Kullanılması gerekiyor**

    using System.Text.RegularExpressions;

**Olması güzel**

- Buradan sonuç almak için çözümünüzü derlemenize gerek kalmadan kalıplarınızı çevrimiçi olarak test edebilirsiniz: [Beni tıklayın][1]
- Regex101 Örneği: [Beni tıklayın][2]

__________

*Özellikle yeni başlayanlar, daha karmaşık metin tabanlı aramalar için güçlü ve doğru yerde olduğu için normal ifadeyle görevlerini abartma eğilimindedir. Bu, insanların 'XmlDocument' gibi bu görev için zaten bitmiş bir sınıf olup olmadığını kendilerine sormadan xml belgelerini regex ile ayrıştırmaya çalıştıkları noktadır.*

* Regex, karmaşıklığa karşı seçilecek son silah olmalıdır. En azından 20 satırlık kalıp yazmadan önce 'doğru yolu' aramak için biraz çaba sarf etmeyi unutmayın.*


[1]: https://regex101.com/
[2]: https://regex101.com/r/cG9lP5/1


## Tek maç
*`System.Text.RegularExpressions kullanarak;`*

    string pattern = ":(.*?):";
    string lookup = "--:text in here:--";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    // Get the match from your regex-object
    Match mLookup = rgxLookup.Match(lookup);
    
    // The group-index 0 always covers the full pattern.
    // Matches inside parentheses will be accessed through the index 1 and above.
    string found = mLookup.Groups[1].Value;

**Sonuç:**

    found = "text in here"

## Birden fazla eşleşme
*`System.Text.RegularExpressions kullanarak;`*

    List<string> found = new List<string>();
    string pattern = ":(.*?):";
    string lookup = "--:text in here:--:another one:-:third one:---!123:fourth:";
    
    // Instanciate your regex object and pass a pattern to it
    Regex rgxLookup = new Regex(pattern, RegexOptions.Singleline, TimeSpan.FromSeconds(1));
    MatchCollection mLookup = rgxLookup.Matches(lookup);
    
    foreach(Match match in mLookup)
    {
        found.Add(match.Groups[1].Value);
    }

**Sonuç:**

    found = new List<string>() { "text in here", "another one", "third one", "fourth" }

