---
title: "Kod Sözleşmeleri"
slug: "kod-sozlesmeleri"
draft: false
images: []
weight: 9947
type: docs
toc: true
---

Kod sözleşmeleri, yöntemlerin öncesi/sonrası koşullarının ve nesneler için değişmez koşulların derlenmesine veya çalışma zamanı analizine izin verir. Bu koşullar, arayanların ve dönüş değerinin uygulama işleme için geçerli durumlarla eşleşmesini sağlamak için kullanılabilir. Kod Sözleşmelerinin diğer kullanımları, belge oluşturmayı içerir.

## Arayüzler için Sözleşmeler
Kod Sözleşmelerini kullanarak bir arabirime bir sözleşme uygulamak mümkündür. Bu, arayüzleri uygulayan soyut bir sınıf bildirerek yapılır. Arayüz 'ContractClassAttribute' ile etiketlenmeli ve sözleşme tanımı (soyut sınıf) 'ContractClassForAttribute' ile etiketlenmelidir.

**C# Örneği...**

    [ContractClass(typeof(MyInterfaceContract))]
    public interface IMyInterface
    {
        string DoWork(string input);
    }
    //Never inherit from this contract defintion class
    [ContractClassFor(typeof(IMyInterface))]
    internal abstract class MyInterfaceContract : IMyInterface
    {
        private MyInterfaceContract() { }

        public string DoWork(string input)
        {
            Contract.Requires(!string.IsNullOrEmpty(input));
            Contract.Ensures(!string.IsNullOrEmpty(Contract.Result<string>()));
            throw new NotSupportedException();
        }
    }
    public class MyInterfaceImplmentation : IMyInterface
    {
        public string DoWork(string input)
        {
            return input;
        }
    }

**Statik Analiz Sonucu...**

[![buraya resim açıklamasını girin][1]][1]


[1]: http://i.stack.imgur.com/eDxbs.png

## Ön koşullar
Ön koşullar, yöntemlerin giriş parametreleri için gerekli minimum değerleri sağlamasına olanak tanır

**Örnek...**

    void DoWork(string input)
    {
        Contract.Requires(!string.IsNullOrEmpty(input));

        //do work
    }

**Statik Analiz Sonucu...**

[![buraya resim açıklamasını girin][1]][1]


[1]: http://i.stack.imgur.com/ZFVU0.png

## Son koşullar
Son koşullar, bir yöntemden döndürülen sonuçların sağlanan tanımla eşleşmesini sağlar. Bu, arayana beklenen sonucun bir tanımını sağlar. Statik analizör tarafından bazı olası sonuçlar sağlanabileceğinden, basitleştirilmiş uygulamalar için son koşullara izin verilebilir.

**Örnek...**

    string GetValue()
    {
        Contract.Ensures(Contract.Result<string>() != null);

        return null;
    }

**Statik Analiz Sonucu...**

[![buraya resim açıklamasını girin][1]][1]


[1]: http://i.stack.imgur.com/gpCrS.png

## Kod Sözleşmelerini Yükleme ve Etkinleştirme
'System.Diagnostics.Contracts' .Net Framework içinde yer alırken. Kod Sözleşmelerini kullanmak için Visual Studio uzantılarını yüklemelisiniz.

'Uzantılar ve Güncellemeler' altında 'Kod Sözleşmeleri'ni arayın ve ardından 'Kod Sözleşmeleri Araçları'nı yükleyin.

[![Kod Sözleşme Araçları kurulumu][1]][1]

Araçlar yüklendikten sonra, Proje çözümünüzde 'Kod Sözleşmeleri'ni etkinleştirmelisiniz. En azından muhtemelen 'Statik Kontrol'ü etkinleştirmek istersiniz (derlemeden sonra kontrol edin). Diğer çözümler tarafından kullanılacak bir kitaplık uyguluyorsanız, "Çalışma Zamanı Denetimi"ni de etkinleştirmeyi düşünebilirsiniz.

[![Proje Ayarları][2]][2]


[1]: http://i.stack.imgur.com/hTYJ1.png
[2]: http://i.stack.imgur.com/f4f1Z.png

