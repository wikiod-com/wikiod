---
title: "XML Belgelendirme Yorumları"
slug: "xml-belgelendirme-yorumlar"
draft: false
images: []
weight: 9846
type: docs
toc: true
---

Bazen xml yorumlarınızdan **genişletilmiş metin belgeleri oluşturmanız** gerekir. Ne yazık ki *** bunun standart bir yolu yok***.

Ancak bu durumda kullanabileceğiniz bazı ayrı projeler var:

- [Kumdan Kale][1]
- [Belge][2]
- [NDoc][1]
- [DocFX][4]


[1]: http://sandcastle.codeplex.com/
[2]: http://docu.jagregory.com/
[3]: http://ndoc.sourceforge.net/
[4]: https://dotnet.github.io/docfx/

## Basit yöntem açıklaması
Belgeleme yorumları, tanımladıkları yöntemin veya sınıfın doğrudan üzerine yerleştirilir. Üç eğik çizgi `///` ile başlarlar ve meta bilgilerin XML aracılığıyla depolanmasına izin verirler.

    /// <summary>
    /// Bar method description
    /// </summary>
    public void Bar()
    { 
            
    }

Etiketlerin içindeki bilgiler, IntelliSense gibi hizmetleri sağlamak için Visual Studio ve diğer araçlar tarafından kullanılabilir:

[![Yöntem xml ek açıklama örneği][1]][1]


[1]: https://i.stack.imgur.com/NDAnP.png


Ayrıca [Microsoft'un yaygın belge etiketleri listesine](https://msdn.microsoft.com/en-us/library/5ast78ax.aspx) bakın.

## Belge yorumlarından XML oluşturma
Koddaki dokümantasyon yorumlarından bir XML dokümantasyon dosyası oluşturmak için, 'csc.exe' C# derleyicisi ile '/doc' seçeneğini kullanın.

Visual Studio 2013/2015'te, **Proje** -> **Özellikler** -> **Derleme** -> **Çıktı**'da 'XML dokümantasyon dosyası' onay kutusunu işaretleyin:

[![XML dokümantasyon dosyası][1]][1]

Projeyi oluşturduğunuzda, derleyici tarafından proje adına karşılık gelen bir ada sahip bir XML dosyası üretilecektir (ör. "XMLDocumentation.dll" -> "XMLDocumentation.xml").

Derlemeyi başka bir projede kullandığınızda, XML dosyasının başvurulan DLL ile aynı dizinde olduğundan emin olun.

Bu örnek:

    /// <summary>
    /// Data class description
    /// </summary>
    public class DataClass
    {
        /// <summary>
        /// Name property description
        /// </summary>
        public string Name { get; set; }
    }


    /// <summary>
    /// Foo function
    /// </summary>
    public class Foo
    {
        /// <summary>
        /// This method returning some data
        /// </summary>
        /// <param name="id">Id parameter</param>
        /// <param name="time">Time parameter</param>
        /// <returns>Data will be returned</returns>
        public DataClass GetData(int id, DateTime time)
        {
            return new DataClass();
        }
    }


Bu xml'yi derlemede üretir:

    <?xml version="1.0"?>
    <doc>
        <assembly>
            <name>XMLDocumentation</name>
        </assembly>
        <members>
            <member name="T:XMLDocumentation.DataClass">
                <summary>
                Data class description
                </summary>
            </member>
            <member name="P:XMLDocumentation.DataClass.Name">
                <summary>
                Name property description
                </summary>
            </member>
            <member name="T:XMLDocumentation.Foo">
                <summary>
                Foo function
                </summary>
            </member>
            <member name="M:XMLDocumentation.Foo.GetData(System.Int32,System.DateTime)">
                <summary>
                This method returning some data
                </summary>
                <param name="id">Id parameter</param>
                <param name="time">Time parameter</param>
                <returns>Data will be returned</returns>
            </member>
        </members>
    </doc>

[1]: https://i.stack.imgur.com/tXXQy.png

## Param ve dönüş öğeleriyle yöntem dokümantasyonu yorumu
    /// <summary>
    /// Returns the data for the specified ID and timestamp.
    /// </summary>
    /// <param name="id">The ID for which to get data. </param>
    /// <param name="time">The DateTime for which to get data. </param>
    /// <returns>A DataClass instance with the result. </returns>
    public DataClass GetData(int id, DateTime time)
    {
       // ...
    }

**IntelliSense** size her parametrenin açıklamasını gösterir:

[![parametre yorumu][1]][1]

İpucu: Intellisense, Visual Studio'da görüntülenmezse, ilk köşeli ayracı veya virgülü silin ve ardından yeniden yazın.

[1]: https://i.stack.imgur.com/cH3OQ.png

## Arayüz ve sınıf dokümantasyonu yorumları
    /// <summary>
    /// This interface can do Foo
    /// </summary>
    public interface ICanDoFoo
    {
        // ... 
    }

    /// <summary>
    /// This Bar class implements ICanDoFoo interface
    /// </summary>
    public class Bar : ICanDoFoo
    {
        // ...
    }

**Sonuç**

Arayüz özeti

[![arayüz özeti][1]][1]

sınıf özeti

[![sınıf özeti][2]][2]

[1]: https://i.stack.imgur.com/ExpwI.png
[2]: https://i.stack.imgur.com/730eY.png

## Belgelerde başka bir sınıfa gönderme
`<see>` etiketi başka bir sınıfa bağlanmak için kullanılabilir. Başvurulacak sınıfın adını içermesi gereken 'cref' üyesini içerir. Visual Studio, bu etiketi yazarken Intellsense sağlar ve bu tür referanslar, başvurulan sınıf yeniden adlandırılırken de işlenir.

    /// <summary>
    /// You might also want to check out <see cref="SomeOtherClass"/>.
    /// </summary>
    public class SomeClass
    {
    }
Visual Studio Intellisense açılır pencerelerinde, bu tür referanslar metinde de renkli olarak gösterilecektir.

Genel bir sınıfa başvurmak için aşağıdakine benzer bir şey kullanın:

    /// <summary>
    /// An enhanced version of <see cref="List{T}"/>.
    /// </summary>
    public class SomeGenericClass<T>
    {
    }

