---
title: "Öznitellikler"
slug: "oznitellikler"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Özel bir nitelik oluşturma
    //1) All attributes should be inherited from System.Attribute
    //2) You can customize your attribute usage (e.g. place restrictions) by using System.AttributeUsage Attribute
    //3) You can use this attribute only via reflection in the way it is supposed to be used
    //4) MethodMetadataAttribute is just a name. You can use it without "Attribute" postfix - e.g. [MethodMetadata("This text could be retrieved via reflection")].
    //5) You can overload an attribute constructors
    [System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Class)]
    public class MethodMetadataAttribute : System.Attribute
    {
        //this is custom field given just for an example
        //you can create attribute without any fields
        //even an empty attribute can be used - as marker
        public string Text { get; set; }
    
        //this constructor could be used as [MethodMetadata]
        public MethodMetadataAttribute ()
        {
        }
    
        //This constructor could be used as [MethodMetadata("String")]
        public MethodMetadataAttribute (string text)
        {
            Text = text;
        }
    }

## Bir niteliği okuma
"GetCustomAttributes" yöntemi, üyeye uygulanan bir dizi özel nitelik döndürür. Bu diziyi aldıktan sonra, bir veya daha fazla belirli özniteliği arayabilirsiniz.

    var attribute = typeof(MyClass).GetCustomAttributes().OfType<MyCustomAttribute>().Single();

Veya onlar aracılığıyla yineleyin

    foreach(var attribute in typeof(MyClass).GetCustomAttributes()) {
        Console.WriteLine(attribute.GetType());
    }

'System.Reflection.CustomAttributeExtensions' öğesinden 'GetCustomAttribute' uzantı yöntemi, belirtilen türde özel bir özniteliği alır, herhangi bir 'MemberInfo'ya uygulanabilir.

    var attribute = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute));

`GetCustomattribute` ayrıca, aramak için öznitelik türünü belirtmek için genel imzaya sahiptir.

    var attribute = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>();

Boole argümanı "inherit" bu yöntemlerin her ikisine de iletilebilir. Bu değer "true" olarak ayarlanırsa, öğenin ataları da incelenir.

## bir öznitelik kullanarak
    [StackDemo(Text = "Hello, World!")]
    public class MyClass
    {
        [StackDemo("Hello, World!")]
        static void MyMethod()
        {
        }
    }

## Hata AyıklayıcıGörüntüleme Özelliği
'DebuggerDisplay' Özniteliğinin eklenmesi, üzerine gelindiğinde hata ayıklayıcının sınıfı görüntüleme şeklini değiştirecektir.

`{}` içine sarılmış ifadeler hata ayıklayıcı tarafından değerlendirilecektir. Bu, aşağıdaki örnekteki gibi basit bir özellik veya daha karmaşık mantık olabilir.

    
    [DebuggerDisplay("{StringProperty} - {IntProperty}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    }
    

[![Hata AyıklayıcıGörüntüleme Örneği][1]][1]

Kapanış parantezinden önce `,nq` eklenmesi, bir dizge çıktısı alınırken tırnak işaretlerini kaldırır.

    [DebuggerDisplay("{StringProperty,nq} - {IntProperty}")]
`{}` içinde genel ifadelere izin verilse de bunlar önerilmez. 'DebuggerDisplay' özniteliği, derleme meta verilerine bir dize olarak yazılacaktır. "{}" içindeki ifadelerin geçerliliği kontrol edilmez. Bu nedenle, bazı basit aritmetiklerden daha karmaşık mantık içeren bir "DebuggerDisplay" özelliği C#'da iyi çalışabilir, ancak VB.NET'te değerlendirilen aynı ifade muhtemelen sözdizimsel olarak geçerli olmayacak ve hata ayıklama sırasında bir hata üretecektir.

"DebuggerDisplay"i dilden bağımsız hale getirmenin bir yolu, ifadeyi bir yönteme veya özelliğe yazıp onun yerine onu çağırmaktır.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
       private string DebuggerDisplay()
        {
            return $"{StringProperty} - {IntProperty}"";
        }
    }

`` Debuggerdisplay` '' de sadece bazı özellikleri veya sadece bir kısmı çıktı yapmak ve aynı zamanda nesnenin türünü de hata ayıklama ve denetlerken isteyebilir.
Aşağıdaki örnek, hata ayıklama ortamlarında "DebuggerDisplay" kullanıldığından, yardımcı yöntemi "#if DEBUG" ile çevreler.

    [DebuggerDisplay("{DebuggerDisplay(),nq}")]
    public class AnObject
    {
       public int ObjectId { get; set; }
       public string StringProperty { get; set; }
       public int IntProperty { get; set; }
    
    #if DEBUG
       private string DebuggerDisplay()
        {
            return
                $"ObjectId:{this.ObjectId}, StringProperty:{this.StringProperty}, Type:{this.GetType()}";
        }
        #endif
    }

[1]: http://i.stack.imgur.com/6JjJs.png

## Arayan bilgisi özellikleri
Çağıran bilgi öznitelikleri, çağrılan yönteme çağrı yapan hakkında bilgi aktarmak için kullanılabilir. Deklarasyon şöyle görünür:
    
    using System.Runtime.CompilerServices;

    public void LogException(Exception ex,
                             [CallerMemberName]string callerMemberName = "",
                             [CallerLineNumber]int callerLineNumber = 0,
                             [CallerFilePath]string callerFilePath = "")
    {
        //perform logging
    }

Ve çağrı şöyle görünür:

    public void Save(DBContext context)
    {
        try
        {
            context.SaveChanges();
        }
        catch (Exception ex)
        {
            LogException(ex);
        }
    }
    

Yalnızca ilk parametrenin açıkça "LogException" yöntemine iletildiğine, geri kalanının ise derleme zamanında ilgili değerlerle sağlanacağına dikkat edin.

`callerMemberName` parametresi, çağıran yöntemin adı olan "Save"` değerini alacaktır.

'callerLineNumber' parametresi, 'LogException' yöntem çağrısının hangi satıra yazıldığı numarasını alacaktır.

Ve 'callerFilePath' parametresi, 'Save' yönteminin bildirildiği dosyanın tam yolunu alacaktır.


## Arayüzden bir özniteliği okuma
Sınıflar bir arayüzden öznitelikleri miras almadığından, bir arayüzden öznitelikler elde etmenin basit bir yolu yoktur. Türetilmiş bir sınıfta bir arabirim uygularken veya üyeleri geçersiz kılarken, öznitelikleri yeniden bildirmeniz gerekir.
Dolayısıyla aşağıdaki örnekte çıktı üç durumda da 'True' olacaktır.

    using System;
    using System.Linq;
    using System.Reflection;

    namespace InterfaceAttributesDemo {
        
        [AttributeUsage(AttributeTargets.Interface, Inherited = true)]
        class MyCustomAttribute : Attribute {
            public string Text { get; set; }
        }
        
        [MyCustomAttribute(Text = "Hello from interface attribute")]
        interface IMyClass {
            void MyMethod();
        }
        
        class MyClass : IMyClass {
            public void MyMethod() { }
        }
        
        public class Program {
            public static void Main(string[] args) {
                GetInterfaceAttributeDemo();
            }
            
            private static void GetInterfaceAttributeDemo() {
                var attribute1 = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute), true);
                Console.WriteLine(attribute1 == null); // True
                
                var attribute2 = typeof(MyClass).GetCustomAttributes(true).OfType<MyCustomAttribute>().SingleOrDefault();
                Console.WriteLine(attribute2 == null); // True
                
                var attribute3 = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>(true);
                Console.WriteLine(attribute3 == null); // True
            }
        }
    }

Arayüz özniteliklerini almanın bir yolu, bir sınıf tarafından uygulanan tüm arayüzlerde onları aramaktır.

    var attribute = typeof(MyClass).GetInterfaces().SelectMany(x => x.GetCustomAttributes().OfType<MyCustomAttribute>()).SingleOrDefault();
    Console.WriteLine(attribute == null); // False
    Console.WriteLine(attribute.Text); // Hello from interface attribute


## Eski Özellik
System.Obsolete, bir türü veya daha iyi bir sürümü olan bir üyeyi işaretlemek için kullanılan bir özniteliktir ve bu nedenle kullanılmamalıdır.

    [Obsolete("This class is obsolete. Use SomeOtherClass instead.")]
    class SomeClass
    {
        //
    }

Yukarıdaki sınıfın kullanılması durumunda derleyici "Bu sınıf eskidir. Bunun yerine SomeOtherClass kullanın." uyarısını verecektir.



