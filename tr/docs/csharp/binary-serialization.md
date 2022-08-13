---
title: "İkili Serileştirme"
slug: "ikili-serilestirme"
draft: false
images: []
weight: 9944
type: docs
toc: true
---

İkili serileştirme motoru, .NET çerçevesinin bir parçasıdır, ancak burada verilen örnekler C#'a özeldir. .NET çerçevesinde yerleşik diğer serileştirme motorlarıyla karşılaştırıldığında, ikili serileştirici hızlı ve verimlidir ve genellikle çalışması için çok az fazladan kod gerektirir. Ancak, kod değişikliklerine daha az toleranslıdır; diğer bir deyişle, bir nesneyi seri hale getirir ve ardından nesnenin tanımında küçük bir değişiklik yaparsanız, muhtemelen doğru şekilde seri durumdan çıkmayacaktır.

## Serileştirme Bağlayıcı
Bağlayıcı, uygulama etki alanınıza hangi türlerin yüklendiğini inceleme fırsatı verir.

SerializationBinder'dan devralınan bir sınıf oluşturun

    class MyBinder : SerializationBinder
    {
        public override Type BindToType(string assemblyName, string typeName)
        {
            if (typeName.Equals("BinarySerializationExample.Item"))
                return typeof(Item);
            return null;
        }
    }


Artık hangi türlerin yüklendiğini kontrol edebilir ve bu temelde gerçekten ne almak istediğimize karar verebiliriz.

Bir bağlayıcı kullanmak için onu BinaryFormatter'a eklemelisiniz.

    object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        binaryFormatter.Binder = new MyBinder();

        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }


Eksiksiz çözüm

    using System;
    using System.IO;
    using System.Runtime.Serialization;
    using System.Runtime.Serialization.Formatters.Binary;
    
    namespace BinarySerializationExample
    {
        class MyBinder : SerializationBinder
        {
            public override Type BindToType(string assemblyName, string typeName)
            {
                if (typeName.Equals("BinarySerializationExample.Item"))
                    return typeof(Item);
                return null;
            }
        }
    
        [Serializable]
        public class Item
        {
            private string _name;
    
            public string Name
            {
                get { return _name; }
                set { _name = value; }
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                var item = new Item
                {
                    Name = "Orange"
                };
    
                var bytes = SerializeData(item);    
                var deserializedData = (Item)DeserializeData(bytes);
            }
    
            private static byte[] SerializeData(object obj)
            {
                var binaryFormatter = new BinaryFormatter();
                using (var memoryStream = new MemoryStream())
                {
                    binaryFormatter.Serialize(memoryStream, obj);
                    return memoryStream.ToArray();
                }
            }
    
            private static object DeserializeData(byte[] bytes)
            {
                var binaryFormatter = new BinaryFormatter
                {
                    Binder = new MyBinder()
                };
    
                using (var memoryStream = new MemoryStream(bytes))
                    return binaryFormatter.Deserialize(memoryStream);
            }
        }
    }



## Serileştirme davranışını niteliklerle kontrol etme
"[NonSerialized]" özniteliğini kullanırsanız, o üye, seri durumdan çıkarmadan sonra her zaman varsayılan değerine sahip olacaktır (ör. "int" için 0, "dize" için null, "bool" için false vb.), nesnenin kendisinde yapılan herhangi bir başlatmanın (yapıcılar, bildirimler, vb.). Telafi etmek için, "[OnSerializing]" (seri durumdan çıkarmadan hemen ÖNCE olarak adlandırılır) ve "[OnDeserialized]" (seri hale getirmeden hemen SONRA adlandırılır) nitelikleri, karşılıkları "[OnSerializing]" ve "[OnSerialized]" ile birlikte sağlanır.

Vektörümüze bir "Derecelendirme" eklemek istediğimizi ve değerin her zaman 1'den başladığından emin olmak istediğimizi varsayalım. Aşağıda yazıldığı gibi, seri hale getirildikten sonra 0 olacaktır:

    [Serializable]
    public class Vector
    {
        public int X;
        public int Y;
        public int Z;
    
        [NonSerialized]
        public decimal Rating = 1M;

        public Vector()
        {
            Rating = 1M;
        }

        public Vector(decimal initialRating)
        {
            Rating = initialRating;
        }
    }

Bu sorunu çözmek için, sınıfın içine aşağıdaki yöntemi ekleyerek onu 1'e ayarlayabiliriz:

    [OnDeserializing]
    void OnDeserializing(StreamingContext context)
    {
        Rating = 1M;
    }

Veya, hesaplanmış bir değere ayarlamak istiyorsak, seri durumdan çıkarmanın bitmesini bekleyebilir ve ardından ayarlayabiliriz:

    [OnDeserialized]
    void OnDeserialized(StreamingContext context)
    {
        Rating = 1 + ((X+Y+Z)/3);
    }

Benzer şekilde, `[OnSerializing]` ve `[OnSerialized]` kullanarak işlerin nasıl yazıldığını kontrol edebiliriz.

## Geriye dönük uyumlulukta bazı sorunlar
Bu küçük örnek, önceden buna dikkat etmezseniz, programlarınızda geriye dönük uyumluluğu nasıl kaybedebileceğinizi gösterir. Ve serileştirme sürecini daha fazla kontrol altına almanın yolları

İlk başta, programın ilk versiyonunun bir örneğini yazacağız:

Versiyon 1


    [Serializable]
    class Data
    {
        [OptionalField]
        private int _version;
        
        public int Version
        {
            get { return _version; }
            set { _version = value; }
        }
    }


Şimdi programın ikinci versiyonunda yeni bir sınıf eklediğini varsayalım. Ve onu bir dizide saklamamız gerekiyor.

Şimdi kod şöyle görünecek:

Versiyon 2


    [Serializable]
    class NewItem
    {
        [OptionalField]
        private string _name;
    
        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }
    
    [Serializable]
    class Data
    {
        [OptionalField]
        private int _version;
    
        public int Version
        {
            get { return _version; }
            set { _version = value; }
        }
    
        [OptionalField]
        private List<NewItem> _newItems;
    
        public List<NewItem> NewItems
        {
            get { return _newItems; }
            set { _newItems = value; }
        }
    }

Ve seri hale getirmek ve seri hale getirmek için kod


    private static byte[] SerializeData(object obj)
    {
        var binaryFormatter = new BinaryFormatter();
        using (var memoryStream = new MemoryStream())
        {
            binaryFormatter.Serialize(memoryStream, obj);
            return memoryStream.ToArray();
        }
    }
    
    private static object DeserializeData(byte[] bytes)
    {
        var binaryFormatter = new BinaryFormatter();
        using (var memoryStream = new MemoryStream(bytes))
            return binaryFormatter.Deserialize(memoryStream);
    }

Peki, v2 programında verileri seri hale getirdiğinizde ve v1 programında bunları seri hale getirmeye çalıştığınızda ne olacak?

Bir istisna alırsınız:

    System.Runtime.Serialization.SerializationException was unhandled
    Message=The ObjectManager found an invalid number of fixups. This usually indicates a problem in the Formatter.Source=mscorlib
    StackTrace:
       at System.Runtime.Serialization.ObjectManager.DoFixups()
       at System.Runtime.Serialization.Formatters.Binary.ObjectReader.Deserialize(HeaderHandler handler, __BinaryParser serParser, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
       at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream, HeaderHandler handler, Boolean fCheck, Boolean isCrossAppDomain, IMethodCallMessage methodCallMessage)
       at System.Runtime.Serialization.Formatters.Binary.BinaryFormatter.Deserialize(Stream serializationStream)
       at Microsoft.Samples.TestV1.Main(String[] args) in c:\Users\andrew\Documents\Visual Studio 2013\Projects\vts\CS\V1 Application\TestV1Part2\TestV1Part2.cs:line 29
       at System.AppDomain._nExecuteAssembly(Assembly assembly, String[] args)
       at Microsoft.VisualStudio.HostingProcess.HostProc.RunUsersAssembly()
       at System.Threading.ExecutionContext.Run(ExecutionContext executionContext, ContextCallback callback, Object state)
       at System.Threading.ThreadHelper.ThreadStart()


Neden? Niye?

ObjectManager, diziler ve başvuru ve değer türleri için bağımlılıkları çözmek için farklı bir mantığa sahiptir. Derlememizde bulunmayan yeni bir referans türü dizisi ekledik.

ObjectManager bağımlılıkları çözmeye çalıştığında grafiği oluşturur. Diziyi gördüğünde hemen düzeltemez, böylece sahte bir referans oluşturur ve daha sonra diziyi düzeltir.

Ve bu tip derlemede olmadığından ve bağımlılıklar düzeltilemez. Nedense düzeltmeler için diziyi öğe listesinden çıkarmaz ve sonunda “IncorrectNumberOfFixups” istisnasını atar.

Serileştirme sürecinde bazı 'getcha'lar. Bazı nedenlerden dolayı, yalnızca yeni başvuru türlerinin dizileri için doğru çalışmaz.

    A Note:
    Similar code will work correctly if you do not use arrays with new classes

Ve bunu düzeltmenin ve uyumluluğu korumanın ilk yolu?

- Sınıflar yerine yeni yapılar koleksiyonu kullanın veya
sözlük (olası sınıflar), çünkü bir sözlük bir koleksiyondur
anahtardeğer çifti (yapısı)
- Eski kodu değiştiremezseniz ISerializable kullanın



## Bir nesneyi seri hale getirilebilir hale getirme
İkili serileştirme için bir nesnenin tamamını işaretlemek için "[Serileştirilebilir]" özniteliğini ekleyin:

    [Serializable]
    public class Vector
    {
        public int X;
        public int Y;
        public int Z;

        [NonSerialized]
        public decimal DontSerializeThis;

        [OptionalField]
        public string Name;
    }

"[NonSerialized]" özelliğini kullanarak açıkça devre dışı bırakmadığımız sürece tüm üyeler seri hale getirilecektir. Örneğimizde `X`, `Y`, `Z` ve `Name` hepsi serileştirilmiştir.

'[NonSerialized]' veya '[OptionalField]' ile işaretlenmedikçe tüm üyelerin seri durumdan çıkarmada hazır bulunmaları gerekir. Örneğimizde, "X", "Y" ve "Z"nin tümü gereklidir ve bunlar akışta mevcut değilse seri durumdan çıkarma başarısız olur. "DontSerializeThis" her zaman "varsayılan(ondalık)" (0'dır) olarak ayarlanacaktır. Akışta "Ad" varsa, bu değere ayarlanır, aksi takdirde "varsayılan(dize)"ye ayarlanır (bu, boştur). "[OptionalField]"in amacı biraz sürüm toleransı sağlamaktır.

## Serileştirme vekilleri (ISerializationSurrogate'i Uygulama)
Bir nesnenin diğerinin serileştirmesini ve serileştirmesini kaldırmasını sağlayan bir serileştirme vekil seçici uygular

Ayrıca, kendisi seri hale getirilemeyen bir sınıfın düzgün şekilde serileştirilmesine veya seri durumdan çıkarılmasına izin verir.


ISerializationSurrogate arabirimini uygulayın

    public class ItemSurrogate : ISerializationSurrogate
    {
        public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
        {
            var item = (Item)obj;
            info.AddValue("_name", item.Name);
        }
    
        public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
        {
            var item = (Item)obj;
            item.Name = (string)info.GetValue("_name", typeof(string));
            return item;
        }
    }

Ardından, bir SurrogateSelector tanımlayıp başlatarak ve bunu IFormatter'ınıza atayarak IFormatter'ınızın vekiller hakkında bilgi sahibi olmasını sağlamalısınız.



    var surrogateSelector = new SurrogateSelector();
    surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());    
    var binaryFormatter = new BinaryFormatter
    {
        SurrogateSelector = surrogateSelector
    };



Sınıf serileştirilebilir olarak işaretlenmemiş olsa bile.

    //this class is not serializable
    public class Item
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }
    }



Eksiksiz çözüm


    using System;
    using System.IO;
    using System.Runtime.Serialization;
    using System.Runtime.Serialization.Formatters.Binary;
    
    namespace BinarySerializationExample
    {
        class Item
        {
            private string _name;
    
            public string Name
            {
                get { return _name; }
                set { _name = value; }
            }
        }
    
        class ItemSurrogate : ISerializationSurrogate
        {
            public void GetObjectData(object obj, SerializationInfo info, StreamingContext context)
            {
                var item = (Item)obj;
                info.AddValue("_name", item.Name);
            }
    
            public object SetObjectData(object obj, SerializationInfo info, StreamingContext context, ISurrogateSelector selector)
            {
                var item = (Item)obj;
                item.Name = (string)info.GetValue("_name", typeof(string));
                return item;
            }
        }
    
        class Program
        {
            static void Main(string[] args)
            {
                var item = new Item
                {
                    Name = "Orange"
                };
    
                var bytes = SerializeData(item);
                var deserializedData = (Item)DeserializeData(bytes);
            }
    
            private static byte[] SerializeData(object obj)
            {
                var surrogateSelector = new SurrogateSelector();
                surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());
    
                var binaryFormatter = new BinaryFormatter
                {
                    SurrogateSelector = surrogateSelector
                };
    
                using (var memoryStream = new MemoryStream())
                {
                    binaryFormatter.Serialize(memoryStream, obj);
                    return memoryStream.ToArray();
                }
            }
    
            private static object DeserializeData(byte[] bytes)
            {
                var surrogateSelector = new SurrogateSelector();
                surrogateSelector.AddSurrogate(typeof(Item), new StreamingContext(StreamingContextStates.All), new ItemSurrogate());
    
                var binaryFormatter = new BinaryFormatter
                {
                    SurrogateSelector = surrogateSelector
                };
    
                using (var memoryStream = new MemoryStream(bytes))
                    return binaryFormatter.Deserialize(memoryStream);
            }
        }
    }



## ISerializable'ı uygulayarak daha fazla kontrol ekleme
Bu, serileştirme, türlerin nasıl kaydedileceği ve yükleneceği üzerinde daha fazla kontrol sahibi olur.

ISerializable arabirimini uygulayın ve derlemek için boş bir kurucu oluşturun

    [Serializable]
    public class Item : ISerializable
    {
        private string _name;

        public string Name
        {
            get { return _name; }
            set { _name = value; }
        }

        public Item ()
        {

        }

        protected Item (SerializationInfo info, StreamingContext context)
        {
            _name = (string)info.GetValue("_name", typeof(string));
        }

        public void GetObjectData(SerializationInfo info, StreamingContext context)
        {
            info.AddValue("_name", _name, typeof(string));
        }
    }



Veri serileştirme için istediğiniz adı ve istediğiniz türü belirleyebilirsiniz.

    info.AddValue("_name", _name, typeof(string));

Veriler seri hale getirildiğinde, istediğiniz türü okuyabileceksiniz.

    _name = (string)info.GetValue("_name", typeof(string));


