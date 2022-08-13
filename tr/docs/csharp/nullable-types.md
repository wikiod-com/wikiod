---
title: "Null yapılabilir türler"
slug: "null-yaplabilir-turler"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Sözdizimi
- `Nullable<int> i = 10;`
- int? j = 11;
- int? k = boş;
- DateTime? DateOfBirth = DateTime.now;
- ondalık? Miktar = 1.0m;
- bool? Kullanılabilir = doğru;
- karakter? Harf = 'a';
- (tip)? değişken ismi

Nullable türleri, altta yatan bir türün tüm değerlerini ve `null`u temsil edebilir.

Sözdizimi ** `t?

Null yapılabilir değerler aslında `System.ValueType` nesneleridir, bu nedenle kutulu ve kutusuz olabilirler. Ayrıca, null yapılabilir bir nesnenin "null" değeri, bir başvuru nesnesinin "null" değeriyle aynı değildir, yalnızca bir bayraktır.

Null yapılabilir bir nesne kutulaması yapıldığında, null değer "null" referansına dönüştürülür ve null olmayan değer, null yapılamayan temel türe dönüştürülür.

    DateTime? dt = null;
    var o = (object)dt;
    var result = (o == null); // is true

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var dt2 = (DateTime)dt; // correct cause o contains DateTime value

İkinci kural, doğru, ancak paradoksal koda yol açar:

    DateTime? dt = new DateTime(2015, 12, 11);
    var o = (object)dt;
    var type = o.GetType(); // is DateTime, not Nullable<DateTime>

Kısacası:

    DateTime? dt = new DateTime(2015, 12, 11);
    var type = dt.GetType(); // is DateTime, not Nullable<DateTime>

## Null yapılabilir bir değer başlatılıyor


## Nullable'ın bir değeri olup olmadığını kontrol edin


## Null yapılabilir bir türün değerini alın
Null yapılabilir `int` sonrasında verilir

    int? i = 10;

Varsayılan değerin gerekli olması durumunda, [boş birleştirme operatörü](https://www.wikiod.com/tr/docs/c%23/37/null-coalescing-operator#t=201512031945577057448), "GetValueOrDefault" yöntemini veya atamadan önce null yapılabilir int `HasValue` olup olmadığını kontrol edin.

    int j = i ?? 0;
    int j = i.GetValueOrDefault(0);
    int j = i.HasValue ? i.Value : 0;

Aşağıdaki kullanım her zaman *güvensizdir*. Çalışma zamanında "i" null ise, bir "System.InvalidOperationException" atılır. Tasarım zamanında, bir değer ayarlanmazsa, `Atanmamış yerel değişken 'i' kullanımı' hatası alırsınız.

    int j = i.Value;

## Null yapılabilir bir değerden varsayılan bir değer alma
`.GetValueorDefault ()` yöntemi, `.Hasvalue` özelliği yanlış olsa bile bir değer döndürür (bir istisna atan değer özelliğinin aksine).

    class Program
    {
        static void Main()
        {
            int? nullableExample = null;
            int result = nullableExample.GetValueOrDefault();
            Console.WriteLine(result); // will output the default value for int - 0
            int secondResult = nullableExample.GetValueOrDefault(1);
            Console.WriteLine(secondResult) // will output our specified default - 1
            int thirdResult = nullableExample ?? 1;
            Console.WriteLine(secondResult) // same as the GetValueOrDefault but a bit shorter
        }
    }
______________________
Çıktı:

    0
    1

## Genel bir tür parametrenin nullable tipi olup olmadığını kontrol edin


## Null yapılabilir türlerin varsayılan değeri null
    public class NullableTypesExample
    {
        static int? _testValue;

        public static void Main()
        {
            if(_testValue == null)
                Console.WriteLine("null");
            else
                Console.WriteLine(_testValue.ToString());
        }
    }

Çıktı:

> boş


## Temel Nullable<T> bağımsız değişkeninin etkin kullanımı
Herhangi bir null yapılabilir tür, **genel** bir türdür. Ve herhangi bir null yapılabilir tür, bir **değer** türüdür.

[reflection][2]/kod oluşturma amaçlarıyla ilgili kod oluştururken [Nullable.GetUnderlyingType][1] yönteminin sonucunu **etkili bir şekilde kullanmaya** olanak tanıyan bazı püf noktaları vardır:

    public static class TypesHelper {
        public static bool IsNullable(this Type type) {
            Type underlyingType;
            return IsNullable(type, out underlyingType);
        }
        public static bool IsNullable(this Type type, out Type underlyingType) {
            underlyingType = Nullable.GetUnderlyingType(type);
            return underlyingType != null;
        }
        public static Type GetNullable(Type type) {
            Type underlyingType;
            return IsNullable(type, out underlyingType) ? type : NullableTypesCache.Get(type);
        }
        public static bool IsExactOrNullable(this Type type, Func<Type, bool> predicate) {
            Type underlyingType;
            if(IsNullable(type, out underlyingType))
                return IsExactOrNullable(underlyingType, predicate);
            return predicate(type);
        }
        public static bool IsExactOrNullable<T>(this Type type)
            where T : struct {
            return IsExactOrNullable(type, t => Equals(t, typeof(T)));
        }
    }

Kullanım:

    Type type = typeof(int).GetNullable();
    Console.WriteLine(type.ToString());
    
    if(type.IsNullable())
        Console.WriteLine("Type is nullable.");
    Type underlyingType;
    if(type.IsNullable(out underlyingType))
        Console.WriteLine("The underlying type is " + underlyingType.Name + ".");
    if(type.IsExactOrNullable<int>())
        Console.WriteLine("Type is either exact or nullable Int32.");
    if(!type.IsExactOrNullable(t => t.IsEnum))
        Console.WriteLine("Type is neither exact nor nullable enum.");

Çıktı:

    System.Nullable`1[System.Int32]
    Type is nullable.
    The underlying type is Int32.
    Type is either exact or nullable Int32.
    Type is neither exact nor nullable enum.

not. 'NullableTypesCache' aşağıdaki gibi tanımlanır:

    static class NullableTypesCache {
        readonly static ConcurrentDictionary<Type, Type> cache = new ConcurrentDictionary<Type, Type>();
        static NullableTypesCache() {
            cache.TryAdd(typeof(byte), typeof(Nullable<byte>));
            cache.TryAdd(typeof(short), typeof(Nullable<short>));
            cache.TryAdd(typeof(int), typeof(Nullable<int>));
            cache.TryAdd(typeof(long), typeof(Nullable<long>));
            cache.TryAdd(typeof(float), typeof(Nullable<float>));
            cache.TryAdd(typeof(double), typeof(Nullable<double>));
            cache.TryAdd(typeof(decimal), typeof(Nullable<decimal>));
            cache.TryAdd(typeof(sbyte), typeof(Nullable<sbyte>));
            cache.TryAdd(typeof(ushort), typeof(Nullable<ushort>));
            cache.TryAdd(typeof(uint), typeof(Nullable<uint>));
            cache.TryAdd(typeof(ulong), typeof(Nullable<ulong>));
            //... 
        }
        readonly static Type NullableBase = typeof(Nullable<>);
        internal static Type Get(Type type) {
            // Try to avoid the expensive MakeGenericType method call
            return cache.GetOrAdd(type, t => NullableBase.MakeGenericType(t)); 
        }
    }


[1]: https://msdn.microsoft.com/en-us/library/system.nullable.getunderlyingtype(v=vs.110).aspx
[2]: https://www.wikiod.com/tr/docs/c%23/28/Reflection


