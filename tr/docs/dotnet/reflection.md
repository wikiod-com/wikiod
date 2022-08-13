---
title: "Refleks"
slug: "refleks"
draft: false
images: []
weight: 9898
type: docs
toc: true
---

## Meclis nedir?
Derlemeler, herhangi bir [Common Language Runtime (CLR)][CLR] uygulamasının yapı taşıdır.
Tanımladığınız her tür, yöntemleri, özellikleri ve bayt kodları ile birlikte bir Montaj içinde derlenir ve paketlenir.

    using System.Reflection;

<b></b>
    
    Assembly assembly = this.GetType().Assembly;   
    
Derlemeler kendi kendini belgeler: yalnızca türleri, yöntemleri ve IL kodlarını değil, aynı zamanda hem derleme hem de çalışma zamanında bunları incelemek ve tüketmek için gerekli Meta Verileri içerirler:

    Assembly assembly = Assembly.GetExecutingAssembly();

    foreach (var type in assembly.GetTypes())
    {
        Console.WriteLine(type.FullName);
    }
 
Derlemelerin tam ve benzersiz kimliklerini tanımlayan adları vardır:

    Console.WriteLine(typeof(int).Assembly.FullName);
    // Will print: "mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089"

Bu ad bir 'PublicKeyToken' içeriyorsa, buna *kesin ad* denir. Bir derlemeyi güçlü adlandırma, derlemeyle dağıtılan ortak anahtara karşılık gelen özel anahtarı kullanarak bir imza oluşturma işlemidir. Bu imza, derlemeyi oluşturan tüm dosyaların adlarını ve karmalarını içeren Derleme bildirimine eklenir ve 'PublicKeyToken' adın bir parçası olur. Aynı kesin ada sahip derlemeler aynı olmalıdır; sürüm oluşturmada ve derleme çakışmalarını önlemek için güçlü adlar kullanılır.

[CLR]: https://en.wikipedia.org/wiki/Common_Language_Runtime

## Yansımalı iki nesneyi karşılaştırın
    public class Equatable
    {
        public string field1;

        public override bool Equals(object obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;

            var type = obj.GetType();
            if (GetType() != type)
                return false;

            var fields = type.GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                if (field.GetValue(this) != field.GetValue(obj))
                    return false;

            return true;
        }

        public override int GetHashCode()
        {
            var accumulator = 0;
            var fields = GetType().GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public);
            foreach (var field in fields)
                accumulator = unchecked ((accumulator * 937) ^ field.GetValue(this).GetHashCode());

            return accumulator;
        }
    }

**Not:** bu örnek, basitlik için alan tabanlı bir karşılaştırma yapar (statik alanları ve özellikleri yok sayar)


## Yansıma kullanarak bir T nesnesi nasıl oluşturulur
Varsayılan yapıcıyı kullanma

    T variable = Activator.CreateInstance(typeof(T));


parametreli yapıcıyı kullanma

    T variable = Activator.CreateInstance(typeof(T), arg1, arg2);

## Nesne oluşturma ve yansıma kullanarak özellikleri ayarlama
Diyelim ki Propertua özelliğine sahip bir 'Classy' sınıfımız var.

    public class Classy
    {
        public string Propertua {get; set;}
    }

yansımayı kullanarak 'Propertua'yı ayarlamak için:

    var typeOfClassy = typeof (Classy);
    var classy = new Classy();
    var prop = typeOfClassy.GetProperty("Propertua");
    prop.SetValue(classy, "Value");

## Yansımalı bir numaralandırmanın özniteliğini alma (ve önbelleğe alma)
Nitelikler, numaralandırmalardaki meta verileri belirtmek için yararlı olabilir. Bunun değerini almak yavaş olabilir, bu nedenle sonuçları önbelleğe almak önemlidir.

        private static Dictionary<object, object> attributeCache = new Dictionary<object, object>();

        public static T GetAttribute<T, V>(this V value)
            where T : Attribute
            where V : struct
        {
            object temp;

            // Try to get the value from the static cache.
            if (attributeCache.TryGetValue(value, out temp))
            {
                return (T) temp;
            }
            else
            {
                // Get the type of the struct passed in.
                Type type = value.GetType();   
                FieldInfo fieldInfo = type.GetField(value.ToString());

                // Get the custom attributes of the type desired found on the struct.
                T[] attribs = (T[])fieldInfo.GetCustomAttributes(typeof(T), false);

                // Return the first if there was a match.
                var result = attribs.Length > 0 ? attribs[0] : null;

                // Cache the result so future checks won't need reflection.
                attributeCache.Add(value, result);

                return result;
            }
        }

