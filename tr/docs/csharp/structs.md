---
title: "yapılar"
slug: "yaplar"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

Sınıflardan farklı olarak, bir "struct" bir değer türüdür ve *varsayılan olarak* yönetilen öbek üzerinde değil, yerel yığında oluşturulur. Bu, belirli yığın kapsam dışına çıktığında, "yapı"nın tahsisinin kaldırıldığı anlamına gelir. GC, artık "yapı" tarafından referans alınmadıklarını belirlediğinde, tahsisi kaldırılmış "yapı"ların içerilen referans türleri de süpürülür.

'struct' lar kalıtımla devralamazlar ve kalıtım için temel olamazlar, örtük olarak mühürlenirler ve ayrıca 'korumalı' üyeler içeremezler. Ancak, bir "struct", sınıfların yaptığı gibi bir arabirim uygulayabilir.

## Bir yapı bildirmek
    public struct Vector 
    {
        public int X;
        public int Y;
        public int Z;
    }

    public struct Point
    {
        public decimal x, y;
        
        public Point(decimal pointX, decimal pointY)
        {
            x = pointX;
            y = pointY;
        }
    }

- "struct" örnek alanları, parametreli bir kurucu aracılığıyla veya "struct" inşasından sonra ayrı ayrı ayarlanabilir.
- Özel üyeler yalnızca kurucu tarafından başlatılabilir.
- 'struct', örtük olarak System.ValueType'tan miras alan kapalı bir türü tanımlar.
- Yapılar başka bir türden miras alamazlar, ancak arabirimleri uygulayabilirler.
- Yapılar atamada kopyalanır, yani tüm veriler yeni örneğe kopyalanır ve bunlardan birinde yapılan değişiklikler diğeri tarafından yansıtılmaz.
- Bir yapı "null" olamaz, ancak null yapılabilir bir tür olarak *kullanılabilir:

       Vector v1 = null; //illegal
       Vector? v2 = null; //OK
       Nullable<Vector> v3 = null // OK

- Yapılar, 'yeni' operatörü kullanılarak veya kullanılmadan somutlaştırılabilir.

       //Both of these are acceptable
       Vector v1 = new Vector();
       v1.X = 1;
       v1.Y = 2;
       v1.Z = 3;
    
       Vector v2;
       v2.X = 1;
       v2.Y = 2;
       v2.Z = 3;

    However, the `new` operator must be used in order to use an initializer:

       Vector v1 = new MyStruct { X=1, Y=2, Z=3 }; // OK
       Vector v2 { X=1, Y=2, Z=3 }; // illegal

Bir yapı, birkaç istisna dışında, bir sınıfın bildirebileceği her şeyi bildirebilir:
- Bir yapı parametresiz bir kurucu bildiremez. "struct" örnek alanları, parametreli bir kurucu aracılığıyla veya "struct" inşasından sonra ayrı ayrı ayarlanabilir. Özel üyeler yalnızca kurucu tarafından başlatılabilir.
- Bir yapı, örtük olarak mühürlendiğinden üyeleri korumalı olarak ilan edemez.
- Struct alanları yalnızca const veya static ise başlatılabilir.

## Yapı kullanımı
**Yapıcı ile:**

    Vector v1 = new Vector();
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Vector v1 = new Vector();
    //v1.X is not assigned
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=0,Y=2,Z=3

    Point point1 = new Point();
    point1.x = 0.5;
    point1.y = 0.6;
    
    Point point2 = new Point(0.5, 0.6);

**Yapıcı olmadan:**

    Vector v1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    //Output ERROR "Use of possibly unassigned field 'X'

    Vector v1;
    v1.X = 1;
    v1.Y = 2;
    v1.Z = 3;
    
    Console.WriteLine("X = {0}, Y = {1}, Z = {2}",v1.X,v1.Y,v1.Z);
    // Output X=1,Y=2,Z=3

    Point point3;
    point3.x = 0.5;
    point3.y = 0.6;

Yapıcı ile bir yapı kullanırsak, atanmamış alanla ilgili sorun yaşamayacağız (atanmamış her alanın boş değeri vardır).

Sınıflardan farklı olarak, bir yapının oluşturulması gerekmez, yani yapıcılardan birini çağırmanız gerekmedikçe new anahtar sözcüğünü kullanmanıza gerek yoktur. Bir yapı, değer türü olduğundan ve bu nedenle boş olamayacağından new anahtar sözcüğünü gerektirmez.

## Yapı uygulama arayüzü
    public interface IShape
    {
        decimal Area();
    }
    
    public struct Rectangle : IShape
    {
        public decimal Length { get; set; }
        public decimal Width { get; set; }
    
        public decimal Area()
        {
            return Length * Width;
        }
    }

## Yapılar atamada kopyalanır
Sinse yapıları, tüm veriler atama sırasında _kopyalanan_ değer türleridir ve yeni kopyada yapılacak herhangi bir değişiklik, orijinal kopyanın verilerini değiştirmez. Aşağıdaki kod parçacığı, "p1" öğesinin "p2"ye _kopyalandığını ve "p1" üzerinde yapılan değişikliklerin "p2" örneğini etkilemediğini gösterir.

    var p1 = new Point {
        x = 1,
        y = 2
    };
    
    Console.WriteLine($"{p1.x} {p1.y}"); // 1 2
    
    var p2 = p1;
    Console.WriteLine($"{p2.x} {p2.y}"); // Same output: 1 2
    
    p1.x = 3;
    Console.WriteLine($"{p1.x} {p1.y}"); // 3 2
    Console.WriteLine($"{p2.x} {p2.y}"); // p2 remain the same: 1 2

