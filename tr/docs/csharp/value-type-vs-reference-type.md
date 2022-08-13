---
title: "Değer türü ve Referans türü"
slug: "deger-turu-ve-referans-turu"
draft: false
images: []
weight: 9929
type: docs
toc: true
---

## Sözdizimi
* Referansa göre geçiş: public void Double(ref int numberToDouble) { }

## Giriiş

### Değer türleri

Değer türleri, ikisinin daha basittir. Değer türleri genellikle verilerin kendisini temsil etmek için kullanılır. Bir tamsayı, bir Boolean veya 3B uzaydaki bir nokta, iyi değer türlerinin örnekleridir.

Değer türleri (yapılar), struct anahtar sözcüğü kullanılarak bildirilir. Yeni bir yapının nasıl bildirileceğine ilişkin bir örnek için sözdizimi bölümüne bakın.

Genel olarak konuşursak, değer türlerini bildirmek için kullanılan 2 anahtar kelimemiz var:

- Yapılar
- Numaralandırmalar

### Referans türleri

Referans türleri biraz daha karmaşıktır. Referans türleri, Nesne Yönelimli Programlama anlamında geleneksel nesnelerdir. Bu nedenle, kalıtımı (ve buradaki faydaları) desteklerler ve ayrıca kesinleştiricileri desteklerler.

C#'da genellikle şu referans türlerine sahibiz:
- Sınıflar
- Delegeler
- Arayüzler

Yeni referans türleri (sınıflar), class anahtar sözcüğü kullanılarak bildirilir. Bir örnek için, yeni bir başvuru tipinin nasıl bildirileceğini öğrenmek için sözdizimi bölümüne bakın.

## Büyük Farklılıklar

Referans türleri ve değer türleri arasındaki temel farklar aşağıda görülebilir.

### Yığında değer türleri bulunur, yığında referans türleri bulunur

Bu, ikisi arasında sıklıkla bahsedilen farktır, ancak gerçekte, C#'ta int gibi bir değer türü kullandığınızda, programın bu değişkeni doğrudan bu değere başvurmak için kullanmasıdır. int mine = 0 derseniz, mine değişkeni doğrudan 0'a atıfta bulunur, bu da verimlidir. Bununla birlikte, referans türleri aslında (adından da anlaşılacağı gibi) temel nesneye bir referans içerir, bu C++ gibi diğer dillerdeki işaretçilere benzer.

Bunun etkilerini hemen fark etmeyebilirsiniz, ancak etkileri vardır, güçlüdür ve incedir. Örnek için başka bir yerde referans türlerini değiştirme örneğe bakın.

Bu fark, aşağıdaki diğer farklılıkların birincil nedenidir ve bilinmeye değerdir.

### Bir yöntemde onları değiştirdiğinizde değer türleri değişmez, referans türleri değişir

Bir değer türü bir yönteme parametre olarak iletildiğinde, yöntem değeri herhangi bir şekilde değiştirirse, değer değişmez. aynı nesneyi kullanan diğer şeyler, orijinal değerleri yerine yeni değiştirilen nesneye sahip olacaktır.

Daha fazla bilgi için yöntemlerde değer türleri ve referans türleri örneğine bakın.

#### Ya onları değiştirmek istersem?

Bunları "ref" anahtar sözcüğünü kullanarak yönteminize iletin ve ardından bu nesneyi referans olarak iletin. Yani hafızadaki aynı nesne. Bu nedenle yaptığınız değişikliklere saygı duyulacaktır. Bir örnek için referans yoluyla geçme örneğine bakın.

### Değer türleri boş olamaz, referans türleri

Söylediği gibi, bir referans türüne null atayabilirsiniz, yani atadığınız değişkenin kendisine atanmış gerçek bir nesnesi olamaz. Ancak değer türleri söz konusu olduğunda bu mümkün değildir. Bununla birlikte, değer türünüzün null yapılabilir olmasına izin vermek için Nullable<Type> kullanabilirsiniz, bu bir gereklilikse, ancak bu düşündüğünüz bir şeyse, bir sınıfın burada en iyi yaklaşım olup olmayacağını kuvvetle düşünün. kendi tipindir.

## ref anahtar sözcüğünü kullanarak referansa göre geçme.

[belgelerden][1] :


> C#'da argümanlar parametrelere ya değere göre ya da
> referans. Başvuruya göre geçmek, işlev üyelerini, yöntemleri,
> değeri değiştirmek için özellikler, dizin oluşturucular, operatörler ve yapıcılar
> parametrelerinin ve bu değişikliğin aramada devam etmesini sağlayın
> çevre. Bir parametreyi referansa göre iletmek için "ref" veya "out" kullanın
> anahtar kelime.

"Ref" ve "out" arasındaki fark, "out"un, geçirilen parametrenin işlev sona ermeden önce atanması gerektiği anlamına gelmesidir. Buna karşılık, "ref" ile iletilen parametreler değiştirilebilir veya değiştirilmeden bırakılabilir.


    using System;

    class Program
    {
        static void Main(string[] args)
        {
            int a = 20;
            Console.WriteLine("Inside Main - Before Callee: a = {0}", a);
            Callee(a);
            Console.WriteLine("Inside Main - After Callee: a = {0}", a);
            
            Console.WriteLine("Inside Main - Before CalleeRef: a = {0}", a);
            CalleeRef(ref a);
            Console.WriteLine("Inside Main - After CalleeRef: a = {0}", a);
         
            Console.WriteLine("Inside Main - Before CalleeOut: a = {0}", a);
            CalleeOut(out a);
            Console.WriteLine("Inside Main - After CalleeOut: a = {0}", a);
            
            Console.ReadLine();
        }
    
        static void Callee(int a)
        {
            a = 5;
            Console.WriteLine("Inside Callee a : {0}", a);
        }
    
        static void CalleeRef(ref int a)
        {
            a = 6;
            Console.WriteLine("Inside CalleeRef a : {0}", a);
        }
        
        static void CalleeOut(out int a)
        {
            a = 7;
            Console.WriteLine("Inside CalleeOut a : {0}", a);
        }
    }

**Çıktı** :

    Inside Main - Before Callee: a = 20
    Inside Callee a : 5
    Inside Main - After Callee: a = 20
    Inside Main - Before CalleeRef: a = 20
    Inside CalleeRef a : 6
    Inside Main - After CalleeRef: a = 6
    Inside Main - Before CalleeOut: a = 6
    Inside CalleeOut a : 7
    Inside Main - After CalleeOut: a = 7

[1]: https://msdn.microsoft.com/en-IN/library/0f66670z.aspx


## Değerleri başka bir yerde değiştirme
<!-- tüm dil: c# -->
```
public static void Main(string[] args)
{
    var studentList = new List<Student>();
    studentList.Add(new Student("Scott", "Nuke"));
    studentList.Add(new Student("Vincent", "King"));
    studentList.Add(new Student("Craig", "Bertt"));

    // make a separate list to print out later
    var printingList = studentList; // this is a new list object, but holding the same student objects inside it

    // oops, we've noticed typos in the names, so we fix those
    studentList[0].LastName = "Duke";
    studentList[1].LastName = "Kong";
    studentList[2].LastName = "Brett";

    // okay, we now print the list
    PrintPrintingList(printingList);
}

private static void PrintPrintingList(List<Student> students)
{
    foreach (Student student in students)
    {
        Console.WriteLine(string.Format("{0} {1}", student.FirstName, student.LastName));
    }
}
```

PrintingList listesi, yazım hatalarından sonra öğrenci adlarında yapılan düzeltmelerden önce yapılmış olsa da, PrintPrintingList yönteminin yine de düzeltilen adları yazdırdığını fark edeceksiniz:

    Scott Duke
    Vincent Kong
    Craig Brett

Bunun nedeni, her iki listenin de aynı öğrencilere yapılan referansların bir listesini içermesidir. SO, temel alınan öğrenci nesnesini değiştirmek, her iki listeye göre kullanımlara yayılır.

İşte öğrenci sınıfının nasıl görüneceği.

```
public class Student
{
    public string FirstName { get; set; }
    public string LastName { get; set; }

    public Student(string firstName, string lastName)
    {
        this.FirstName = firstName;
        this.LastName = lastName;
    }
}
```


## Referansa göre geçme
Yöntemler örneğindeki Değer Türleri ve Referans Türleri'nin düzgün çalışmasını istiyorsanız, yöntemi çağırırken olduğu kadar, referans olarak iletmek istediğiniz parametre için yöntem imzanızdaki ref anahtar sözcüğünü kullanın.
<!-- tüm dil: c# -->

```
public static void Main(string[] args)
{
    ...
    DoubleNumber(ref number); // calling code
    Console.WriteLine(number); // outputs 8
    ...
}
```

```
public void DoubleNumber(ref int number)
{
    number += number;
}
```

Bu değişiklikleri yapmak, sayı güncellemesini beklendiği gibi yapar, yani sayı için konsol çıktısı 8 olur.

## Atama


## ref ve out metod parametreleri arasındaki fark
Bir değer türünü başvuruya göre iletmenin iki olası yolu vardır: "başvuru" ve "çıkış". Aradaki fark, "ref" ile geçirilerek değerin başlatılması gerektiği, ancak "out" ile geçirildiğinde değil. 'out' kullanılması, yöntem çağrısından sonra değişkenin bir değere sahip olmasını sağlar:

    public void ByRef(ref int value)
    {
        Console.WriteLine(nameof(ByRef) + value);
        value += 4;
        Console.WriteLine(nameof(ByRef) + value);
    }

    public void ByOut(out int value)
    {
        value += 4 // CS0269: Use of unassigned out parameter `value'  
        Console.WriteLine(nameof(ByOut) + value); // CS0269: Use of unassigned out parameter `value'  

        value = 4;
        Console.WriteLine(nameof(ByOut) + value);
    }

    public void TestOut()
    {
        int outValue1;
        ByOut(out outValue1); // prints 4

        int outValue2 = 10;   // does not make any sense for out
        ByOut(out outValue2); // prints 4
    }

    public void TestRef()
    {
        int refValue1;
        ByRef(ref refValue1); // S0165  Use of unassigned local variable 'refValue'

        int refValue2 = 0;
        ByRef(ref refValue2); // prints 0 and 4

        int refValue3 = 10;
        ByRef(ref refValue3); // prints 10 and 14
    }

Buradaki sorun, "out" kullanılarak parametrenin yöntemden ayrılmadan önce başlatılması gerektiğidir, bu nedenle aşağıdaki yöntem "ref" ile mümkündür ancak "out" ile mümkün değildir:


    public void EmtyRef(bool condition, ref int value)
    {
        if (condition)
        {
            value += 10;
        }
    }

    public void EmtyOut(bool condition, out int value)
    {
        if (condition)
        {
            value = 10;
        }
    } //CS0177: The out parameter 'value' must be assigned before control leaves the current method

Bunun nedeni, eğer "koşul" geçerli değilse, "değer"in atanmamış olmasıdır.

## ref ve çıkış parametreleri


