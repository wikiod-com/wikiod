---
title: "Döküm"
slug: "dokum"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

*Dönüştürme*, *Dönüştürme* ile aynı şey değildir. `"-1"` dize değerini bir tamsayı değerine (`-1`) dönüştürmek mümkündür, ancak bu, `Convert.ToInt32()` veya `Int32.Parse()` gibi kütüphane yöntemleriyle yapılmalıdır. Doğrudan döküm sözdizimi kullanılarak yapılamaz.

## Bir nesneyi temel türe yayın


## Yayın yapmadan uyumluluğu kontrol etme
Bir değerin türünün belirli bir türü genişletip genişletmediğini veya uygulayıp uygulamadığını bilmeniz gerekiyorsa, ancak onu gerçekten bu tür olarak yayınlamak istemiyorsanız, 'is' operatörünü kullanabilirsiniz.

    if(value is int)
    {
       Console.WriteLine(value + "is an int");
    }

## Açık Döküm
Bir değerin belirli bir türde olduğunu biliyorsanız, bu türün gerekli olduğu bir bağlamda kullanmak için onu açıkça o türe çevirebilirsiniz.

    object value = -1;
    int number = (int) value;
    Console.WriteLine(Math.Abs(number));

"Değer"i doğrudan "Math.Abs()" öğesine geçirmeyi deneseydik, "Math.Abs()" parametre olarak bir "nesne" alan bir aşırı yüklemeye sahip olmadığı için derleme zamanı istisnası alırdık.

"değer" bir "int"e dönüştürülemezse, bu örnekteki ikinci satır bir "InvalidCastException" oluşturur.

## Güvenli Açık Döküm (`as` operatörü)
Bir değerin düşündüğünüz türden olup olmadığından emin değilseniz, 'as' operatörünü kullanarak güvenle yayınlayabilirsiniz. Değer bu türden değilse, elde edilen değer "null" olacaktır.

    object value = "-1";
    int? number = value as int?;
    if(number != null)
    {
        Console.WriteLine(Math.Abs(number.Value));
    }

"boş" değerlerin hiçbir türü olmadığını unutmayın, bu nedenle "as" anahtar kelimesi, herhangi bir "boş" değeri yayınlarken güvenli bir şekilde "null" sonucunu verecektir.

## Örtülü Döküm
Derleyici her zaman o türe dönüştürülebileceğini biliyorsa, bir değer uygun türe otomatik olarak dönüştürülür.

    int number = -1;
    object value = number;
    Console.WriteLine(value);

Bu örnekte, derleyici tüm "int"lerin "nesne"lere dönüştürülebileceğini bildiği için tipik açık döküm sözdizimini kullanmamıza gerek yoktu. Aslında, değişken oluşturmaktan kaçınabilir ve bir "nesne" bekleyen "Console.WriteLine()" argümanı olarak doğrudan "-1"i iletebiliriz.

    Console.WriteLine(-1);

## Açık Sayısal Dönüşümler
Açık döküm operatörleri, birbirlerini genişletmeseler veya uygulamasalar bile, sayısal türlerin dönüşümlerini gerçekleştirmek için kullanılabilir.

    double value = -1.1;
    int number = (int) value;

Hedef tipin orijinal türden daha az kesinliğe sahip olduğu durumlarda kesinliğin kaybolacağını unutmayın. Örneğin, yukarıdaki örnekte çift değer olarak '-1.1', tamsayı olarak '-1' olur.

Ayrıca, sayısal dönüştürmeler derleme zamanı türlerine dayanır, bu nedenle sayısal türler nesnelere "kutulanmışsa" çalışmazlar.

    object value = -1.1;
    int number = (int) value; // throws InvalidCastException


## Dönüşüm Operatörleri
C#'ta, türler, açık veya örtük yayınlar kullanılarak değerlerin diğer türlere ve diğer türlerden dönüştürülmesine izin veren özel *Dönüşüm Operatörleri* tanımlayabilir. Örneğin, bir JavaScript ifadesini temsil etmesi amaçlanan bir sınıf düşünün:

    public class JsExpression
    {
        private readonly string expression;
        public JsExpression(string rawExpression)
        {
            this.expression = rawExpression;
        }
        public override string ToString()
        {
            return this.expression;
        }
        public JsExpression IsEqualTo(JsExpression other)
        {
            return new JsExpression("(" + this + " == " + other + ")");
        }
    }

İki JavaScript değerinin karşılaştırmasını temsil eden bir JsExpression oluşturmak istiyorsak, şöyle bir şey yapabiliriz:

    JsExpression intExpression = new JsExpression("-1");
    JsExpression doubleExpression = new JsExpression("-1.0");
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Ancak, açık döküm kullanırken basit bir dönüştürmeye izin vermek için "JsExpression"a bazı *açık dönüştürme operatörleri* ekleyebiliriz.

    public static explicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static explicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = (JsExpression)(-1);
    JsExpression doubleExpression = (JsExpression)(-1.0);
    Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

Veya sözdizimini çok daha basit hale getirmek için bu operatörleri *implicit* olarak değiştirebiliriz.

    public static implicit operator JsExpression(int value)
    {
        return new JsExpression(value.ToString());
    }
    public static implicit operator JsExpression(double value)
    {
        return new JsExpression(value.ToString());
    }

    // Usage:
    JsExpression intExpression = -1;
    Console.WriteLine(intExpression.IsEqualTo(-1.0)); // (-1 == -1.0)



## LINQ Döküm işlemleri
Aşağıdaki gibi türleriniz olduğunu varsayalım:

    interface IThing {  }
    class Thing : IThing {  }

LINQ, bir "IEnumerable<>" öğesinin derleme zamanı genel türünü "Enumerable.Cast<>()" ve "Enumerable.OfType<>()" uzantı yöntemleri aracılığıyla değiştiren bir projeksiyon oluşturmanıza olanak tanır.

    IEnumerable<IThing> things = new IThing[] {new Thing()};
    IEnumerable<Thing> things2 = things.Cast<Thing>();
    IEnumerable<Thing> things3 = things.OfType<Thing>();

'things2' değerlendirildiğinde, 'Cast<>()' yöntemi, 'things' içindeki tüm değerleri 'Thing'lere dönüştürmeye çalışacaktır. Atılamayan bir değerle karşılaşırsa, bir `InvalidCastException' atılır.

'things3' değerlendirildiğinde, 'OfType<>()' yöntemi aynı şeyi yapacaktır, ancak, eğer dönüştürülemeyen bir değerle karşılaşırsa, bir istisna atmak yerine bu değeri atlayacaktır.

Bu yöntemlerin genel türü nedeniyle, Dönüştürme Operatörlerini çağıramazlar veya sayısal dönüştürmeler gerçekleştiremezler.

    double[] doubles = new[]{1,2,3}.Cast<double>().ToArray(); // Throws InvalidCastException

Geçici çözüm olarak bir `.Select()` içinde bir döküm gerçekleştirebilirsiniz:

    double[] doubles = new[]{1,2,3}.Select(i => (double)i).ToArray();

