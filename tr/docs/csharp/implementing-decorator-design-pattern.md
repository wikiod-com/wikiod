---
title: "Dekoratör Tasarım Modelini Uygulama"
slug: "dekorator-tasarm-modelini-uygulama"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

Dekoratör kullanmanın artıları:
- çalışma zamanında farklı konfigürasyonlarda yeni işlevler ekleyebilirsiniz
- miras için iyi bir alternatif
- müşteri kullanmak istediği konfigürasyonu seçebilir

## Kafeterya simülasyonu
Dekoratör, yapısal tasarım kalıplarından biridir. Nesnenin davranışını eklemek, kaldırmak veya değiştirmek için kullanılır. Bu belge size Decorator DP'yi nasıl doğru şekilde kullanacağınızı öğretecektir.

Bunun fikrini size basit bir örnekle açıklayayım. Şu anda ünlü kahve şirketi Starbobs'ta olduğunuzu hayal edin. Dilediğiniz kahveyi sipariş verebilirsiniz - kremalı ve şekerli, kremalı ve soslu ve çok daha fazla kombinasyonlu! Ancak, tüm içeceklerin temeli kahvedir - koyu, acı bir içecek, değiştirebilirsiniz. Kahve makinesini simüle eden basit bir program yazalım.

İlk olarak, temel içeceğimizi tanımlayan soyut bir sınıf oluşturmamız gerekiyor:

    public abstract class AbstractCoffee
    {
        protected AbstractCoffee k = null;
 
        public AbstractCoffee(AbstractCoffee k)
        {
            this.k = k;
        }
 
        public abstract string ShowCoffee();
    }

Şimdi şeker, süt ve tepesi gibi bazı ekstralar oluşturalım. Oluşturulan sınıflar "AbstractCoffee"yi uygulamalıdır - onu dekore edeceklerdir:

    public class Milk : AbstractCoffee
    {
        public Milk(AbstractCoffee c) : base(c) { }
        public override string ShowCoffee()
        {
            if (k != null)
                return k.ShowCoffee() + " with Milk";
            else return "Milk";
        }
    }
    public class Sugar : AbstractCoffee
    {
        public Sugar(AbstractCoffee c) : base(c) { }
 
        public override string ShowCoffee()
        {
            if (k != null) return k.ShowCoffee() + " with Sugar";
            else return "Sugar";
        }
    }
    public class Topping : AbstractCoffee
    {
        public Topping(AbstractCoffee c) : base(c) { }
 
        public override string ShowCoffee()
        {
            if (k != null) return k.ShowCoffee() + " with Topping";
            else return "Topping";
        }
    }
Şimdi favori kahvemizi oluşturabiliriz:

    public class Program
    {
        public static void Main(string[] args)
        {
            AbstractCoffee coffee = null; //we cant create instance of abstract class
            coffee = new Topping(coffee); //passing null
            coffee = new Sugar(coffee); //passing topping instance
            coffee = new Milk(coffee);  //passing sugar
            Console.WriteLine("Coffee with " + coffee.ShowCoffee());
 
        }
    }
Kodu çalıştırmak aşağıdaki çıktıyı üretecektir:
> Şekerli Sütlü Kahve

