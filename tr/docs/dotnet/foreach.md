---
title: "Her biri için"
slug: "her-biri-icin"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

**Hiç kullandınız mı?**

.NET çerçevesinin amacının, sorguların herhangi bir yan etkisi olmaması ve 'ForEach' yönteminin tanım gereği bir yan etkiye neden olduğunu iddia edebilirsiniz. Bunun yerine düz bir "foreach" kullanırsanız, kodunuzu daha sürdürülebilir ve test edilmesi daha kolay bulabilirsiniz.


## IEnumerable için uzantı yöntemi
"ForEach()", "List<T>" sınıfında tanımlanır, ancak "IQueryable<T>" veya "IEnumerable<T>" üzerinde tanımlanmaz. Bu durumlarda iki seçeneğiniz var:

**Önce ToList**

Numaralandırma (veya sorgu), sonuçları yeni bir listeye kopyalayarak veya veritabanını çağırarak değerlendirilecektir. Yöntem daha sonra her öğe üzerinde çağrılır.
    
    IEnumerable<Customer> customers = new List<Customer>();
    
    customers.ToList().ForEach(c => c.SendEmail());
    
Bir ara liste oluşturulduğundan, bu yöntemin bariz bellek kullanımı ek yükü vardır.

**Uzantı yöntemi**

Bir uzatma yöntemi yazın:

    public static void ForEach<T>(this IEnumerable<T> enumeration, Action<T> action)
    {
        foreach(T item in enumeration)
        {
            action(item);
        }
    }

Kullanmak:

    IEnumerable<Customer> customers = new List<Customer>();

    customers.ForEach(c => c.SendEmail());
    
Dikkat: Çerçevenin LINQ yöntemleri, *saf* olma niyetiyle tasarlanmıştır, yani yan etkiler üretmezler. ForEach yönteminin tek amacı yan etkiler yaratmaktır ve bu yönüyle diğer yöntemlerden ayrılmaktadır. Bunun yerine sadece düz bir "foreach" döngüsü kullanmayı düşünebilirsiniz.

## Listedeki bir nesne üzerinde metot çağırma
    public class Customer {
       public void SendEmail()
       {
           // Sending email code here
       }
    }
    
    List<Customer> customers = new List<Customer>();
    
    customers.Add(new Customer());
    customers.Add(new Customer());

    customers.ForEach(c => c.SendEmail());

