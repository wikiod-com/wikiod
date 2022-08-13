---
title: "Operatörün adı"
slug: "operatorun-ad"
draft: false
images: []
weight: 9901
type: docs
toc: true
---

'nameof' operatörü, bir __variable__, __type__ veya __member__ adını, değişmez olarak kodlamadan dize biçiminde almanızı sağlar.

İşlem, derleme zamanında değerlendirilir; bu, bir IDE'nin yeniden adlandırma özelliğini kullanarak başvurulan bir tanımlayıcıyı yeniden adlandırabileceğiniz ve ad dizesinin bununla güncelleneceği anlamına gelir.

## Sözdizimi
- nameof(ifade)

## PropertyChanged olayını yükseltme
**Snippet**

    public class Person : INotifyPropertyChanged
    {
        private string _address;

        public event PropertyChangedEventHandler PropertyChanged;

        private void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }

        public string Address
        {
            get { return _address; }
            set
            {
                if (_address == value)
                {
                    return;
                }

                _address = value;
                OnPropertyChanged(nameof(Address));
            }
        }
    }

    ...

    var person = new Person();
    person.PropertyChanged += (s,e) => Console.WriteLine(e.PropertyName);

    person.Address = "123 Fake Street";
      
**Konsol Çıkışı**

> Adres

## Temel kullanım: Bir değişken adını yazdırma
'nameof' operatörü, bir değişkenin, türün veya üyenin adını, değişmez olarak kodlamadan dize biçiminde almanızı sağlar. İşlem, derleme zamanında değerlendirilir; bu, bir IDE'nin yeniden adlandırma özelliğini kullanarak, başvurulan bir tanımlayıcıyı yeniden adlandırabileceğiniz ve ad dizesinin bununla güncelleneceği anlamına gelir.

    var myString = "String Contents";
    Console.WriteLine(nameof(myString));

çıktı

> dizem

çünkü değişkenin adı "myString". Değişken adını yeniden düzenlemek, dizeyi değiştirir.

Bir başvuru türünde çağrılırsa, "nameof" operatörü, temel alınan nesnenin adını veya tür adını değil, geçerli başvurunun adını döndürür. Örneğin:

    string greeting = "Hello!";
    Object mailMessageBody = greeting;

    Console.WriteLine(nameof(greeting)); // Returns "greeting"
    Console.WriteLine(nameof(mailMessageBody)); // Returns "mailMessageBody", NOT "greeting"!

## Argüman Kontrolü ve Koruma Cümleleri
Tercih

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException(nameof(orderLine));
            ...
        }
    }

Üzerinde

    public class Order
    {
        public OrderLine AddOrderLine(OrderLine orderLine)
        {
            if (orderLine == null) throw new ArgumentNullException("orderLine");
            ...
        }
    }    

'nameof' özelliğinin kullanılması, yöntem parametrelerinin yeniden düzenlenmesini kolaylaştırır.

## Kesinlikle yazılan MVC eylem bağlantıları
Her zamanki gibi gevşek yazılanlar yerine:

    @Html.ActionLink("Log in", "UserController", "LogIn")

Artık güçlü bir şekilde yazılan eylem bağlantılarını yapabilirsiniz:

    @Html.ActionLink("Log in", @typeof(UserController), @nameof(UserController.LogIn))

Şimdi, kodunuzu yeniden düzenlemek ve "UserController.LogIn" yöntemini "UserController.SignIn" olarak yeniden adlandırmak istiyorsanız, tüm dize oluşumlarını arama konusunda endişelenmenize gerek yok. Derleyici işi yapacak.


## PropertyChanged olaylarını işleme
**Snippet**

    public class BugReport : INotifyPropertyChanged
    {
        public string Title { ... }
        public BugStatus Status { ... }
    }

    ...

    private void BugReport_PropertyChanged(object sender, PropertyChangedEventArgs e)
    {
        var bugReport = (BugReport)sender;

        switch (e.PropertyName)
        {
            case nameof(bugReport.Title):
                Console.WriteLine("{0} changed to {1}", e.PropertyName, bugReport.Title);
                break;

            case nameof(bugReport.Status):
                Console.WriteLine("{0} changed to {1}", e.PropertyName, bugReport.Status);
                break;
        }
    }

    ...

    var report = new BugReport();
    report.PropertyChanged += BugReport_PropertyChanged;

    report.Title = "Everything is on fire and broken";
    report.Status = BugStatus.ShowStopper;

**Konsol Çıkışı**

> Başlık Her şey yanıyor ve bozuk olarak değiştirildi
>
> Durum ShowStopper olarak değiştirildi
    

## Genel bir tür parametresine uygulandı
**Snippet**

    public class SomeClass<TItem>
    {
        public void PrintTypeName()
        {
            Console.WriteLine(nameof(TItem));
        }
    }

    ...

    var myClass = new SomeClass<int>();
    myClass.PrintTypeName();

    Console.WriteLine(nameof(SomeClass<int>));

**Konsol Çıkışı**

> TItem
>
> BazıSınıf

## Bir parametre adını yazdırma
**Snippet**

    public void DoSomething(int paramValue)
    {
        Console.WriteLine(nameof(paramValue));
    }

    ...

    int myValue = 10;
    DoSomething(myValue);

**Konsol Çıkışı**

> paramValue

## Nitelikli tanımlayıcılara uygulanır
**Snippet**

    Console.WriteLine(nameof(CompanyNamespace.MyNamespace));
    Console.WriteLine(nameof(MyClass));
    Console.WriteLine(nameof(MyClass.MyNestedClass));
    Console.WriteLine(nameof(MyNamespace.MyClass.MyNestedClass.MyStaticProperty));

**Konsol Çıkışı**
> MyNamespace
>
> Sınıfım
>
> MyNestedClass
>
> MyStaticProperty

