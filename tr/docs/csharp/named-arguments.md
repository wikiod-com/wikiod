---
title: "Adlandırılmış Argümanlar"
slug: "adlandrlms-argumanlar"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Argüman sırası gerekli değil
Adlandırılmış bağımsız değişkenleri istediğiniz sırada yerleştirebilirsiniz.

Örnek Yöntem:

    public static string Sample(string left, string right)
    {
         return string.Join("-",left,right);
    }

Çağrı Örneği:

    Console.WriteLine (Sample(left:"A",right:"B"));
    Console.WriteLine (Sample(right:"A",left:"B"));

Sonuçlar:

    A-B
    B-A
    


## Adlandırılmış Bağımsız Değişkenler kodunuzu daha net hale getirebilir
Bu basit sınıfı düşünün:

    class SmsUtil
    {
        public bool SendMessage(string from, string to, string message, int retryCount, object attachment)
        {
             // Some code
        }
    }

C# 3.0'dan önce şöyleydi:

    var result = SmsUtil.SendMessage("Mehran", "Maryam", "Hello there!", 12, null);

**adlandırılmış bağımsız değişkenler** ile bu yöntem çağrısını daha da net hale getirebilirsiniz:

    var result = SmsUtil.SendMessage(
        from: "Mehran",
        to:  "Maryam",
        message "Hello there!",
        retryCount: 12,
        attachment: null);


## Adlandırılmış bağımsız değişkenler ve isteğe bağlı parametreler
Adlandırılmış bağımsız değişkenleri isteğe bağlı parametrelerle birleştirebilirsiniz.

Bu yöntemi görelim:

    
    public sealed class SmsUtil
    {
        public static bool SendMessage(string from, string to, string message, int retryCount = 5, object attachment = null)
        {
             // Some code
        }
    }

Bu yöntemi *olmadan* çağırmak istediğinizde 'retryCount' argümanını ayarlayın:


    var result = SmsUtil.SendMessage(
                            from       : "Cihan",
                            to         : "Yakar",
                            message    : "Hello there!",
                            attachment : new object());

## Adlandırılmış Bağımsız Değişkenler, isteğe bağlı parametrelerdeki hataları önler
Yöntem değiştirildiğinde olası hataları önlemek için her zaman isteğe bağlı parametrelere Adlandırılmış Bağımsız Değişkenler kullanın.

    class Employee
    {
        public string Name { get; private set; }

        public string Title { get; set; }

        public Employee(string name = "<No Name>", string title = "<No Title>")
        {
            this.Name = name;
            this.Title = title;
        }
    }

    var jack = new Employee("Jack", "Associate");   //bad practice in this line
Yukarıdaki kod, yapıcı bir gün aşağıdaki gibi değiştirilene kadar derlenir ve düzgün çalışır:

    //Evil Code: add optional parameters between existing optional parameters
    public Employee(string name = "<No Name>", string department = "intern", string title = "<No Title>")
    {
        this.Name = name;
        this.Department = department;
        this.Title = title;
    }
   
    //the below code still compiles, but now "Associate" is an argument of "department"
    var jack = new Employee("Jack", "Associate");

"Takımda başka biri" hata yaptığında hataları önlemek için en iyi uygulama:

    var jack = new Employee(name: "Jack", title: "Associate");



