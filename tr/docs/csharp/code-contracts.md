---
title: "Kod Sözleşmeleri"
slug: "kod-sozlesmeleri"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sözdizimi
1. Sözleşme.Gerektirir(Koşul,kullanıcıMesajı)
    
    Contract.Requires<T>(Condition,userMessage)
    
    Contract.Result<T>
    
    Contract.Ensures() 
    
    Contract.Invariants()

.NET, System.Diagnostics ad alanında bulunan ve .NET 4.0'da tanıtılan Sözleşmeler sınıfı aracılığıyla Sözleşmeye Göre Tasarım fikrini destekler. Kod Sözleşmeleri API'si, kodun statik ve çalışma zamanı denetimleri için sınıflar içerir ve bir yöntem içinde ön koşulları, son koşulları ve değişmezleri tanımlamanıza olanak tanır. Ön koşullar, bir yöntemin yürütülebilmesi için parametrelerin yerine getirmesi gereken koşulları, bir yöntemin tamamlanmasından sonra doğrulanan son koşullar ve değişmezler, bir yöntemin yürütülmesi sırasında değişmeyen koşulları tanımlar.

**Neden Kod Sözleşmeleri gereklidir?**

Uygulamanız çalışırken bir uygulamanın sorunlarını takip etmek, tüm geliştiricilerin ve yöneticilerin en önemli endişelerinden biridir. İzleme birçok şekilde gerçekleştirilebilir. Örneğin -

- Uygulamamız üzerinde izleme uygulayabilir ve uygulama çalışırken bir uygulamanın detaylarını alabilirsiniz.

- Uygulamayı çalıştırırken olay günlüğü mekanizmasını kullanabilirsiniz. Mesajlar, Olay Görüntüleyici kullanılarak görülebilir

- Belirli bir zaman aralığından sonra Performans İzleme uygulayabilir ve uygulamanızdan canlı veri yazabilirsiniz.

Kod Sözleşmeleri, bir uygulamadaki sorunları izlemek ve yönetmek için farklı bir yaklaşım kullanır. Bir yöntem çağrısından döndürülen her şeyi doğrulamak yerine, ön koşullar, son koşullar ve yöntemler üzerindeki değişmezler yardımıyla Kod Sözleşmeleri, yöntemlerinize giren ve çıkan her şeyin doğru olduğundan emin olun.

## Son koşullar
    public double GetPaymentsTotal(string name)
    {     
        Contract.Ensures(Contract.Result<double>() >= 0);
     
        double total = 0.0;
     
        foreach (var payment in this._payments) {
            if (string.Equals(payment.Name, name)) {
                total += payment.Amount;
            }
        }
     
        return total;
    }

## Değişmezler
    namespace CodeContractsDemo
    {
        using System;
        using System.Diagnostics.Contracts;
     
        public class Point
        {
            public int X { get; set; }
            public int Y { get; set; }
     
            public Point()
            {
            }
     
            public Point(int x, int y)
            {
                this.X = x;
                this.Y = y;
            }
     
            public void Set(int x, int y)
            {
                this.X = x;
                this.Y = y;
            }
     
            public void Test(int x, int y)
            {
                for (int dx = -x; dx <= x; dx++) {
                    this.X = dx;
                    Console.WriteLine("Current X = {0}", this.X);
                }
     
                for (int dy = -y; dy <= y; dy++) {
                    this.Y = dy;
                    Console.WriteLine("Current Y = {0}", this.Y);
                }
     
                Console.WriteLine("X = {0}", this.X);
                Console.WriteLine("Y = {0}", this.Y);
            }
     
            [ContractInvariantMethod]
            private void ValidateCoordinates()
            {
                Contract.Invariant(this.X >= 0);
                Contract.Invariant(this.Y >= 0);
            }
        }
    }

## Arayüz Üzerinde Sözleşme Tanımlama
    [ContractClass(typeof(ValidationContract))]
    interface IValidation
    {
        string CustomerID{get;set;}
        string Password{get;set;}
    }
     
    [ContractClassFor(typeof(IValidation))]
    sealed class ValidationContract:IValidation
    {
        string IValidation.CustomerID
        {
            [Pure]
            get
            {
                return Contract.Result<string>();
            }
            set
            {
                Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(value), "Customer ID cannot be null!!");
            }
        }
     
        string IValidation.Password
        {
            [Pure]
            get
            {
                return Contract.Result<string>();
            }
            set
            {
                Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(value), "Password cannot be null!!");
            }
        }
    }
     
    class Validation:IValidation
    {
        public string GetCustomerPassword(string customerID)
        {
            Contract.Requires(!string.IsNullOrEmpty(customerID),"Customer ID cannot be Null");
            Contract.Requires<ArgumentNullException>(!string.IsNullOrEmpty(customerID), "Exception!!");
            Contract.Ensures(Contract.Result<string>() != null);
            string password="AAA@1234";
            if (customerID!=null)
            {
                return password;    
            }
            else
            {
                return null;
            }
             
        }
     
        private string m_custID, m_PWD;
     
        public string CustomerID
        {
            get
            {
                return m_custID;
            }
            set
            {
                m_custID = value;
            }
        }
     
        public string Password
        {
            get
            {
                return m_PWD;
            }
            set
            {
                m_PWD = value;
            }
        }
    }

Yukarıdaki kodda, '[ContractClass]' özniteliği ile 'IValidation' adında bir arayüz tanımladık. Bu öznitelik, bir Arayüz için bir sözleşme uyguladığımız bir sınıfın adresini alır. 'ValidationContract' sınıfı, arayüzde tanımlanan özellikleri kullanır ve 'Contract.Requires<T>' kullanarak boş değerleri kontrol eder. "T" bir istisna sınıfıdır.

Ayrıca get erişimcisini "[Pure]" özelliğiyle işaretledik. Pure niteliği, yöntemin veya bir özelliğin, "IValidation" arabiriminin uygulandığı bir sınıfın örnek durumunu değiştirmemesini sağlar.

## Ön koşullar
    namespace CodeContractsDemo
    {
        using System;
        using System.Collections.Generic;
        using System.Diagnostics.Contracts;
     
        public class PaymentProcessor
        {
            private List<Payment> _payments = new List<Payment>();
     
            public void Add(Payment payment)
            {
                Contract.Requires(payment != null);
                Contract.Requires(!string.IsNullOrEmpty(payment.Name));
                Contract.Requires(payment.Date <= DateTime.Now);
                Contract.Requires(payment.Amount > 0);
     
                this._payments.Add(payment);
            }
        }
    }

