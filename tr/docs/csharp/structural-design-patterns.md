---
title: "Yapısal Tasarım Desenleri"
slug: "yapsal-tasarm-desenleri"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Yapısal tasarım kalıpları, nesnelerin ve sınıfların nasıl birleştirilebileceğini ve büyük bir yapı oluşturabileceğini tanımlayan ve varlıklar arasındaki ilişkileri gerçekleştirmenin basit bir yolunu tanımlayarak tasarımı kolaylaştıran kalıplardır. Açıklanan yedi yapısal model vardır. Bunlar: Adaptör, Köprü, Kompozit, Dekoratör, Cephe, Flyweight ve Proxy'dir.

## Adaptör Tasarım Deseni
> [**“Adaptör”**][1] adından da anlaşılacağı gibi, iki
> uyumsuz arayüzler birbirleriyle iletişim kurar.

> **Örneğin:** bir Iphone 8 (veya başka bir Apple ürünü) satın alırsanız, birçok
> adaptörler. Varsayılan arayüz ses jakını desteklemediğinden veya
> USB'yi seçin. Bu adaptörler ile kablolu kulaklık kullanabilir veya normal bir Ethernet kablosu kullanabilirsiniz. Yani *"birbiriyle uyumsuz iki arayüz birbiriyle iletişim kurar"*.

> **Teknik terimlerle bu şu anlama gelir:**
> bir müşterinin beklediği başka bir arayüze sınıflandırın. Adaptör izin
> uyumsuzluk nedeniyle başka türlü çalışamayan sınıflar birlikte çalışır
> arayüzler. Bu kalıba katılan sınıflar ve nesneler
> şunlardır:

**Adaptör deseni 4 öğeden çıkar**
> 1. **ITarget:** Bu, istemci tarafından işlevsellik elde etmek için kullanılan arabirimdir.
> 2. **Adaptee:** İstemcinin istediği ancak arayüzü istemciyle uyumlu olmayan işlevselliktir.
> 3. **İstemci:** Bu, uyarlanan kişinin kodunu kullanarak bazı işlevler elde etmek isteyen sınıftır.
> 4. **Adapter:** Bu, ITarget'ı uygulayacak ve istemcinin çağırmak istediği Adaptee kodunu çağıracak sınıftır.

**UML**

[![buraya resim açıklamasını girin][2]][2]

**İlk kod Örneği (Teorik örnek)**.

    public interface ITarget
    {
        void MethodA();
    }

    public class Adaptee
    {
        public void MethodB()
        {
            Console.WriteLine("MethodB() is called");
        }
    }

    public class Client
    {
        private ITarget target;

        public Client(ITarget target)
        {
            this.target = target;
        }

        public void MakeRequest()
        {
            target.MethodA();
        }
    }  

    public class Adapter : Adaptee, ITarget
    {
        public void MethodA()
        {
            MethodB();
        }
    }

**İkinci kod örneği (Gerçek dünya uygulaması)**

    /// <summary>
    ///  Interface: This is the interface which is used by the client to achieve functionality.
    /// </summary>
    public interface ITarget
    {
        List<string> GetEmployeeList();
    }

    /// <summary>
    /// Adaptee: This is the functionality which the client desires but its interface is not compatible with the client.
    /// </summary>
    public class CompanyEmplyees
    {
        public string[][] GetEmployees()
        {
            string[][] employees = new string[4][];

            employees[0] = new string[] { "100", "Deepak", "Team Leader" };
            employees[1] = new string[] { "101", "Rohit", "Developer" };
            employees[2] = new string[] { "102", "Gautam", "Developer" };
            employees[3] = new string[] { "103", "Dev", "Tester" };

            return employees;
        }
    }

    /// <summary>
    /// Client: This is the class which wants to achieve some functionality by using the adaptee’s code (list of employees).
    /// </summary>
    public class ThirdPartyBillingSystem
    {
        /* 
         * This class is from a thirt party and you do'n have any control over it. 
         * But it requires a Emplyee list to do its work
         */

        private ITarget employeeSource;

        public ThirdPartyBillingSystem(ITarget employeeSource)
        {
            this.employeeSource = employeeSource;
        }

        public void ShowEmployeeList()
        {
            // call the clietn list in the interface
            List<string> employee = employeeSource.GetEmployeeList();

            Console.WriteLine("######### Employee List ##########");
            foreach (var item in employee)
            {
                Console.Write(item);
            }

        }
    }

    /// <summary>
    /// Adapter: This is the class which would implement ITarget and would call the Adaptee code which the client wants to call.
    /// </summary>
    public class EmployeeAdapter : CompanyEmplyees, ITarget
    {
        public List<string> GetEmployeeList()
        {
            List<string> employeeList = new List<string>();
            string[][] employees = GetEmployees();
            foreach (string[] employee in employees)
            {
                employeeList.Add(employee[0]);
                employeeList.Add(",");
                employeeList.Add(employee[1]);
                employeeList.Add(",");
                employeeList.Add(employee[2]);
                employeeList.Add("\n");
            }

            return employeeList;
        }
    }

    /// 
    /// Demo
    /// 
    class Programs
    {
        static void Main(string[] args)
        {
            ITarget Itarget = new EmployeeAdapter();
            ThirdPartyBillingSystem client = new ThirdPartyBillingSystem(Itarget);
            client.ShowEmployeeList();
            Console.ReadKey();
        }
    }

**Ne zaman kullanılır**
- Bir sistemin, uyumsuz olan başka bir sistemin sınıflarını kullanmasına izin ver
Bununla birlikte.
- Yeni ve halihazırda mevcut olan sistem arasında iletişime izin verin.
birbirinden bağımsız
- Ado.Net SqlAdapter, OracleAdapter, MySqlAdapter buna en iyi örnektir.
Adaptör Modeli.


[1]: https://en.wikipedia.org/wiki/Adapter_pattern
[2]: https://i.stack.imgur.com/oYMFy.gif

