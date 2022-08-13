---
title: "CLSCompliantÖzelliği"
slug: "clscompliantozelligi"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Sözdizimi
1. [derleme:CLSCompliant(true)]
2. [CLS Uyumlu(doğru)]

## Parametreler
| Yapıcı| parametre|
| ------ | ------ |
| CLSCompliantAttribute(Boolean)| Belirtilen program öğesinin CLS uyumlu olup olmadığını gösteren bir Boole değeriyle CLSCompliantAttribute sınıfının bir örneğini başlatır.|

Ortak Dil Belirtimi (CLS), CLI'yi (Ortak Dil Altyapısı belirtimlerini doğrulayan dil) hedefleyen herhangi bir dilin, diğer CLS uyumlu dillerle birlikte çalışabilmesi için onaylaması gereken bir dizi temel kuraldır.

[CLI dillerinin listesi][1]


[1]: https://en.wikipedia.org/wiki/List_of_CLI_languages

Kitaplıkları dağıtırken çoğu durumda derlemenizi CLSCompliant olarak işaretlemeniz gerekir. Bu öznitelik, kodunuzun tüm CLS uyumlu diller tarafından kullanılabileceğini garanti eder. Bu, kodunuzun CLR([Common Language Runtime][1]) üzerinde derlenip çalıştırılabilen herhangi bir dil tarafından tüketilebileceği anlamına gelir.

Montajınız `clscompliantattribute 'ile işaretlendiğinde, derleyici kodunuzun CLS kurallarından herhangi birini ihlal edip etmediğini kontrol eder ve gerektiğinde ** uyarı ** döndürür.



[1]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx

## CLS kurallarının geçerli olduğu Erişim Değiştirici
    using System;
    
    [assembly:CLSCompliant(true)]
    namespace CLSDoc
    {
       
        public class Cat
        {
            internal UInt16 _age = 0;
            private UInt16 _daysTillVacination = 0;
    
            //Warning CS3003  Type of 'Cat.DaysTillVacination' is not CLS-compliant
            protected UInt16 DaysTillVacination
            {
                get { return _daysTillVacination; }
            }
    
            //Warning    CS3003    Type of 'Cat.Age' is not CLS-compliant
            public UInt16 Age
            { get { return _age; } }

            //valid behaviour by CLS-compliant rules
            public int IncreaseAge()
            {
                int increasedAge = (int)_age + 1;
               
                return increasedAge;
            }
    
        }
    }
    
CLS uyumluluğuna ilişkin kurallar yalnızca genel/korunan bileşenler için geçerlidir.


## CLS kuralının ihlali: İmzasız türler / sbyte
    using System;
    
    [assembly:CLSCompliant(true)]
    namespace CLSDoc
    {
       
        public class Car
        {
            internal UInt16 _yearOfCreation = 0;
    
            //Warning CS3008  Identifier '_numberOfDoors' is not CLS-compliant 
            //Warning CS3003  Type of 'Car._numberOfDoors' is not CLS-compliant 
            public UInt32 _numberOfDoors = 0;
    
            //Warning    CS3003    Type of 'Car.YearOfCreation' is not CLS-compliant
            public UInt16 YearOfCreation
            {
                get { return _yearOfCreation; }
            }
    
    
            //Warning CS3002  Return type of 'Car.CalculateDistance()' is not CLS-compliant
            public UInt64 CalculateDistance()
            {
                return 0;
            }
    
            
            //Warning CS3002  Return type of 'Car.TestDummyUnsignedPointerMethod()' is not CLS-compliant 
            public UIntPtr TestDummyUnsignedPointerMethod()
            {
                int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
                UIntPtr ptr = (UIntPtr)arr[0];
    
                
                return ptr;
            }

            //Warning CS3003  Type of 'Car.age' is not CLS-compliant 
            public sbyte age = 120;
    
    
        }
    }



## CLS Kuralının İhlali: Aynı Adlandırma
    using System;
    
    [assembly:CLSCompliant(true)]
    namespace CLSDoc
    {
       
        public class Car
        {
            //Warning    CS3005    Identifier 'Car.CALCULATEAge()' differing only in case is not CLS-compliant
            public int CalculateAge()
            {
                return 0;
            }
    
            public int CALCULATEAge()
            {
                return 0;
            }
    
        }
    }

Visual Basic büyük / küçük harfe duyarlı değildir

## CLS kuralının ihlali: Tanımlayıcı _
    using System;
    
    [assembly:CLSCompliant(true)]
    namespace CLSDoc
    {
       
        public class Car
        {
            //Warning CS3008  Identifier '_age' is not CLS-complian    
            public int _age = 0;    
        }
    
    }


_ ile değişken başlatamazsınız


## CLS kuralının ihlali: CLSComplaint olmayan sınıftan devralma
    using System;
    
    [assembly:CLSCompliant(true)]
    namespace CLSDoc
    {
    
        [CLSCompliant(false)]
        public class Animal
        {
            public int age = 0;
        }
      
        //Warning    CS3009    'Dog': base type 'Animal' is not CLS-compliant
        public class Dog : Animal
        {
        }
    
    }



