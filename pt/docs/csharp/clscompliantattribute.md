---
title: "Atributo compatível com CLS"
slug: "atributo-compativel-com-cls"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Sintaxe
1. [montagem: CLSCompliant(true)]
2. [Conforme CLS(verdadeiro)]

## Parâmetros
| Construtor| Parâmetro|
| ------ | ------ |
| CLSCompliantAttribute(Boolean)| Inicializa uma instância da classe CLSCompliantAttribute com um valor booleano indicando se o elemento de programa indicado é compatível com CLS.|

A Common Language Specification (CLS) é um conjunto de regras básicas para as quais qualquer linguagem direcionada à CLI (linguagem que confirma as especificações da Common Language Infrastructure) deve confirmar para interoperar com outras linguagens compatíveis com CLS.

[Lista de idiomas CLI][1]


[1]: https://en.wikipedia.org/wiki/List_of_CLI_languages

Você deve marcar seu assembly como CLSCompliant na maioria dos casos quando estiver distribuindo bibliotecas. Esse atributo garantirá que seu código possa ser usado por todas as linguagens compatíveis com CLS. Isso significa que seu código pode ser consumido por qualquer linguagem que possa ser compilada e executada em CLR([Common Language Runtime][1])

Quando seu assembly estiver marcado com `CLSCompliantAttribute`, o compilador verificará se seu código viola alguma das regras do CLS e retornará **warning** se for necessário.



[1]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx

## Modificador de acesso ao qual as regras do CLS se aplicam
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
    
As regras para conformidade com CLS aplicam-se apenas a componentes públicos/protegidos.


## Violação da regra CLS: Tipos não assinados / sbyte
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



## Violação da regra CLS: mesma nomenclatura
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

Visual Basic não diferencia maiúsculas de minúsculas

## Violação da regra CLS: Identificador _
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


Você não pode iniciar a variável com _


## Violação da regra CLS: Herdar de uma classe não CLSComplaint
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



