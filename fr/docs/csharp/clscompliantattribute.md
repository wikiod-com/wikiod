---
title: "CLSCompliantAttributeCLSCompliantAttribute"
slug: "clscompliantattributeclscompliantattribute"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntaxe
1. [assembly:CLSCompliant(true)]
2. [Conforme CLS (vrai)]

## Paramètres
| Constructeur| Paramètre|
| ------ | ------ |
| CLSCompliantAttribute(Booléen)| Initialise une instance de la classe CLSCompliantAttribute avec une valeur booléenne indiquant si l'élément de programme indiqué est conforme CLS.|

La spécification de langage commun (CLS) est un ensemble de règles de base auxquelles tout langage ciblant la CLI (langage qui confirme les spécifications de l'infrastructure de langage commun) doit confirmer afin d'interagir avec d'autres langages conformes à CLS.

[Liste des langages CLI][1]


[1] : https://en.wikipedia.org/wiki/List_of_CLI_languages

Vous devez marquer votre assembly comme CLSCompliant dans la plupart des cas lorsque vous distribuez des bibliothèques. Cet attribut vous garantira que votre code sera utilisable par tous les langages compatibles CLS. Cela signifie que votre code peut être consommé par n'importe quel langage qui peut être compilé et exécuté sur CLR([Common Language Runtime][1])

Lorsque votre assembly est marqué avec `CLSCompliantAttribute`, le compilateur vérifiera si votre code enfreint l'une des règles CLS et renverra **avertissement** si nécessaire.



[1] : https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx

## Modificateur d'accès auquel les règles CLS s'appliquent
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
    
Les règles de conformité CLS s'appliquent uniquement aux composants publics/protégés.


## Violation de la règle CLS : Types non signés / sbyte
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



## Violation de la règle CLS : Même dénomination
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

Visual Basic n'est pas sensible à la casse

## Violation de la règle CLS : Identifiant _
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


Vous ne pouvez pas démarrer la variable avec _


## Violation de la règle CLS : hériter d'une classe non CLSComplaint
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



