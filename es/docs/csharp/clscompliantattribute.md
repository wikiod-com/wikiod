---
title: "CLSCompliantAttribute"
slug: "clscompliantattribute"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Sintaxis
1. [ensamblado: compatible con CLS (verdadero)]
2. [Cumple con CLS (verdadero)]

## Parámetros
| Constructor| Parámetro|
| ------ | ------ |
| CLSCompliantAttribute(Booleano)| Inicializa una instancia de la clase CLSCompliantAttribute con un valor booleano que indica si el elemento de programa indicado es compatible con CLS.|

La especificación de lenguaje común (CLS) es un conjunto de reglas básicas que debe cumplir cualquier idioma que se dirija a la CLI (lenguaje que confirma las especificaciones de infraestructura de lenguaje común) para poder interoperar con otros lenguajes compatibles con CLS.

[Lista de idiomas CLI][1]


[1]: https://en.wikipedia.org/wiki/List_of_CLI_languages

Debe marcar su ensamblado como CLSCompliant en la mayoría de los casos cuando distribuya bibliotecas. Este atributo le garantizará que su código será utilizable por todos los lenguajes compatibles con CLS. Esto significa que su código puede ser consumido por cualquier idioma que se pueda compilar y ejecutar en CLR ([Common Language Runtime] [1])

Cuando su ensamblaje está marcado con `CLSCompliantAttribute`, el compilador verificará si su código viola alguna de las reglas de CLS y devolverá **advertencia** si es necesario.



[1]: https://msdn.microsoft.com/en-us/library/8bs2ecf4(v=vs.110).aspx

## Modificador de acceso al que se aplican las reglas CLS
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
    
Las reglas para el cumplimiento de CLS se aplican solo a componentes públicos/protegidos.


## Violación de la regla CLS: tipos sin firmar / sbyte
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



## Violación de la regla CLS: mismo nombre
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

Visual Basic no distingue entre mayúsculas y minúsculas

## Violación de la regla CLS: Identificador _
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


No puede comenzar la variable con _


## Violación de la regla CLS: heredar de la clase que no es CLSComplaint
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



