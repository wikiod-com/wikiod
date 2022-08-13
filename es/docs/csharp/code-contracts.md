---
title: "Código de Contratos"
slug: "codigo-de-contratos"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sintaxis
1. Contrato.Requiere (Condición, mensaje de usuario)
    
    Contract.Requires<T>(Condition,userMessage)
    
    Contract.Result<T>
    
    Contract.Ensures() 
    
    Contract.Invariants()

.NET admite la idea de diseño por contrato a través de su clase de contratos que se encuentra en el espacio de nombres System.Diagnostics y se introdujo en .NET 4.0. Code Contracts API incluye clases para comprobaciones de código estáticas y en tiempo de ejecución y le permite definir condiciones previas, condiciones posteriores e invariantes dentro de un método. Las condiciones previas especifican las condiciones que deben cumplir los parámetros antes de que se pueda ejecutar un método, las condiciones posteriores que se verifican al finalizar un método y las invariantes definen las condiciones que no cambian durante la ejecución de un método.

**¿Por qué se necesitan contratos de código?**

El seguimiento de los problemas de una aplicación cuando se está ejecutando es una de las principales preocupaciones de todos los desarrolladores y administradores. El seguimiento se puede realizar de muchas maneras. Por ejemplo -

- Puede aplicar el rastreo en nuestra aplicación y obtener los detalles de una aplicación cuando la aplicación se está ejecutando

- Puede usar el mecanismo de registro de eventos cuando está ejecutando la aplicación. Los mensajes se pueden ver usando el Visor de eventos

- Puede aplicar la supervisión del rendimiento después de un intervalo de tiempo específico y escribir datos en vivo desde su aplicación.

Code Contracts utiliza un enfoque diferente para rastrear y administrar problemas dentro de una aplicación. En lugar de validar todo lo que se devuelve de una llamada de método, Code Contracts con la ayuda de condiciones previas, condiciones posteriores e invariantes en los métodos, garantiza que todo lo que ingresa y sale de sus métodos sea correcto.

## Postcondiciones
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

## Invariantes
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

## Definición de contratos en la interfaz
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

En el código anterior, hemos definido una interfaz llamada `IValidation` con un atributo `[ContractClass]`. Este atributo toma una dirección de una clase donde hemos implementado un contrato para una Interfaz. La clase `ValidationContract` hace uso de las propiedades definidas en la interfaz y verifica los valores nulos usando `Contract.Requires<T>`. `T` es una clase de excepción.

También hemos marcado el descriptor de acceso get con un atributo `[Pure]`. El atributo puro asegura que el método o una propiedad no cambie el estado de instancia de una clase en la que se implementa la interfaz `IValidation`.

## Condiciones previas
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

