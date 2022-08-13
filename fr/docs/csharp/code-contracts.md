---
title: "Contrats de code"
slug: "contrats-de-code"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Syntaxe
1. Contract.Requires(Condition,userMessage)
    
    Contract.Requires<T>(Condition,userMessage)
    
    Contract.Result<T>
    
    Contract.Ensures() 
    
    Contract.Invariants()

.NET prend en charge l'idée de conception par contrat via sa classe Contracts trouvée dans l'espace de noms System.Diagnostics et introduite dans .NET 4.0. L'API Code Contracts inclut des classes pour les vérifications statiques et d'exécution du code et vous permet de définir des conditions préalables, des postconditions et des invariants au sein d'une méthode. Les préconditions spécifient les conditions que les paramètres doivent remplir avant qu'une méthode puisse s'exécuter, les postconditions qui sont vérifiées à la fin d'une méthode et les invariants définissent les conditions qui ne changent pas pendant l'exécution d'une méthode.

**Pourquoi les contrats de code sont-ils nécessaires ?**

Le suivi des problèmes d'une application lorsque votre application est en cours d'exécution est l'une des principales préoccupations de tous les développeurs et administrateurs. Le suivi peut être effectué de plusieurs façons. Par exemple -

- Vous pouvez appliquer le traçage sur notre application et obtenir les détails d'une application lorsque l'application est en cours d'exécution

- Vous pouvez utiliser le mécanisme de journalisation des événements lorsque vous exécutez l'application. Les messages peuvent être vus à l'aide de l'Observateur d'événements

- Vous pouvez appliquer la surveillance des performances après un intervalle de temps spécifique et écrire des données en direct à partir de votre application.

Code Contracts utilise une approche différente pour le suivi et la gestion des problèmes au sein d'une application. Au lieu de valider tout ce qui est renvoyé à partir d'un appel de méthode, les contrats de code à l'aide de préconditions, de postconditions et d'invariants sur les méthodes garantissent que tout ce qui entre et sort de vos méthodes est correct.

## Postconditions
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

## Invariants
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

## Définir des contrats sur l'interface
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

Dans le code ci-dessus, nous avons défini une interface appelée `IValidation` avec un attribut `[ContractClass]`. Cet attribut prend une adresse d'une classe où nous avons implémenté un contrat pour une Interface. La classe `ValidationContract` utilise les propriétés définies dans l'interface et vérifie les valeurs nulles à l'aide de `Contract.Requires<T>`. 'T' est une classe d'exception.

Nous avons également marqué l'accesseur get avec un attribut `[Pure]`. L'attribut pure garantit que la méthode ou une propriété ne modifie pas l'état de l'instance d'une classe dans laquelle l'interface `IValidation` est implémentée.

## Conditions préalables
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

