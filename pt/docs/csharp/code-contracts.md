---
title: "Contratos de código"
slug: "contratos-de-codigo"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Sintaxe
1. Contrato. Requer (Condição, mensagem do usuário)
    
    Contract.Requires<T>(Condition,userMessage)
    
    Contract.Result<T>
    
    Contract.Ensures() 
    
    Contract.Invariants()

O .NET oferece suporte à ideia Design by Contract por meio de sua classe Contracts encontrada no namespace System.Diagnostics e introduzida no .NET 4.0. A API de contratos de código inclui classes para verificações estáticas e de tempo de execução de código e permite definir pré-condições, pós-condições e invariantes em um método. As pré-condições especificam as condições que os parâmetros devem cumprir antes que um método possa ser executado, as pós-condições que são verificadas após a conclusão de um método e as invariantes definem as condições que não mudam durante a execução de um método.

**Por que os contratos de código são necessários?**

O rastreamento de problemas de um aplicativo quando seu aplicativo está em execução é uma das principais preocupações de todos os desenvolvedores e administradores. O rastreamento pode ser realizado de várias maneiras. Por exemplo -

- Você pode aplicar rastreamento em nosso aplicativo e obter os detalhes de um aplicativo quando o aplicativo estiver em execução

- Você pode usar o mecanismo de log de eventos quando estiver executando o aplicativo. As mensagens podem ser vistas usando o Visualizador de Eventos

- Você pode aplicar o Monitoramento de desempenho após um intervalo de tempo específico e gravar dados ao vivo de seu aplicativo.

Os contratos de código usam uma abordagem diferente para rastrear e gerenciar problemas em um aplicativo. Em vez de validar tudo o que é retornado de uma chamada de método, os contratos de código com a ajuda de pré-condições, pós-condições e invariantes nos métodos garantem que tudo que entra e sai de seus métodos esteja correto.

## Pós-condições
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

## Definindo Contratos na Interface
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

No código acima, definimos uma interface chamada `IValidation` com um atributo `[ContractClass]`. Este atributo recebe um endereço de uma classe onde implementamos um contrato para uma Interface. A classe `ValidationContract` faz uso de propriedades definidas na interface e verifica os valores nulos usando `Contract.Requires<T>`. `T` é uma classe de exceção.

Também marcamos o acessador get com um atributo `[Pure]`. O atributo pure garante que o método ou uma propriedade não altere o estado da instância de uma classe na qual a interface `IValidation` seja implementada.

## Pré-condições
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

