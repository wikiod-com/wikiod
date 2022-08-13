---
title: "SOLID"
slug: "solid"
draft: false
images: []
weight: 9973
type: docs
toc: true
---

**What is S.O.L.I.D ?**

S.O.L.I.D. is a mnemonic(memory aid) acronym. The Solid principles should help software developers to avoid „code smells“ and should lead to good sourcecode. Good sourcecode means in this context that the sourcecode is easy to extend and maintain. The main focus of the Solid principles are classes

**What to expect:**

Why you should apply S.O.L.I.D

How to apply the five S.O.L.I.D principles (examples)


## SRP - Single Responsibility Principle
The S in S.O.L.I.D stands for Single responsibility principle(SRP).

*Responsibility* means in this context reasons to change, so the principle states that a class should only have one reason to change.

Robert C. Martin stated it (during his lecture at Yale shool of management in 10 Sep 2014) as follows

> You could also say, don't put functions that change for different
> reasons in the same class.

or

> Don't mix concerns in your classes

**Reason to apply the SRP:**

When you change a class you might affect functionality related to other responsibilities of the class. Keeping the responsibilities at a low level minimizes the risk of side effects.

**Bad example**

We have an interface IWallet and a Wallet class which implements the IWallet. The Wallet holds our money and the brand, furthermore should it print our money as string representation. The class is used by 
1. a webservice
2. a textwriter which prints the money in Euros into a textfile.

[![Bad example][1]][1]

The SRP is here violated because we have two concerns:
1. The storing of the money and brand 
2. The representation of the money. 

*C# example code*

    public interface IWallet
    {
        void setBrand(string brand);
        string getBrand();
        void setMoney(decimal money);
        decimal getMoney();
        string printMoney();
    }

    public class Wallet : IWallet
    {
        private decimal m_Money;
        private string m_Brand;

        public string getBrand()
        {
            return m_Brand;
        }

        public decimal getMoney()
        {
            return m_Money;
        }

        public void setBrand(string brand)
        {
            m_Brand = brand;
        }

        public void setMoney(decimal money)
        {
            m_Money = money;
        }

        public string printMoney()
        {
            return m_Money.ToString();
        }
    }

**Good example**

[![Good example][2]][2]

To avoid the violation of the SRP, we removed the `printMoney` method from the Wallet class and placed it into a Printer class. The Printer class is now responsible for the printing and the Wallet is now responsible for the storing of the values.

C# example code

    public interface IPrinter
    {
        void printMoney(decimal money);
    }

    public class EuroPrinter : IPrinter
    {
        public void printMoney(decimal money)
        {
            //print euro
        }
    }

    public class DollarPrinter : IPrinter
    {
        public void printMoney(decimal money)
        {
            //print Dollar
        }
    }

    public interface IWallet
    {
        void setBrand(string brand);
        string getBrand();
        void setMoney(decimal money);
        decimal getMoney();
    }

    public class Wallet : IWallet
    {
        private decimal m_Money;
        private string m_Brand;

        public string getBrand()
        {
            return m_Brand;
        }

        public decimal getMoney()
        {
            return m_Money;
        }

        public void setBrand(string brand)
        {
            m_Brand = brand;
        }

        public void setMoney(decimal money)
        {
            m_Money = money;
        }
    }


  [1]: https://i.stack.imgur.com/sVYV6.png
  [2]: https://i.stack.imgur.com/Wtxxu.png

