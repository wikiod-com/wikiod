---
title: "Assertion Types"
slug: "assertion-types"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

## Verifying a Returned Value
    [Test]
    public void Calculator_Add_ReturnsSumOfTwoNumbers()
    {
        Calculator calculatorUnderTest = new Calculator();
      
        double result = calculatorUnderTest.Add(2, 3);

        Assert.AreEqual(5, result);
    }

## State Based Testing
Given this simple class, we can test that the `ShaveHead` method is working correctly by asserting state of the `HairLength` variable is set to zero after the `ShaveHead` method is called.

    public class Person
    {
        public string Name;
        public int HairLength;
        
        public Person(string name, int hairLength)
        {
            this.Name = name;
            this.HairLength = hairLength;
        }
        
        public void ShaveHead()
        {
            this.HairLength = 0;
        }
    }

    [Test]
    public void Person_ShaveHead_SetsHairLengthToZero()
    {
        Person personUnderTest = new Person("Danny", 10);
        
        personUnderTest.ShaveHead();
        
        int hairLength = personUnderTest.HairLength;
        
        Assert.AreEqual(0, hairLength);
    }



## Verifying an Exception is Thrown
Sometimes it is necessary to assert when an exception is thrown. Different unit testing frameworks have different conventions for asserting that an exception was thrown, (like NUnit's Assert.Throws method). This example does not use any framework specific methods, just built in exception handling.

    [Test]
    public void GetItem_NegativeNumber_ThrowsArgumentInvalidException
    {
        ShoppingCart shoppingCartUnderTest = new ShoppingCart();
        shoppingCartUnderTest.Add("apple");
        shoppingCartUnderTest.Add("banana");
        
        double invalidItemNumber = -7;
        
        bool exceptionThrown = false;
        
        try
        {
            shoppingCartUnderTest.GetItem(invalidItemNumber);
        }
        catch(ArgumentInvalidException e)
        {
            exceptionThrown = true;
        }
        
        Assert.True(exceptionThrown);
    }

