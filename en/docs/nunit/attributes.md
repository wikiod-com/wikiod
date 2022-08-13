---
title: "Attributes"
slug: "attributes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Version 1 of NUnit used the classic approach to identifying tests based on inheritance and naming conventions. From version 2.0 on, NUnit has used custom attributes for this purpose.

Because NUnit test fixtures do not inherit from a framework class, the developer is free to use inheritance in other ways. And because there is no arbitrary convention for naming tests, the choice of names can be entirely oriented toward communicating the purpose of the test.

All NUnit attributes are contained in the NUnit.Framework namespace. Each source file that contains tests must include a using statement for that namespace and the project must reference the framework assembly, nunit.framework.dll.

Beginning with NUnit 2.4.6, NUnit's attributes are no longer sealed and any attributes that derive from them will be recognized by NUnit.

## TestCaseAttributeExample
<!-- language: lang-cs -->
    [TestCase(0, 0, 0)]
    [TestCase(34, 25, 59)]
    [TestCase(-1250, 10000, 8750)]
    public void AddNumbersTest(int a, int b, int expected)
    {
        // Act
        int result = a + b;
                
        // Assert
        Assert.That(result, Is.EqualTo(expected));
    }

## TestFixture
<!-- language: lang-cs -->
    [TestFixture]
    public class Tests {
        
        [Test]
        public void Test1() {
            Assert.That(true, Is.EqualTo(true));
        }
    
    }

A test fixture marks a class as containing tests. 

## TestFixtureSetUp
This attribute used t identify a method that is called once to perform setup before any child tests are run. For the new versions we are using *OneTimeSetUp* as the TestFixtureSetUp is obsolete.

[![TestFixtureSetUp is obselete][1]][1]

**OneTimeSetUp**

[![enter image description here][2]][2]

    [OneTimeSetUp]
    public void SetUp()
     {
             
     }


  [1]: https://i.stack.imgur.com/1qotP.png
  [2]: https://i.stack.imgur.com/Gd7vn.png

## TearDown
This attribute is used to identify a method that is called immediately after each tests, it will be called even if there is any error, this is the place we can dispose our objects.

     [TearDown]
     public void CleanAfterEveryTest()
     {
                
     }

## ValuesAttribute
The **ValuesAttribute** is used to specify *a set of values* for an individual parameter of a test method with parameters.
   
 

        [Test]
        public void Sum_Works_Correctly(
            [Values(1, 2, 3)] int x,
            [Values(4, 5)] int y)
        {
            // Arrange
            var calculator = new Calculator();

            // Act
            int result = calculator.Sum(x, y);

            // Assert
            Assert.That(result, Is.EqualTo(x + y));
        }
Here we can see which test cases are run against these values:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/DqIBi.png

