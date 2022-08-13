---
title: "Getting started with nunit"
slug: "getting-started-with-nunit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Why you can’t use Assert.Equals
Ever wondered why you cannot use Assert.Equals() for both Nunit and MSTest. If you have not then maybe as a start you need to be aware that you cannot use this method. Instead you would use Assert.AreEqual() to compare two objects for equality.

The reason here is very simple. Like any class the Assert class is inheriting from System.Object that has a public virtual Equals method meant to check if a given object is equal to the current object. Therefor calling that equals method would be a mistake as in a unit test you would instead to compare two objects that have nothing to do with the Assert class. As a result Nunit and MSTest both chose to provide a method Assert.AreEqual for that purpose.

Furthermore to ensure that you do not use the Equals method by mistake they have decided to throw Exceptions to warn you if you do use this by mistake.

**Nunit Implementation:**

            [EditorBrowsable(EditorBrowsableState.Never)]
        public static new bool Equals(object a, object b)
        {
            // TODO: This should probably be InvalidOperationException
            throw new AssertionException("Assert.Equals should not be used for Assertions");
        }

## TestCaseAttribute
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

[![AddNumbersTest passed][1]][1]


  [1]: http://i.stack.imgur.com/6S8qc.png

## Installation Using NuGet
    Install-Package NUnit

This package includes all assemblies needed to create unit tests. 

Tests can be executed using one of the following methods:
 - Visual Studio Unit Test Window
 - Console runner
 - Third party runner that supports NUnit 3

# Visual Studio Unit Test Window

To execute tests using the Visual Studio Unit Test Window, install the NUnit 3 Test Adapter. https://visualstudiogallery.msdn.microsoft.com/0da0f6bd-9bb6-4ae3-87a8-537788622f2d

# Console Runner

Install the NUnit Console Runner via NuGet

    Install-Package NUnit.Console

The executable nunit3-console.exe is located in packages\NUnit.3.X.X\tools


## Hello World
<!-- language: lang-cs -->
    [TestFixture]
    public class UnitTest1
    {
        class Message
        {
            public string Text { get; } = "Hello World";
        }
    
        [Test]
        public void HelloWorldTest()
        {
            // Act
            var message = new Message();
    
            // Assert
            Assert.That(message.Text, Is.EqualTo("Hello World"));
        }
    }

[![Hello World test][1]][1]


  [1]: http://i.stack.imgur.com/jGqEr.png

