---
title: "Unit Testing Best Practices"
slug: "unit-testing-best-practices"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

A unit test is the smallest testable part of an application like functions, classes, procedures, interfaces. Unit testing is a method by which individual units of source code are tested to determine if they are fit for use.
Unit tests are basically written and executed by software developers to make sure that code meets its design and requirements and behaves as expected.



## MakeSut concept
Testcode has the same quality demands, as production code.
MakeSut()  
- improves readability 
- can be easily refactored 
- perfectly supports dependency injection.

Here's the concept:

    [Test]
    public void TestSomething()
    {
        var sut = MakeSut();
        
        string result = sut.Do();
        Assert.AreEqual("expected result", result);
    }

The simplest MakeSut() just returns the tested class:

    private ClassUnderTest MakeSUT()
    {
        return new ClassUnderTest();
    }

When dependencies are needed, they can be injected here:

    private ScriptHandler MakeSut(ICompiler compiler = null, ILogger logger = null, string scriptName="", string[] args = null)
    {
        //default dependencies can be created here
        logger = logger ?? MockRepository.GenerateStub<ILogger>();
        ...
    }

One might say, that MakeSut is just a simple alternative for setup and teardown methods provided by Testrunner frameworks and might cosider these methods a better place for test specific setup and teardown.

Everybody can decide on her own, which way to use. For me MakeSut() provides better readability and much more flexibility. Last but not least, the concept is independent from any testrunner framework.


## Good Naming
The importance of good naming, can be best illustrated by some bad examples:

    [Test]
    Test1() {...} //Cryptic name - absolutely no information 
    
    [Test]
    TestFoo() {...} //Name of the function - and where can I find the expected behaviour?

    [Test]
    TestTFSid567843() {...} //Huh? You want me to lookup the context in the database?

Good tests need goodn names. Good test do not test methods, the test scenarios or requirements.

Good naming also provides information about context and expected behaviour. Ideally, when the test fails on your build machine, you should be able to decide what is wrong, without looking at the test code, or even harder, having the necessity to debug it.

Good naming spares you time for reading code and debugging:

    [Test]
    public void GetOption_WithUnkownOption_ReturnsEmptyString() {...}
    [Test]
    public void GetOption_WithUnknownEmptyOption_ReturnsEmptyString() {...}
    
For beginners it may be helpful to start the test name with **EnsureThat_** or similar prefix. Start with an "EnsureThat_" helps to begin thinking about the scenario or requirement, that needs a test:

    [Test]
    public void EnsureThat_GetOption_WithUnkownOption_ReturnsEmptyString() {...}
    [Test]
    public void EnsureThat_GetOption_WithUnknownEmptyOption_ReturnsEmptyString() {...}


Naming is important for test fixtures too. Name the test fixture after the class being tested:

    [TestFixture]
    public class OptionsTests //tests for class Options
    {
        ...
    }


The final conclusion is:

Good naming leads to good tests which leads to good design in production code.

## From simple to complex
Same as, with writing classes - start with the simple cases, then add requirement (aka tests)  and implementation (aka production code) case by case:

    [Test]
    public void EnsureThat_IsLeapYearIfDecimalMultipleOf4() {...}
    [Test]
    public void EnsureThat_IsNOTLeapYearIfDecimalMultipleOf100 {...}
    [Test]
    public void EnsureThat_IsLeapYearIfDecimalMultipleOf400 {...}
    

Don't forget the refactoring step, when finished with requirements - first refactor the code, then refactor the tests

When finished, you should have a complete, up to date  and READABLE documentation of your class.


