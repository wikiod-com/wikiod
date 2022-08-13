---
title: "The general rules for unit testing for all languages"
slug: "the-general-rules-for-unit-testing-for-all-languages"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

When starting with unit-testing all kinds of questions come up:

What is unit-testing? What is a SetUp and TearDown? How do I deal with dependencies? Why do unit-testing at all? How do I make good unit tests?

This article will answer all these questions, so you can start unit-testing in any language you want.

**What is unit testing?**
-------------------------

Unit testing is the testing of code to ensure that it performs the task that it is meant to perform. It tests code at the very lowest level possible - the individual methods of your classes.

**What is a unit?**
-------------------

Any discrete module of code that can be tested in isolation. Most of the time classes and their methods. This class is generally referred to as the "Class Under Test" (CUT) or the "System Under Test" (SUT)

**The difference between unit testing and integration testing**
---------------------------------------------------------------

Unit testing is the act of testing a single class in isolation, completely apart from any of its actually dependencies.
Integration testing is the act of testing a single class along with one or more of its actual dependencies.

**The SetUp and TearDown**
--------------------------

When made the SetUp method is run before every unit test and the TearDown after every test.

In general you add all prerequisite steps in the SetUp and all the clean-up steps in the TearDown. But you only make these method if these steps are needed for every test. If not, than these steps are taken within the specific tests in the "arrange" section.

**How to deal with dependencies**
---------------------------------

Many times a class has the dependency of other classes to execute its methods. To be able to not depend on these other classes, you have to fake these. You can make these classes yourself or use an isolation or mockup framework.
An isolation framework is a collection of code that enables the easy creation of fake classes.

**Fake classes**
----------------

Any class that provides functionality sufficient to pretend that it is a dependency needed by a CUT.
There are two types of fakes: Stubs and Mocks.

 - A stub: A fake that has no effect on the passing or failing of the test and that exists purely to allow the test to run.
 - A mock: A fake that keeps track of the behavior of the CUT and passes or fails the test based on that behavior.

**Why do unit testing?**
------------------------

**1. Unit testing will find bugs**

> When you write a full suite of tests that define what the expected
> behavior is for a given class, anything that isn't behaving as
> expected is revealed.

**2. Unit testing will keep bugs away**

> Make a change that introduces a bug and your tests can reveal it the
> very next time you run your tests.

**3. Unit testing saves time**

> Writing unit tests helps ensure that your code is working as designed
> right from the start. Unit tests define what your code should do and
> thus you won't be spending time writing code that does things it
> shouldn’t do. No one checks in code that they don't believe works and
> you have to do something to make yourself think that it works. Spend
> that time to write unit tests.

**4. Unit testing gives peace of mind**

> You can run all those tests and know that your code works as it is
> supposed to. Knowing the state of your code, that it works, and that
> you can update and improve it without fear is a very good thing.

**5. Unit testing documents the proper use of a class**

> Unit tests become simple examples of how your code works, what it is
> expected to do and the proper way to use your code being tested.

**General rules for unit testing**
----------------------------------

**1. For the structure of a unit test, follow the AAA rule**

> *Arrange:*
> 
> Set up thing to be tested. Like variables, fields and properties to enable the test to be run as well as the expected result.
>     
> *Act:*
>      Actually call the method you're testing
>     
> *Assert:*
> 
> Call the testing framework to verify that the result of your "act" is
> what was expected.

**2. Test one thing at the time in isolation**

> All classes should be tested in isolation. They shouldn’t depend on
> anything other than the mocks and stubs. They shouldn’t depend on the
> results of other tests.

**3. Write simple "right down the middle" tests first**

> The first tests you write should be the simplest tests. They should be
> the ones that basically and easily illustrate the functionality you
> are trying to write. Then, once those tests pass, you should start
> write the more complicated tests that test the edges and boundaries of
> your code.

**4. Write tests that test the edges**

> Once the basics are tested and you know your basic functionality
> works, you should test the edges. A good set of tests will explore the
> outer edges of what might happen to a given method.
> 
>For example:
>- What happens if an overflow occurs?
>- What if values go to zero or below?
>- What if they go to MaxInt or MinInt?
>- What if you create an arc of 361 degrees?
>- What happens if you pass an empty string?
>- What happens if a string is 2GB in size?

 **5. Test across boundaries**

> Unit tests should test both sides of a given boundary. Moving across
> boundaries are places where your code might fail or perform in
> unpredictable ways.

**6. If you can, test the entire spectrum**

> If it's practical, test the entire set of possibilities of for your
> functionality. If it involves an enumerated type, test the
> functionality with every one of the items in the enumeration. It might
> be impractical to test every possibility, but if you can test every
> possibility, do it.

 **7. If possible, cover every code path**

> This one is challenging as well, but if your code is designed for
> testing, and you make use of a code coverage tool, you can ensure that
> every line of your code is covered by unit tests at least once.
> Covering every code path won’t guarantee that there aren’t any bugs,
> but it surely gives you valuable information about the state of every
> line of code.

**8. Write tests that reveal a bug, then fix it**

> If you find a bug, write a test that reveals it. Then, you van easily
> fix the bug by debugging the test. Then you have a nice regression
> test to make sure that if the bug comes back for any reason, you'll
> know right away. It's really easy to fix a bug when you have a simple,
> straight forward test to run in the debugger.
> 
> A side benefit here is that you've tested your test. Because you’ve
> seen the test fail and then when you have seen it pass, you know that
> the test is valid in that it has been proven to work correctly. This
> makes it an even better regression test.

**9. Make each test independent of each other**

> Tests should never depend on each other. If your tests have to run in
> a certain order, you need to change the tests.

**10. Write one assert per test**

> You should write one assert per test. If you can’t do that, then
> refractor your code so your SetUp and TearDown events are used to
> correctly create the environment so that each test can be run
> individually.

**11. Name your tests clearly. Don’t be afraid of long names**

> Since you’re doing one assert per test, each test can end up being
> very specific. Thus, don’t be afraid to use long, complete test names.
> 
> A long complete name lets you know immediately what test failed and
> exactly what the test was trying to do.
> 
> Long, clearly named tests can also document your tests. A test named
> "DividedByZeroShouldThrowException" documents exactly what the code
> does when you try to divide by zero.

**12. Test that every raised exception is actually raised**

> If your code raises an exception, then write a test to ensure that
> every exception you raise in fact gets raised when it is supposed to.

**13. Avoid the use of CheckTrue or Assert.IsTrue**

> Avoid checking for a Boolean condition. For instance, instead if
> checking if two things are equal with CheckTrue or Assert.IsTrue, use
> CheckEquals or Assert.IsEqual instead. Why? Because of this:
> 
> CheckTrue (Expected, Actual)
>      This will report something like:
>      "Some test failed: Expected was True but actual result was False."
> 
> This doesn’t tell you anything.
> 
> CheckEquals (Expected, Actual)
> 
> This will tell you something like: "Some test failed: Expected 7 but
> actual result was 3."
> 
> Only use CheckTrue or Assert.IsTrue when your expected value is
> actually a Boolean condition.

**14. Constantly run your tests**

> Run your tests while you are writing code. Your tests should run fast,
> enabling you to run them after even minor changes. If you can’t run
> your tests as part of your normal development process then something
> is going wrong. Unit tests are supposed to run almost instantly. If
> they aren't, it's probably because you aren’t running them in
> isolation.

**15. Run your tests as part of every automated build**

> Just as you should be running test while you develop, they should also
> be an integral part of your continuous integration process. A failed
> test should mean that your build is broken. Don’t let failing tests
> linger. Consider it a build failure and fix it immediately.

## Example of simple unit test in C#
For this example we will test the sum method of a simple calculator.

In this example we will test the application: ApplicationToTest. This one has a class called Calc. This class has a method Sum().

The method Sum() looks like this:

    public void Sum(int a, int b)
    {
        return a + b;
    }

The unit test to test this method looks like this:

    [Testclass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            //Arrange
            ApplicationToTest.Calc ClassCalc = new ApplicationToTest.Calc();
            int expectedResult = 5;

            //Act
            int result = ClassCalc.Sum(2,3);

            //Assert
            Assert.AreEqual(expectedResult, result);
        }
    }

