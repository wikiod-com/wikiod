---
title: "Getting started with unit-testing"
slug: "getting-started-with-unit-testing"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## A basic unit test
At its simplest, a unit test consists of three stages:
 *  Prepare the environment for the test
 *  Execute the code to be tested
 *  Validate the expected behaviour matches the observed behaviour

These three stages are often called 'Arrange-Act-Assert', or 'Given-When-Then'.

Below is example in C# that uses the [NUnit][1] framework.

    [TestFixture]
    public CalculatorTest
    {
       [Test]
       public void Add_PassSevenAndThree_ExpectTen()
       {
           // Arrange - setup environment
           var systemUnderTest = new Calculator();         

           // Act - Call system under test
           var calculatedSum = systemUnderTest.Add(7, 3);  
           
           // Assert - Validate expected result
           Assert.AreEqual(10, calculatedSum);             
      }
    }

Where necessary, an optional fourth clean up stage tidies up.


  [1]: https://www.wikiod.com/nunit

## A unit test with stubbed dependency
Good unit tests are independent, but code often has dependencies.  We use various kinds of [test doubles][1] to remove the dependencies for testing.  One of the simplest test doubles is a stub.  This is a function with a hard-coded return value called in place of the real-world dependency.

    // Test that oneDayFromNow returns a value 24*60*60 seconds later than current time
    
    let systemUnderTest = new FortuneTeller()       // Arrange - setup environment
    systemUnderTest.setNow(() => {return 10000})    //   inject a stub which will 
                                                    //   return 10000 as the result
    
    let actual = systemUnderTest.oneDayFromNow()    // Act - Call system under test
    
    assert.equals(actual, 10000 + 24 * 60 * 60)     // Assert - Validate expected result

In production code, `oneDayFromNow` would call Date.now(), but that would make for inconsistent and unreliable tests.  So here we stub it out.

  [1]: https://www.wikiod.com/unit-testing/test-doubles


## A unit test with a spy (interaction test)
Classic unit tests test *state*, but it can be impossible to properly test methods whose behavior depends on other classes through state.  We test these methods through *interaction tests*, which verify that the system under test correctly calls its collaborators.  Since the collaborators have their own unit tests, this is sufficient, and actually a better test of the actual responsibility of the tested method.  We don't test that this method returns a particular result given an input, but instead that it correctly calls its collaborator(s).

    // Test that squareOfDouble invokes square() with the doubled value
    
    let systemUnderTest = new Calculator()          // Arrange - setup environment
    let square = spy()
    systemUnderTest.setSquare(square)               //   inject a spy
    
    let actual = systemUnderTest.squareOfDouble(3)  // Act - Call system under test
    
    assert(square.calledWith(6))                    // Assert - Validate expected interaction



## Simple Java+JUnit Test
[JUnit][1] is the leading testing framework used for testing Java code.
       
The class under test models a simple bank account, that charges a penalty when you go overdrawn.

    public class BankAccount {
        private int balance;
    
        public BankAccount(int i){
            balance = i;
        }
    
        public BankAccount(){
            balance = 0;
        }
    
        public int getBalance(){
            return balance;
        }
    
        public void deposit(int i){
            balance += i;
        }
    
        public void withdraw(int i){
            balance -= i;
            if (balance < 0){
                balance -= 10; // penalty if overdrawn
            }
        }
    }

This test class validates the behaviour of some of the `BankAccount` public methods.
        
    import org.junit.Test;
    import static org.junit.Assert.*;
    
    // Class that tests
    public class BankAccountTest{
   
        BankAccount acc;

        @Before                        // This will run **before** EACH @Test
        public void setUptestDepositUpdatesBalance(){
            acc = new BankAccount(100);  
        } 

        @After                        // This Will run **after** EACH @Test
        public void tearDown(){
        // clean up code
        }

        @Test
        public void testDeposit(){
           // no need to instantiate a new BankAccount(), @Before does it for us

            acc.deposit(100);

            assertEquals(acc.getBalance(),200); 
        }
    
        @Test
        public void testWithdrawUpdatesBalance(){    
            acc.withdraw(30);

            assertEquals(acc.getBalance(),70); // pass
        }
    
        @Test
        public void testWithdrawAppliesPenaltyWhenOverdrawn(){
    
            acc.withdraw(120);

            assertEquals(acc.getBalance(),-30);
        }
    }


  [1]: https://www.wikiod.com/junit

## Unit Test with Parameters using NUnit and C#
    using NUnit.Framework;

    namespace MyModuleTests 
    {
        [TestFixture]
        public class MyClassTests
        {
            [TestCase(1, "Hello", true)]
            [TestCase(2, "bye", false)]
            public void MyMethod_WhenCalledWithParameters_ReturnsExpected(int param1, string param2, bool expected)
            {
            //Arrange
            var foo = new MyClass(param1);

            //Act
            var result = foo.MyMethod(param2);

            //Assert
            Assert.AreEqual(expected, result);
            }
        }
    }

## A basic python unit test
    import unittest

    def addition(*args):
        """ add two or more summands and return the sum """
    
        if len(args) < 2:
            raise ValueError, 'at least two summands are needed'
        
        for ii in args: 
            if not isinstance(ii, (int, long, float, complex )):
                raise TypeError

        # use build in function to do the job
        return sum(args) 

Now the test part:

    class Test_SystemUnderTest(unittest.TestCase):
    
        def test_addition(self):
            """test addition function"""

            # use only one summand - raise an error 
            with self.assertRaisesRegexp(ValueError, 'at least two summands'):
                addition(1)
            
            # use None - raise an error
            with self.assertRaises(TypeError):
                addition(1, None)
            
            # use ints and floats 
            self.assertEqual(addition(1, 1.), 2)
    
            # use complex numbers
            self.assertEqual(addition(1, 1., 1+2j), 3+2j)

    if __name__ == '__main__':
        unittest.main()



## An XUnit test with parameters
    using Xunit;

    public class SimpleCalculatorTests
    {
        [Theory]
        [InlineData(0, 0, 0, true)]
        [InlineData(1, 1, 2, true)]
        [InlineData(1, 1, 3, false)]
        public void Add_PassMultipleParameters_VerifyExpected(
            int inputX, int inputY, int expected, bool isExpectedCorrect)
        {
            // Arrange
            var sut = new SimpleCalculator();

            // Act
            var actual = sut.Add(inputX, inputY);

            // Assert
            if (isExpectedCorrect)
            {
                Assert.Equal(expected, actual);
            }
            else
            {
                Assert.NotEqual(expected, actual);
            }
        }
    }

    public class SimpleCalculator
    {
        public int Add(int x, int y)
        {
            return x + y;
        }
    }

