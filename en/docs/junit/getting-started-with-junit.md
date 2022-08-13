---
title: "Getting started with junit"
slug: "getting-started-with-junit"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Catch expected exception
It is possible to easily catch the exception without any `try catch` block.  

<!-- language: lang-java -->   

    public class ListTest {
      private final List<Object> list = new ArrayList<>();

      @Test(expected = IndexOutOfBoundsException.class)
      public void testIndexOutOfBoundsException() {
        list.get(0);
      }
    }

The example above should suffice for simpler cases, when you don't want/need to check the message carried by the thrown exception.

If you want to check information about exception you may want to use try/catch block:

<!-- language: lang-java --> 

    @Test
    public void testIndexOutOfBoundsException() {
        try {
            list.get(0);
            Assert.fail("Should throw IndexOutOfBoundException");
        } catch (IndexOutOfBoundsException ex) {
            Assert.assertEquals("Index: 0, Size: 0", ex.getMessage());
        }
    }

For this example you have to be aware to always add `Assert.fail()` to ensure that test will be failed when no Exception is thrown. 

For more elaborated cases, JUnit has the [`ExpectedException` `@Rule`][1], which can test this information too and is used as follows:

<!-- language: lang-java -->   

    public class SimpleExpectedExceptionTest {
         @Rule
         public ExpectedException expectedException = ExpectedException.none();
    
         @Test
         public void throwsNothing() {
             // no exception expected, none thrown: passes.
         }
    
         @Test
         public void throwsExceptionWithSpecificType() {
             expectedException.expect(NullPointerException.class);

             throw new NullPointerException();
         }

         @Test
         public void throwsExceptionWithSpecificTypeAndMessage() {
             expectedException.expect(IllegalArgumentException.class);
             expectedException.expectMessage("Wanted a donut.");

             throw new IllegalArgumentException("Wanted a donut.");
         }
    }

# Testing exceptions in JUnit5 #

To achieve the same in JUnit 5, you use a [completely new mechanism][2]:

## The **tested** method ##
 
<!-- language: lang-java -->

    public class Calculator {
        public double divide(double a, double b) {
            if (b == 0.0) {
                throw new IllegalArgumentException("Divider must not be 0");
            }
            return a/b;
        }
    }

## The **test** method ##

<!-- language: lang-java -->   
    public class CalculatorTest {
        @Test
        void triangularMinus5() { // The test method does not have to be public in JUnit5
            Calculator calc = new Calculator();

            IllegalArgumentException thrown = assertThrows(
                IllegalArgumentException.class, 
                () -> calculator.divide(42.0, 0.0));
            // If the exception has not been thrown, the above test has failed.

            // And now you may further inspect the returned exception...
            // ...e.g. like this:
            assertEquals("Divider must not be 0", thrown.getMessage());
    }


  [1]: http://junit.org/junit4/javadoc/4.12/org/junit/rules/ExpectedException.html
  [2]: http://junit.org/junit5/docs/current/user-guide/#writing-tests-assertions

## Ignoring Tests
To ignore a test, simply add the `@Ignore` annotation to the test and optionally provide a parameter to the annotation with the reason. 

    @Ignore("Calculator add not implemented yet.")
    @Test
    public void testPlus() {
        assertEquals(5, calculator.add(2,3));
    }

Compared to commenting the test or removing the `@Test` annotation, the test runner will still report this test and note that it was ignored. 

It is also possible to ignore a test case conditionally by using JUnit *assumptions*.
A sample use-case would be to run the test-case only after a certain bug is fixed by a developer.
Example:

    import org.junit.Assume;
    import org.junit.Assert;
    ...
    
    @Test 
    public void testForBug1234() {

        Assume.assumeTrue(isBugFixed(1234));//will not run this test until the bug 1234 is fixed

        Assert.assertEquals(5, calculator.add(2,3));
    }

The default runner treats tests with failing assumptions as ignored. It is possible that other runners may behave differently e.g. treat them as passed.

## JUnit – Basic annotation examples




Here’re some basic JUnit annotations you should understand:
===========================================================



    @BeforeClass – Run once before any of the test methods in the class, public static void 
    @AfterClass – Run once after all the tests in the class has been run, public static void
    @Before – Run before @Test, public void
    @After – Run after @Test, public void
    @Test – This is the test method to run, public void

## Installation or Setup
Since JUnit is a Java library, all you have to do to install it is to add a few JAR files into the classpath of your Java project and you're ready to go.

You can download these two JAR files manually: [junit.jar](http://bit.ly/My9IXz) & [hamcrest-core.jar](http://bit.ly/1gbl25b).

If you're using Maven, you can simply add in a dependency into your `pom.xml`:

    <dependency>
      <groupId>junit</groupId>
      <artifactId>junit</artifactId>
      <version>4.12</version>
      <scope>test</scope>
    </dependency>

Or if you're using Gradle,add in a dependency into your `build.gradle`:

    apply plugin: 'java'

    dependencies {
        testCompile 'junit:junit:4.12'
    }

After this you can create your first test class:

<!-- language: java -->

    import static org.junit.Assert.assertEquals;

    import org.junit.Test;
    
    public class MyTest {
        @Test
        public void onePlusOneShouldBeTwo() {
            int sum = 1 + 1;
            assertEquals(2, sum);
        }
    }

and run it from command line:
- Windows `java -cp .;junit-X.YY.jar;hamcrest-core-X.Y.jar org.junit.runner.JUnitCore MyTest`
- Linux or OsX `java -cp .:junit-X.YY.jar:hamcrest-core-X.Y.jar org.junit.runner.JUnitCore MyTest`

or with Maven: `mvn test`

## Basic unit test example
This example is a basic setup for unittesting the StringBuilder.toString() using junit.
 
<!-- language: java -->
    
    import static org.junit.Assert.assertEquals;

    import org.junit.Test;
    
    public class StringBuilderTest {
    
        @Test
        public void stringBuilderAppendShouldConcatinate()  {
            StringBuilder stringBuilder = new StringBuilder();
            stringBuilder.append("String");
            stringBuilder.append("Builder");
            stringBuilder.append("Test");
    
            assertEquals("StringBuilderTest", stringBuilder.toString());
        }
    
    }

## @Before, @After
An annotated method with `@Before` will be executed before every execution of `@Test` methods. Analogous an `@After` annotated method gets executed after every `@Test` method. This can be used to repeatedly set up a Test setting and clean up after every test. So the tests are independent and preparation code is not copied inside the `@Test` method.

Example:

<!-- language: java -->

    import static org.junit.Assert.assertEquals;
    
    import java.util.ArrayList;
    import java.util.List;
    
    import org.junit.After;
    import org.junit.Before;
    import org.junit.Test;
    
    public class DemoTest {
    
        private List<Integer> list;
    
        @Before
        public void setUp() {
            list = new ArrayList<>();
            list.add(3);
            list.add(1);
            list.add(4);
            list.add(1);
            list.add(5);
            list.add(9);
        }
    
        @After
        public void tearDown() {
            list.clear();
        }
    
        @Test
        public void shouldBeOkToAlterTestData() {
            list.remove(0); // Remove first element of list.
            assertEquals(5, list.size()); // Size is down to five
        }
    
        @Test
        public void shouldBeIndependentOfOtherTests() {
            assertEquals(6, list.size());
        }
    }

Methods annotated with `@Before` or `@After` must be `public void` and with zero arguments.

