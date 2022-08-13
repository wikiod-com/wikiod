---
title: "@Test Annotation"
slug: "test-annotation"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
 - @Test
 - @Test(attribute1 = attributeValue, atrribute2 = attributeValue, etc)

## Parameters
|Parameter|Details|
|----|----|
|alwaysRun|If set to true, this test method will always be run even if it depends on a method that failed.|
|dataProvider|The name of the data provider for this test method.|
|dataProviderClass|The class where to look for the data provider. If not specified, the data provider will be looked on the class of the current test method or one of its base classes. If this attribute is specified, the data provider method needs to be static on the specified class.|
|dependsOnGroups|The list of groups this method depends on.|
|dependsOnMethods|The list of methods this method depends on.|
|description|The description for this method.|
|enabled|Whether methods on this class/method are enabled.|
|expectedExceptions|The list of exceptions that a test method is expected to throw. If no exception or a different than one on this list is thrown, this test will be marked a failure.|
|groups|The list of groups this class/method belongs to.|
|invocationCount|The number of times this method should be invoked.|
|invocationTimeOut|The maximum number of milliseconds this test should take for the cumulated time of all the invocationcounts. This attribute will be ignored if invocationCount is not specified.|
|priority|The priority for this test method. Lower priorities will be scheduled first.|
|successPercentage|The percentage of success expected from this method|
|singleThreaded|If set to true, all the methods on this test class are guaranteed to run in the same thread, even if the tests are currently being run with `parallel="methods"`. This attribute can only be used at the class level and it will be ignored if used at the method level. **Note**: this attribute used to be called sequential (now deprecated).|
|timeOut|The maximum number of milliseconds this test should take.|
|threadPoolSize|The size of the thread pool for this method. The method will be invoked from multiple threads as specified by invocationCount. **Note**: this attribute is ignored if invocationCount is not specified|

## Quick example on @Test annotation
`@Test` annotation can be applied to any **class** or **method**. This annotation marks a class or a method as part of the test.

 1. `@Test` at method level - mark annotated method as test method
 2. `@Test` at class level
    - The effect of a class level `@Test` annotation is to make all the public methods of the class to become test methods even if they are not annotated. 
    - `@Test` annotation can also be repeated on a method if you want to add certain attributes.


**Example of `@Test` at method level**:

   
    import org.testng.annotations.Test;

    public class TestClass1 {
        public void notTestMethod() {
        }

        @Test
        public void testMethod() {
        }
    }

**Example of `@Test` at class level**:

    import org.testng.annotations.Test;

    @Test
    public class TestClass2 {
        public void testMethod1() {
        }

        @Test
        public void testMethod2() {
        }
    }

 

