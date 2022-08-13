---
title: "Apex Testing"
slug: "apex-testing"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Using testSetup
You can use a method annotated with `@testSetup` to write code that will have been executed before each test run:

```
public class AccountService {
  public static Account fetchAccount() {
    return [ SELECT Id, Name FROM Account LIMIT 1 ];
  }
}
```

```
@isTest
public class AccountServiceTest {
  private static final String TEST_ACCOUNT_NAME = 'My Test Account';

  @testSetup
  public static void setUpAccountData() {
    Account a = new Account(Name = TEST_ACCOUNT_NAME);
  }

  @isTest
  public static void testFetchAccount() {
    Account a = AccountService.fetchAccount();
    System.assertNotEquals(null, a, 'Account should not be null');
    System.assertEquals(TEST_ACCOUNT_NAME, a.Name, 'Account name should be correct');
  }
}

## Basic Test Class
This test class will test the `IsBlank(...)` method of `SomeClass`. Below is the example `SomeClass`. This class has only the one, basic `static` method, but you will be unable to deploy it to a production instance for use until you have reached the code coverage threshold. 

    public class SomeClass {

        public static Boolean IsBlank(String someData) {
            if (someData == null) {
                return true;
            } else if (someData == '') {
                return true; 
            } else {
                return false;
            }
        }

    }

As one can see, this method is simply a `if` statement with three branches. To write an effective test class, we must cover each branch with code, and use `System.assertEquals(...)` statements to verify that the proper data was received from `IsBlank(...)`.

    @isTest 
    public class SomeClass_test {

        @isTest 
        public static void SomeClass_IsBlank_test() {

            String testData;

            // SomeClass.IsBlank() returns true for Null values
            System.assertEquals(true, SomeClass.IsBlank(testData)); 

            testData = '';
            
            // SomeClass.IsBlank() returns true for empty strings
            System.assertEquals(true, SomeClass.IsBlank(testData)); 

            testData = 'someData';

            // SomeClass.IsBlank() returns false when testData is neither
            // an empty string nor Null
            System.assertEquals(false, SomeClass.IsBlank(testData)); 

        }

    }




## Using static blocks
While you can use the `@testSetup` annotation to designate a method to be run before tests are executed, this method will usually only be run once. If you need code to be run before each test, you can use a `static` block:

```
@isTest
public class MyTest {
  static {
    // code here will be run before each test is executed
  }
}

## Assert Methods


## Assertion Methods
`System.assert` can be used to check that a boolean expression evaluates to true:
```
System.assert(Service.isActive());
System.assert(!Service.getItems().isEmpty(), 'items should not be empty');
```

`System.assertEquals` and `System.assertNotEquals` can be used to check equality of two values. The expected value is passed as the first parameter, and the value under test is passed as the second.

```
System.assertEquals(4, Service.getItems().size());
System.assertNotEquals(null, Service.getItems());

// failure messages are optional:
System.assertEquals(true, Service.doWork(), 'doWork should return true');
System.assertNotEquals(null, Service.doWork(), 'doWork should not be null');
```

