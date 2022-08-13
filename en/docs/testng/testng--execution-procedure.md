---
title: "TestNG - Execution Procedure"
slug: "testng---execution-procedure"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Execution procedure of the TestNG test API methods
    public class TestngAnnotation {
       // test case 1
       @Test
       public void testCase1() {
          System.out.println("in test case 1");
       }
    
       // test case 2
       @Test
       public void testCase2() {
          System.out.println("in test case 2");
       }
    
       @BeforeMethod
       public void beforeMethod() {
          System.out.println("in beforeMethod");
       }
    
       @AfterMethod
       public void afterMethod() {
          System.out.println("in afterMethod");
       }
    
       @BeforeClass
       public void beforeClass() {
          System.out.println("in beforeClass");
       }
    
       @AfterClass
       public void afterClass() {
          System.out.println("in afterClass");
       }
    
       @BeforeTest
       public void beforeTest() {
          System.out.println("in beforeTest");
       }
    
       @AfterTest
       public void afterTest() {
          System.out.println("in afterTest");
       }
    
       @BeforeSuite
       public void beforeSuite() {
          System.out.println("in beforeSuite");
       }
    
       @AfterSuite
       public void afterSuite() {
          System.out.println("in afterSuite");
       }
    
    }


 let's create the file testng.xml in C:\>WORKSPACE to execute annotations.

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE suite SYSTEM "http://testng.org/testng-1.0.dtd" >

    <suite name="Suite1">
      <test name="test1">
        <classes>
           <class name="TestngAnnotation"/>
        </classes>
      </test>
    </suite>

C:\WORKSPACE>javac TestngAnnotation.java

Now run the testng.xml, which will run the test case defined in the provided Test Case class.

    in beforeSuite
    in beforeTest
    in beforeClass
    in beforeMethod
    in test case 1
    in afterMethod
    in beforeMethod
    in test case 2
    in afterMethod
    in afterClass
    in afterTest
    in afterSuite
    
    ===============================================
    Suite
    Total tests run: 2, Failures: 0, Skips: 0
    ===============================================



Execution procedure is as follows:

 1. First of all, **beforeSuite()** method is executed only once.
 2. Lastly, the **afterSuite()** method executes only once.
 3. Even the methods **beforeTest()**, **beforeClass()**, **afterClass()**, and **afterTest()** methods are executed only once.
 4. **beforeMethod()** method executes for each test case but before executing the test case.
 5. **afterMethod()** method executes for each test case but after executing the test case.
 6. In between **beforeMethod()** and **afterMethod()**, each test case executes.


