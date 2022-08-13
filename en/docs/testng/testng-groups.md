---
title: "TestNG Groups"
slug: "testng-groups"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

## Syntax

  

 - @Test(groups = {"group1", "group.regression" }, dependsOnGroups = {"group2", "group3"})

## TestNG Groups configuration and basic example
Groups can be configured under `Suite` and/or `Test` element of `testng.xml`. All groups which are marked as included in `tesng.xml` will be considered for execution, excluded one will be ignored. If a `@Test` method has multiple groups and from those groups if any single groups is excluded in `testng.xml` that `@Test` method will not run. 

Below is the typical `testng.xml` configuration at `Test` level for running groups:

    <suite name="Suite World">
    <test name="Test Name">
      <groups>
        <run>
          <include name="functest" />
          <exclude name="regtest" />
        </run>
      </groups>
      <classes>
        <class name="example.group.GroupTest"/>
      </classes>
    </test>
    </suite>

And this how it's test class will look like:

    package example.group;
    
    import org.testng.annotations.AfterClass;
    import org.testng.annotations.BeforeClass;
    import org.testng.annotations.Test;
    
    public class GroupTest {
        
        @BeforeClass
        public void deSetup(){
            //do configuration stuff here
        }
        
        @Test(groups = { "functest", "regtest" })
        public void testMethod1() {
        }
    
        @Test(groups = {"functest", "regtest"} )
        public void testMethod2() {
        }
    
        @Test(groups = { "functest" })
        public void testMethod3() {
        }
        
        @AfterClass
        public void cleanUp(){
            //do resource release and cleanup stuff here
        }
    }

> On running this `GroupTest` `TestNG` class only `testMethod3()` will
> be executed.

Explanation:

 - `<include name="functest" />` all the test methods of `functest` group are eligible for run if it not excluded by any other group.
 - `<exclude name="regtest" />` no test methods of `regtest` group are eligible for run.
 - `testMethod1()` and `testMethod2()` are in `regtest` group, so they will not have run.
 - `testMethod3()` is in `regtest` group, so it will run.

 



  


## TestNG MetaGroups - Groups of groups
TestNG allows defining groups which can include other groups. MetaGroups logically combine one or more group(s) and control the execution of the `@Test`methods belonging to those groups.

In below example there are various `@Test` methods belonging to different group(s). Few are specific to particular stack and few are regression and acceptance tests. Here MetaGroups can be created. Let's pick any two simple **MetaGroups**:
 1. `allstack` - includes both `liux.jboss.oracle` and `aix.was.db2` groups and enables all test methods belonging to any one of those groups to run together.
 2. `systemtest` - includes `allstack`, `regression` and `acceptance` groups and enables all test methods belonging to any one of those groups to run together.

**testng.xml** configuration

    <suite name="Groups of Groups">
        <test name="MetaGroups Test">
            <groups>
                <!-- allstack group includes both liux.jboss.oracle and aix.was.db2 groups -->
                <define name="allstack">
                    <include name="liux.jboss.oracle" />
                    <include name="aix.was.db2" />
                </define>
    
                <!-- systemtest group includes all groups allstack, regression and acceptance -->
                <define name="systemtest">
                    <include name="allstack" />
                    <include name="regression" />
                    <include name="acceptance" />
                </define>
    
                <run>
                    <include name="systemtest" />
                </run>
            </groups>
    
            <classes>
                <class name="example.group.MetaGroupsTest" />
            </classes>
        </test>
    
    </suite>

**MetaGroupsTest** class

    package example.group;
    
    import org.testng.annotations.AfterMethod;
    import org.testng.annotations.BeforeMethod;
    import org.testng.annotations.Test;
    
    public class MetaGroupsTest {
    
        @BeforeMethod
        public void beforeMethod(){
            //before method stuffs - setup
        }
    
        @Test(groups = { "liux.jboss.oracle", "acceptance" })
        public void testOnLinuxJbossOracleStack() {
            //your test logic goes here
        }
    
        @Test(groups = {"aix.was.db2", "regression"} )
        public void testOnAixWasDb2Stack() {
            //your test logic goes here
        }
    
        @Test(groups = "acceptance")
        public void testAcceptance() {
            //your test logic goes here
        }
    
        @Test(groups = "regression")
        public void testRegression(){
            //your test logic goes here
        }
    
        @AfterMethod
        public void afterMthod(){
            //after method stuffs - cleanup
        }
    }

