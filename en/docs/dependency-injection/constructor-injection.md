---
title: "Constructor Injection"
slug: "constructor-injection"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Constructor Injection
Classes have instance variable (dependencies), on which they call methods.

Example taken from http://www.jamesshore.com/Blog/Dependency-Injection-Demystified.html for reference

    public class Example { 
      private DatabaseThingie myDatabase; 
    
      public Example() { 
        myDatabase = new DatabaseThingie(); 
      } 
    
      public void DoStuff() { 
        ... 
        myDatabase.GetData(); 
        ... 
      } 
    }

This class has one dependency called `DatabaseThingie`. Here in this example class **Example** is responsible for creating its own dependencies, thereby violating **Single Responsibility Principle**. Class has some primary responsibility + it is creating its won dependencies. If dependency creation mechanism changes, Let say now the dependency take argument or Instead of DatabaseThingie I want some other type. The class will change.

**Injecting dependency from External Source**

    public class Example { 
      private DatabaseThingie myDatabase; 

      public Example(DatabaseThingie useThisDatabaseInstead) { 
        myDatabase = useThisDatabaseInstead; 
      }
    
      public void DoStuff() { 
        ... 
        myDatabase.GetData(); 
        ... 
      } 
    }

Here we are Injecting dependency from external source using **Constructor Injection** (Passing dependency in constructor)

We don't care what type of dependency is injected, We just use it. The class won't change due to this.

The main benefit of Dependency Injection is when writing **tests**.

    public class ExampleTest { 

      void testDoStuff() { 
        MockDatabase mockDatabase = new MockDatabase(); 
    
        // MockDatabase is a subclass of DatabaseThingie, so we can 
        // "inject" it here: 
        Example example = new Example(mockDatabase); 
    
        example.DoStuff(); 
        mockDatabase.AssertGetDataWasCalled(); 
      } 
    }

