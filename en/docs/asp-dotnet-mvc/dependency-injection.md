---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

The whole point of Dependency Injection ( DI ) is to reduce code coupling. 
Imagine any kind if interaction which involves newing up something like in the "Hard coded dependency example". 

A big part of writing code is the ability to test it. Every time we new up a new dependency, we make our code difficult to test because we have no control over that dependency. 

How would you test code which depends on DataTime.Now for example? It always changes so you have no reference. This is when you inject a stable parameter as your starting point. You can control it, you can write tests based on various values and make sure you always get the right result.

A good option therefore is to pass an interface or an abstract class as a parameter in the constructor DI. 

An interface represents a well defined contract, you can always rely on the methods to be there and you can always rely on the method signatures. 

Once you start using DI other aspects will open up. For example, even if you pass an interface at some point you will need a real implementation to actually do any work. This is where other concepts appear. We can use IOC ( Inversion of Control ) to resolve our dependencies. This means that we instruct our code to always use a specific implementation for any contract. Of course there are other ways of doing this. We could always instantiate each contract with a specific implementation and from that point onwards our code can use that part :

    public ILogging Logging { get; set }

at some point we initialise it.

    Logging = new FileLogging();

this will always work as long as our class fulfils the expected contract :

    public class FileLogging : ILogging

from the initialise moment onwards we always use the Logging object. This makes lif easier because if we ever decide to change and use a DatabaseLogging for example, we only have to change the code in one place and this is exactly where we initialise the Logging class.

Is DI only good for testing? No, DI is important when writing maintainable code as well. It allows the separation of concerns to be clear. 

When you write any code, think ... is it testable, can I write a test, that's when injecting a DateTime value instead of using DateTime.Now makes sense. 

## Ninject Configurations


## Utilization of the interfaces


## Constructor dependency injection
The Constructor Dependency Injection requires parameters in the constructor to inject dependencies. So you have to pass the values when you create a new object.

    public class Example
    {
        private readonly ILogging _logging;
    
        public Example(ILogging logging)
        {
            this._logging = logging;
        }
    }

## Hard coded dependency
    public class Example
    {
        private FileLogging _logging;
    
        public Example()
        {
            this._logging = new FileLogging();
        }
    }

## parameter DI
    public DateTime SomeCalculation()
    {
         return DateTime.Now.AddDays(3);
    }

vs

    public DateTime SomeCalculation(DateTime inputDate)
    {
        return inputDate.AddDays(3);
    }



## Ninject Dependency Injection


