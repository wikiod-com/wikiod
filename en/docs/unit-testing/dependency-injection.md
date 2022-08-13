---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

One approach that can be taken to writing software is to create dependencies as they are needed.  This is quite an intuitive way to write a program and is the way that most people will tend to be taught, partly because it is easy to follow.  One of the issues with this approach is that it can be hard to test.  Consider a method that does some processing based on the current date.  The method might contain some code like the following:

    if (DateTime.Now.Date > processDate)
    {
        // Do some processing
    }

The code has a direct dependency on the current date.  This method can be hard to test because the current date cannot be easily manipulated.  One approach to making the code more testable is to remove the direct reference to the current date and instead supply (or inject) the current date to the method that does the processing.  This dependency injection can make it much easier to test aspects of code by using [test doubles][1] to simplify the setup step of the unit test.

**IOC systems**

Another aspect to consider is the lifetime of dependencies; in the case where the class itself creates its own dependencies (also known as invariants), it is then responsible for disposing of them. Dependency Injection inverts this (and this is why we often refer to an injection library as an "Inversion of Control" system) and means that instead of the class being responsible for creating, managing and cleaning up its dependencies, an external agent (in this case, the IoC system) does it instead.

This makes it much simpler to have dependencies that are shared amongst instances of the same class; for example consider a service that fetches data from an HTTP endpoint for a class to consume. Since this service is stateless (i.e. it doesn't have any internal state) therefore we really only need a single instance of this service throughout our application. Whilst it is possible (for example, by using a static class) to do this manually, it's much simpler to create the class and tell the IoC system that it's to be created as a *Singleton*, whereby only one instance of the class every exists.

Another example would be database contexts used in a web application, whereby a new Context is required per request (or thread) and not per instance of a controller; this allows the context to be injected in every layer executed by that thread, without having to be manually passed around.

This frees the consuming classes from having to manage the dependencies.


  [1]: https://www.wikiod.com/unit-testing/test-doubles

## Constructor Injection
Constructor injection is the safest way of injecting dependencies that a whole class depends upon.  Such dependencies are often referred to as *invariants*, since an instance of the class cannot be created without supplying them.
By requiring the dependency to be injected at construction, it is guaranteed that an object cannot be created in an inconsistent state.

Consider a class that needs to write to a log file in error conditions.  It has a dependency on a `ILogger`, which can be injected and used when necessary.

    public class RecordProcessor
    {
        readonly private ILogger _logger;

        public RecordProcessor(ILogger logger)
        {
            _logger = logger;
        }

        public void DoSomeProcessing() {
            // ...
            _logger.Log("Complete");
        }
    }

Sometimes while writing tests you may note that constructor requires more dependencies than it is actually needed for a case being tested. The more such tests you have the more likely it is that your class breaks *Single Responsibility Principle* (SRP). That is why it it is not a very good practice to define the default behavior for all mocks of injected dependencies at test class initialization phase as it can mask the potential warning signal.

The unittest for this would look like the following:

    [Test]
    public void RecordProcessor_DependencyInjectionExample()
    {
        ILogger logger = new FakeLoggerImpl(); //or create a mock by a mocking Framework
        
        var sut = new RecordProcessor(logger); //initialize with fake impl in testcode

        Assert.IsTrue(logger.HasCalledExpectedMethod());
    }




## Property Injection
Property injection allows a classes dependencies to be updated after it has been created.  This can be useful if you want to simplify object creation, but still allow the dependencies to be overridden by your tests with test doubles.  

Consider a class that needs to write to a log file in an error condition.  The class knows how to construct a default `Logger`, but allows it to be overridden through property injection. However it worths noting that using property injection this way you are tightly coupling this class with an exact implementation of `ILogger` that is `ConcreteLogger` in this given example. A possible workaround could be a factory that returns the needed ILogger implementation.

    public class RecordProcessor
    {
        public RecordProcessor()
        {
            Logger = new ConcreteLogger();
        }

        public ILogger Logger { get; set; }

        public void DoSomeProcessing()
        {
            // ...
            _logger.Log("Complete");
        }
    }

In most cases, Constructor Injection is preferable to Property Injection because it provides better guarantees about the state of the object immediately after its construction.

## Method Injection
Method injection is a fine grained way of injecting dependencies into processing.  Consider a method that does some processing based on the current date.  The current date is hard to change from a test, so it is much easier to pass a date into the method that you want to test.

    public void ProcessRecords(DateTime currentDate)
    {
        foreach(var record in _records) 
        {
            if (currentDate.Date > record.ProcessDate)
            {
                // Do some processing
            }
        }
    }


## Containers / DI Frameworks
Whilst extracting dependencies out of your code so that they can be injected makes your code easier to test, it pushes the problem further up the hierarchy and can also result in objects that are difficult to construct.  Various dependency injection frameworks / Inversion of Control Containers have been written to help overcome this issue.  These allow type mappings to be registered.  These registrations are then used to resolve dependencies when the container is asked to construct an object.

Consider these classes:

    public interface ILogger {
        void Log(string message);
    }

    public class ConcreteLogger : ILogger
    {
        public ConcreteLogger()
        {
            // ...
        }
        public void Log(string message)
        {
            // ...
        }
    }
    public class SimpleClass
    {
        public SimpleClass()
        {
            // ...
        }
    }

    public class SomeProcessor
    {
        public SomeProcessor(ILogger logger, SimpleClass simpleClass)
        {
            // ...
        }
    }

In order to construct `SomeProcessor`, both an instance of `ILogger` and `SimpleClass` are required.  A container like Unity helps to automate this process.

First the container needs to be constructed and then mappings are registered with it.  This is usually done only once within an application. The area of the system where this occurs is commonly known as the *Composition Root*

    // Register the container
    var container = new UnityContainer();

    // Register a type mapping.  This allows a `SimpleClass` instance
    // to be constructed whenever it is required.
    container.RegisterType<SimpleClass, SimpleClass>();

    // Register an instance.  This will use this instance of `ConcreteLogger` 
    // Whenever an `ILogger` is required.
    container.RegisterInstance<ILogger>(new ConcreteLogger());

After the container is configured, it can be used to create objects, automatically resolving dependencies as required:

    var processor = container.Resolve<SomeProcessor>();


