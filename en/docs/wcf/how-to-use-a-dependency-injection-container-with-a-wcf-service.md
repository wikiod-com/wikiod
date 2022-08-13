---
title: "How to use a dependency injection container with a WCF service"
slug: "how-to-use-a-dependency-injection-container-with-a-wcf-service"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## How to Configure a WCF Service to Use a Dependency Injection Container (Castle Windsor)
This example has two parts - some boilerplate steps for adding Castle Windsor to your WCF service, and then a simple, concrete example to show how we configure and use Windsor's container.

That makes the example a little bit long. If you already understand using a DI container then you likely only care about the boilerplate steps. If using a DI container is unfamiliar then it takes a little more - seeing the whole thing work from end to end - before it makes sense.

**Boilerplate Steps**

 1. Add the [Castle Windsor WCF integration facility](https://www.nuget.org/packages/Castle.WcfIntegrationFacility/) Nuget package to your WCF service application. This will add references to Castle Windsor as well as some components specifically for WCF services.

 2. Add a Global Application Class (global.asax) to your project: Add > New Item > Visual C# > Web > Global Application Class.  
The code that configures your container must be called from the `Application_Start` method. To keep it organized we can put all of our container configuration in a separate class. We don't have to. It doesn't matter. You'll see this done differently in different examples. What matters is that it's called from the `Application_Start` method because that's your *composition root* - where the application starts. The idea is to configure your container when the application starts and then never touch it directly again. It just stays in the background doing its job.

 3. Create a class to configure the container. This does two things:
    - `ContainerInstance.AddFacility<WcfFacility>()` tells Windsor's WCF code to use this particular container when creating instances of your WCF services.
    - `ContainerInstance.Install(FromAssembly.This())` tells Windsor to scan `This` assembly (in other words, your WCF project) looking for classes that implement `IWindsorInstaller`. Those classes will provide instructions to tell your container how to resolve dependencies. (We'll create one a few steps later.)


    public static class Container
    {
        private static readonly IWindsorContainer ContainerInstance = new WindsorContainer();

        public static void Configure()
        {
            ContainerInstance.AddFacility<WcfFacility>();
            ContainerInstance.Install(FromAssembly.This());
        }
    }

 4. Call this method from `Application_Start` in your global.asax:


    public class Global : System.Web.HttpApplication
    {
        protected void Application_Start(object sender, EventArgs e)
        {
            Container.Configure();
        }
    }
  
 5. Create an installer. This is just a class that implements `IWindsorInstaller`. This one is empty. It doesn't do anything. We'll add to this class in a few steps. When we call `ContainerInstance.Install(FromAssembly.This())`, `ContainerInstance` gets passed to the `Install` method so that we can register dependencies with the container.


    public class WindsorInstaller : IWindsorInstaller
    {
        public void Install(IWindsorContainer container, IConfigurationStore store)
        {
            // Nothing here yet!
        }
    }

 6. In the markup for any WCF service you create, add this directive. This indicates that the application will use Windsor's WCF facility to create instances of the service, which in turn means that in can inject dependencies when an instance is created.  


    Factory="Castle.Facilities.WcfIntegration.DefaultServiceHostFactory, Castle.Facilities.WcfIntegration"

**That ends the boilerplate steps for setting up the service to use a Castle Windsor dependency injection container.**

But the example isn't complete unless we configure at least one WCF service for dependency injection. The rest of this isn't boilerplate, just a simple, concrete example.

1. Create a new WCF service called `GreetingService.svc`.
2. Edit the markup. It should look like this:


    <%@ ServiceHost Language="C#" Debug="true" 
        Service="WcfWindsorDocumentation.GreetingService" 
        CodeBehind="GreetingService.svc.cs"     
        Factory="Castle.Facilities.WcfIntegration.DefaultServiceHostFactory, Castle.Facilities.WcfIntegration"
     %> 

3. Replace `IGreetingService` (the service contract) with this:


    [ServiceContract]
    public interface IGreetingService
    {
        [OperationContract]
        string GetGreeting();
    }

4. Replace `GreetingService` (in GreetingService.svc.cs) with this code. Notice that the constructor requires an instance of `IGreetingProvider` which we'll need our container to inject.


    public class GreetingService : IGreetingService
    {
        private readonly IGreetingProvider _greetingProvider;

        public GreetingService(IGreetingProvider greetingProvider)
        {
            _greetingProvider = greetingProvider;
        }

        public string GetGreeting()
        {
            return _greetingProvider.GetGreeting();
        }
    }

5. Add this implementation of `IGreetingProvider`. It also has a few dependencies of its own which we'll need the container to supply. The specifics of these classes aren't too important. They're just to create something easy to follow.


    public interface IGreetingProvider
    {
        string GetGreeting();
    }

    public interface IComputerNameProvider
    {
        string GetComputerName();
    }

    public class ComputerNameGreetingProvider : IGreetingProvider
    {
        private readonly IComputerNameProvider _computerNameProvider;

        public ComputerNameGreetingProvider(IComputerNameProvider computerNameProvider)
        {
            _computerNameProvider = computerNameProvider;
        }

        public string GetGreeting()
        {
            return string.Concat("Hello from ", _computerNameProvider.GetComputerName());
        }
    }

    public class EnvironmentComputerNameProvider : IComputerNameProvider
    {
        public string GetComputerName()
        {
            return System.Environment.MachineName;
        }
    }

6. Now we have all of the classes we need. All that's left is to register dependencies with our container. In other words, we're going to tell the container what classes it needs to create so it knows how to "build" an instance of `GreetingService`. This code goes in our implementation of `IWindsorInstaller` (step 5 of the boilerplate code.)


    public class WindsorInstaller : IWindsorInstaller
    {
        public void Install(IWindsorContainer container, IConfigurationStore store)
        {
            container.Register(
                Component.For<IGreetingService, GreetingService>(),
                Component.For<IGreetingProvider, ComputerNameGreetingProvider>(),
                Component.For<IComputerNameProvider, EnvironmentComputerNameProvider>());
        }
    }

This tells the container:
- It's responsible for creating `GreetingService` when needed.
- When it tries to create `GreetingService` it's going to need an `IGreetingProvider`. When it needs that it should create a `ComputerNameGreetingProvider`.
- When it tries to create `ComputerNameGreetingProvider`, that class requires an `IComputerNameProvider`. It should create an instance of `EnvironmentComputerNameProvider` to fulfill that need.

If, somewhere in the process of creating `GreetingService` or one of its dependencies, it comes across a requirement for a dependency that we haven't registered, it will let us know with a helpful exception, like this.

> Missing dependency.  
> Component WcfWindsorDocumentation.ComputerNameGreetingProvider has a dependency
> on WcfWindsorDocumentation.IComputerNameProvider, which could not be
> resolved.  
> Make sure the dependency is correctly registered in the container as a service, or provided as inline argument.

That means that something dependend on `IComputerNameProvider` but there was nothing registered to fulfill that dependency.

**This just gets the ball rolling.** There's much more to correctly configuring and using a dependency injection container for real-world scenarios. This example only covers what's specific to adding Windsor to a WCF service application. If you use a different container like Autofac or Unity you'll find that while the syntax and details vary, in principle they do the same things and you'll easily spot the similarities.

