---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9451
type: docs
toc: true
---

Aspnet core is built with Dependency Injection as one of its key core concepts. It introduces one conforming container abstraction so you can replace the builtin one with a third-party container of your choice.

## Syntax
- `IServiceCollection.Add(ServiceDescriptor item);`
- `IServiceCollection.AddScoped(Type serviceType);`
- `IServiceCollection.AddScoped(Type serviceType, Type implementationType);`
- `IServiceCollection.AddScoped(Type serviceType, Func<IServiceProvider, object> implementationFactory);`
- `IServiceCollection.AddScoped<TService>()`
- `IServiceCollection.AddScoped<TService>(Func<IServiceProvider, TService> implementationFactory)`
- `IServiceCollection.AddScoped<TService, TImplementation>()`
- `IServiceCollection.AddScoped<TService, TImplementation>(Func<IServiceProvider, TImplementation> implementationFactory)`
- `IServiceCollection.AddSingleton(Type serviceType);`
- `IServiceCollection.AddSingleton(Type serviceType, Func<IServiceProvider, object> implementationFactory);`
- `IServiceCollection.AddSingleton(Type serviceType, Type implementationType);`
- `IServiceCollection.AddSingleton(Type serviceType, object implementationInstance);`
- `IServiceCollection.AddSingleton<TService>()`
- `IServiceCollection.AddSingleton<TService>(Func<IServiceProvider, TService> implementationFactory)`
- `IServiceCollection.AddSingleton<TService>(TService implementationInstance)`
- `IServiceCollection.AddSingleton<TService, TImplementation>()`
- `IServiceCollection.AddSingleton<TService, TImplementation>(Func<IServiceProvider, TImplementation> implementationFactory)`
- `IServiceCollection.AddTransient(Type serviceType);`
- `IServiceCollection.AddTransient(Type serviceType, Func<IServiceProvider, object> implementationFactory);`
- `IServiceCollection.AddTransient(Type serviceType, Type implementationType);`
- `IServiceCollection.AddTransient<TService>()`
- `IServiceCollection.AddTransient<TService>(Func<IServiceProvider, TService> implementationFactory)`
- `IServiceCollection.AddTransient<TService, TImplementation>()`
- `IServiceCollection.AddTransient<TService, TImplementation>(Func<IServiceProvider, TImplementation> implementationFactory)`
- `IServiceProvider.GetService(Type serviceType)`
- `IServiceProvider.GetService<T>()`
- `IServiceProvider.GetServices(Type serviceType)`
- `IServiceProvider.GetServices<T>()`

<!-- language-all: c# -->
To use generic variants of `IServiceProvider` methods you have to include the following namespace:

    using Microsoft.Extensions.DependencyInjection;

## Register dependencies
<!-- language-all: c# -->

Builtin container comes with a set of builtin features :    

# Lifetime control

    public void ConfigureServices(IServiceCollection services)
        {
            // ...
        
            services.AddTransient<ITestService, TestService>();
            // or
            services.AddScoped<ITestService, TestService>();
            // or
            services.AddSingleton<ITestService, TestService>();
            // or
            services.AddSingleton<ITestService>(new TestService());
        }

- **AddTransient**: Created everytime it is resolved
- **AddScoped**: Created once per request
- **AddSingleton**: Lazily created once per application
- **AddSingleton (instance)**: Provides a previously created instance per application

# Enumerable dependencies

It is also possible to register enumerable dependencies :

     services.TryAddEnumerable(ServiceDescriptor.Transient<ITestService, TestServiceImpl1>());
     services.TryAddEnumerable(ServiceDescriptor.Transient<ITestService, TestServiceImpl2>());

You can then consume them as follows :

    public class HomeController : Controller
    {
        public HomeController(IEnumerable<ITestService> services)
        {
            // do something with services.
        }
    }
# Generic dependencies

You can also register generic dependencies :
```
services.Add(ServiceDescriptor.Singleton(typeof(IKeyValueStore<>), typeof(KeyValueStore<>)));
```

And then consume it as follows :
```
public class HomeController : Controller
{
    public HomeController(IKeyValueStore<UserSettings> userSettings)
    {
        // do something with services.
    }
}
```

## Register and manually resolve
<!-- language-all: c# -->

The preferred way of describing dependencies is by using constructor injection which follows [Explicit Dependencies Principle](http://deviq.com/explicit-dependencies-principle/):

ITestService.cs

    public interface ITestService
    {
        int GenerateRandom();
    }

TestService.cs

    public class TestService : ITestService
    {
        public int GenerateRandom()
        {
            return 4;
        }
    }

Startup.cs (ConfigureServices)

    public void ConfigureServices(IServiceCollection services)
    {
        // ...

        services.AddTransient<ITestService, TestService>();
    }

HomeController.cs


    using Microsoft.Extensions.DependencyInjection;
    
    namespace Core.Controllers
    {
        public class HomeController : Controller
        {
            public HomeController(ITestService service)
            {
                int rnd = service.GenerateRandom();
            }
        }
    }

## Injecting a dependency into a Controller Action
<!-- language-all: c# -->
A less known builtin feature is Controller Action injection using the `FromServicesAttribute`. 

    [HttpGet]
    public async Task<IActionResult> GetAllAsync([FromServices]IProductService products)
    {
         return Ok(await products.GetAllAsync());
    }

An important note is that the `[FromServices]` **can not** be used as general "Property Injection" or "Method injection" mechanism! It can only be used on method parameters of an controller action or controller constructor (in the constructor it's obsolete though, as ASP.NET Core DI system already uses constructor injection and there are no extra markers required). 

**It can not be used anywhere outside of a controllers, controller action**. Also it is very specific to ASP.NET Core MVC and resides in the `Microsoft.AspNetCore.Mvc.Core` assembly. 

Original quote from the ASP.NET Core MVC GitHub issue ([Limit [FromServices] to apply only to parameters][1]) regarding this attribute:
 
> @rynowak:
> > @Eilon: 
> > 
> > The problem with properties is that it appears to many people that it can be applied to any property of any object. 
> 
> Agreed, we've had a number of issues posted by users with confusion around how this feature should be used. There's really been a fairly large amount of feedback both of the kinds " [FromServices]  is weird and I don't like it" and " [FromServices]  has confounded me". It feels like a trap, and something that the team would still be answering questions about years from now.
>
> We feel like most valuable scenario for  [FromServices]  is on method parameter to an action for a service that you only need in that one place.
> 
> /cc @danroth27 - docs changes
> 
> To anyone in love with the current  [FromServices] , I'd strongly recommend looking into a DI system that can do property injection (Autofac, for example).

Notes:
* **Any** services registered with the .NET Core Dependency Injection system can be injected inside an controller's action using the `[FromServices]` attribute.
* Most relevant case is when you need a service only in a single action method and don't want to clutter your controller's constructor with another dependency, which will only be used once.
* Can't be used outside of ASP.NET Core MVC (i.e. pure .NET Framework or .NET Core console applications), because it resides in `Microsoft.AspNetCore.Mvc.Core` assembly.
* For property or method injection you must use one of third-party IoC containers available (Autofac, Unity, etc.).

  [1]: https://github.com/aspnet/Mvc/issues/3507#issuecomment-155484837

## Using scoped services during application startup / Database Seeding
Resolving scoped services during application startup can be difficult, because there is no request and hence no scoped service. 

<!-- language-all: c# -->

Resolving a scoped service during application startup via `app.ApplicationServices.GetService<AppDbContext>()` can cause issues, because it will be created in the scope of the global container, effectively making it a singleton with the lifetime of the application, which may lead to exceptions like `Cannot access a disposed object in ASP.NET Core when injecting DbContext`.

The following pattern solves the issue by first creating a new scope and then resolving the scoped services from it, then once the work is done, disposing the scoped container. 

    public Configure(IApplicationBuilder app)
    {
        // serviceProvider is app.ApplicationServices from Configure(IApplicationBuilder app) method
        using (var serviceScope = app.ApplicationServices.GetRequiredService<IServiceScopeFactory>().CreateScope())
        {
            var db = serviceScope.ServiceProvider.GetService<AppDbContext>();

            if (await db.Database.EnsureCreatedAsync())
            {
                await SeedDatabase(db);
            }
        }
    }

This is a semi-official way of the Entity Framework core team to seed data during application startup and is reflected in the [MusicStore sample][1] application. 


  [1]: https://github.com/aspnet/MusicStore/blob/1.0.0/src/MusicStore/Models/SampleData.cs#L22-L34

## Retrieve dependencies on a Controller
<!-- language-all: c# -->
Once registered a dependency can be retrieved by adding parameters on the Controller constructor.

    // ...
    using System;
    using Microsoft.Extensions.DependencyInjection;
    
    namespace Core.Controllers
    {
        public class HomeController : Controller
        {
            public HomeController(ITestService service)
            {
                int rnd = service.GenerateRandom();
            }
        }
    }

## The Options pattern / Injecting options into services
With ASP.NET Core the Microsoft team also introduced the Options pattern, which allows to have strong typed options and once configured the ability to inject the options into your services. 

<!-- language-all: c# -->

First we start with a strong typed class, which will hold our configuration. 

    public class MySettings 
    {
        public string Value1 { get; set; }
        public string Value2 { get; set; }
    }

And an entry in the `appsettings.json`.

    {
      "mysettings" : {
        "value1": "Hello",
        "value2": "World"
      }
    }

Next we initialize it in the Startup class. There are two ways to do this

1. Load it directly from the `appsettings.json` "mysettings" section

       services.Configure<MySettings>(Configuration.GetSection("mysettings"));

2. Do it manually

       services.Configure<MySettings>(new MySettings 
       {
           Value1 = "Hello",
           Value2 = Configuration["mysettings:value2"]
       });

   Each hierarchy level of the `appsettings.json` is separated by a `:`. Since `value2` is a property of the `mysettings` object, we access it via `mysettings:value2`.

Finally we can inject the options into our services, using the `IOptions<T>` interface

    public class MyService : IMyService
    {
        private readonly MySettings settings;

        public MyService(IOptions<MySettings> mysettings) 
        {
            this.settings = mySettings.Value;
        }
    }

# Remarks
If the `IOptions<T>` isn't configured during the startup, injecting `IOptions<T>` will inject the default instance of `T` class. 


## Resolve Controllers, ViewComponents and TagHelpers via Dependency Injection
By default Controllers, ViewComponents and TagHelpers aren't registered and resolved via the dependency injection container. This results in the inability to do i.e. property injection when using a 3rd party Inversion of Control (IoC) container like AutoFac. 

In order to make ASP.NET Core MVC resolve these Types via IoC too, one needs to add the following registrations in the `Startup.cs` (taken from the official [ControllersFromService sample][1] on GitHub)

<!-- language: c# -->

    public void ConfigureServices(IServiceCollection services)
    {
        var builder = services
            .AddMvc()
            .ConfigureApplicationPartManager(manager => manager.ApplicationParts.Clear())
            .AddApplicationPart(typeof(TimeScheduleController).GetTypeInfo().Assembly)
            .ConfigureApplicationPartManager(manager =>
            {
                manager.ApplicationParts.Add(new TypesPart(
                  typeof(AnotherController),
                  typeof(ComponentFromServicesViewComponent),
                  typeof(InServicesTagHelper)));

                manager.FeatureProviders.Add(new AssemblyMetadataReferenceFeatureProvider());
            })
            .AddControllersAsServices()
            .AddViewComponentsAsServices()
            .AddTagHelpersAsServices();

        services.AddTransient<QueryValueService>();
        services.AddTransient<ValueService>();
        services.AddSingleton<IHttpContextAccessor, HttpContextAccessor>();
    }


  [1]: https://github.com/aspnet/Mvc/blob/rel/1.0.1/test/WebSites/ControllersFromServicesWebSite/Startup.cs#L37-L39

## Plain Dependency Injection example (Without Startup.cs)
This shows you how to use [Microsoft.Extensions.DependencyInjection](https://www.nuget.org/packages/Microsoft.Extensions.DependencyInjection/) nuget package without the use of the `WebHostBuilder` from kestrel (e.g. when you want to build something else then a webApp):

    internal class Program
    {
        public static void Main(string[] args)
        {
            var services = new ServiceCollection(); //Creates the service registry
            services.AddTransient<IMyInterface, MyClass>(); //Add registration of IMyInterface (should create an new instance of MyClass every time)
            var serviceProvider = services.BuildServiceProvider(); //Build dependencies into an IOC container
            var implementation = serviceProvider.GetService<IMyInterface>(); //Gets a dependency

            //serviceProvider.GetService<ServiceDependingOnIMyInterface>(); //Would throw an error since ServiceDependingOnIMyInterface is not registered
            var manualyInstaniate = new ServiceDependingOnIMyInterface(implementation); 

            services.AddTransient<ServiceDependingOnIMyInterface>();
            var spWithService = services.BuildServiceProvider(); //Generaly its bad practise to rebuild the container because its heavey and promotes use of anti-pattern.
            spWithService.GetService<ServiceDependingOnIMyInterface>(); //only now i can resolve
        }
    }

    interface IMyInterface
    {
    }

    class MyClass : IMyInterface
    {
    }

    class ServiceDependingOnIMyInterface
    {
        private readonly IMyInterface _dependency;

        public ServiceDependingOnIMyInterface(IMyInterface dependency)
        {
            _dependency = dependency;
        }
    }


## Inner workings of Microsoft.Extensions.DependencyInjection
# IServiceCollection

To start building an IOC container with Microsoft's DI nuget package you start with creating an `IServiceCollection`. You can use the already provided Collection: `ServiceCollection`:

    var services = new ServiceCollection();

This `IServiceCollection` is nothing more than an implementation of: `IList<ServiceDescriptor>, ICollection<ServiceDescriptor>, IEnumerable<ServiceDescriptor>, IEnumerable`

All the following methods are only extension methods to add `ServiceDescriptor` instances to the list:

    services.AddTransient<Class>(); //add registration that is always recreated
    services.AddSingleton<Class>(); // add registration that is only created once and then re-used
    services.AddTransient<Abstract, Implementation>(); //specify implementation for interface
    services.AddTransient<Interface>(serviceProvider=> new Class(serviceProvider.GetService<IDependency>())); //specify your own resolve function/ factory method.
    services.AddMvc(); //extension method by the MVC nuget package, to add a whole bunch of registrations.
    // etc..

    //when not using an extension method:
    services.Add(new ServiceDescriptor(typeof(Interface), typeof(Class)));

# IServiceProvider

The serviceprovider is the one 'Compiling' all the registrations so that they can be used quickly, this can be done with `services.BuildServiceProvider()` which is basically an extension mehtod for:

    var provider = new ServiceProvider( services, false); //false is if it should validate scopes


Behind the scenes every `ServiceDescriptor` in the `IServiceCollection` gets compiled to a factory method `Func<ServiceProvider, object>` where object is the return type and is: the created instance of the Implementation type, the Singleton or your own defined factory method. 

These registrations get added to the `ServiceTable` which is basically a `ConcurrentDictionary` with the key being the `ServiceType` and the value the Factory method defined above.

## Result

Now we have a `ConcurrentDictionary<Type, Func<ServiceProvider, object>>` which we can use concurrently to ask to create Services for us. To show a basic example of how this could have looked. 

      var serviceProvider = new ConcurrentDictionary<Type, Func<ServiceProvider, object>>();
      var factoryMethod = serviceProvider[typeof(MyService)];
      var myServiceInstance = factoryMethod(serviceProvider)
      
This is not how it works! 

This `ConcurrentDictionary` is a property of the `ServiceTable` which is a property of the `ServiceProvider`

