---
title: ".NET - Pure DI examples"
slug: "net---pure-di-examples"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

An example of how to use dependency injection in .net without using a container. Based on examples by Mark Seemann http://blog.ploeh.dk/

## Web Api
    public interface ISingleton : IDisposable { }
    public class TransientDependency { }

    public class Singleton : ISingleton
    {
        public void Dispose() { } 
    }
    
    public class CompositionRoot : IDisposable, IHttpControllerActivator
    {
        private readonly ISingleton _singleton;

        // pass in any true singletons i.e. cross application instance singletons
        public CompositionRoot()
        {
            // intitialise any application instance singletons
            _singleton = new Singleton();
        }

        public void Dispose()
        {
            _singleton.Dispose();
        }
    
        public IHttpController Create(HttpRequestMessage request, HttpControllerDescriptor controllerDescriptor, Type controllerType)
        {
            // Per-Request-scoped services are declared and initialized here
            if (controllerType == typeof(SomeApiController))
            {
                // Transient services are created and directly injected here
                return new SomeApiController(_singleton, new TransientDependency());
            }
    
            var argumentException = new ArgumentException(@"Unexpected controller type! " + controllerType.Name,
                nameof(controllerType));
            Log.Error(argumentException, "don't know how to instantiate API controller: {controllerType}", controllerType.Name);
            throw argumentException;
        }
    }

    public static class DependencyInjection
    {
        public static void WireUp()
        {
            var compositionRoot = new CompositionRoot();
            System.Web.Http.GlobalConfiguration.Configuration.Services.Replace(typeof (IHttpControllerActivator), compositionRoot);
        }
    }

## MVC
    public interface ISingleton : IDisposable { }
    public class TransientDependency { }

    public class Singleton : ISingleton
    {
        public void Dispose() { } 
    }

    public class CompositionRoot : IDisposable, IControllerFactory
    {
        private readonly ISingleton _singleton;

        // pass in any true singletons i.e. cross application instance singletons
        public CompositionRoot()
        {
            // intitialise any application instance singletons
            _singleton = new Singleton();
        }

        public void Dispose()
        {
            _singleton.Dispose();
        }
   
        public IController CreateController(RequestContext requestContext, string controllerName)
        {
            if (controllerName.ToLower() == "home")
            {
                return new HomeController(_singleton, new TransientDependency());
            }
    
            var argumentException = new ArgumentException(@"Unexpected controller! " + controllerName);
            Log.Error("don't know how to instantiate MVC controller: {controllerType}. redirecting to help", controllerName);
            throw argumentException; // Alternatively would return some default Page Not Found placeholder controller;
        }

        public SessionStateBehavior GetControllerSessionBehavior(RequestContext requestContext, string controllerName)
        {
            return SessionStateBehavior.Default; 
        }
    
        public void ReleaseController(IController controller)
        {
            // anything to clean up?
        }
    }

    public static class DependencyInjection
    {
        public static void WireUp()
        {
            var compositionRoot = new CompositionRoot();
            System.Web.Mvc.ControllerBuilder.Current.SetControllerFactory(compositionRoot);
        }
    }

## ASP.NET Core
in Startup.cs

```
// This method gets called by the runtime. Use this method to add services to the container.
public void ConfigureServices(IServiceCollection services)
{
    // Add framework services.
    services.AddMvc();
    
    var controllerActivator = new CompositionRoot();
    services.AddSingleton<IControllerActivator>(controllerActivator);
}
```

CompositionRoot.cs

```
public class CompositionRoot : IControllerActivator, IDisposable
    {
        // Singletons
        private readonly ISingleton _singleton;

        public CompositionRoot()
        {
            // Create singletons
            _singleton = new Singleton();
        }

        public object Create(ControllerContext c) => this.Create(c.ActionDescriptor.ControllerTypeInfo.AsType());
        public void Release(ControllerContext c, object controller) => (controller as IDisposable)?.Dispose();

        public Controller Create(Type type)
        {
            // scoped 
            var scoped = new Scoped();
            // transient get new()-ed up below
            if (type == typeof(HomeController))
                return new HomeController(_singleton, scoped, new Transient());
            
            throw new InvalidOperationException($"Unknown controller {type}.");
        }

        public void Dispose()
        {
            // dispose stuff
        }
    }
```

