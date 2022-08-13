---
title: "Unity WebAPI"
slug: "unity-webapi"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## Setting up Unity with Web API.
**1. Add Unity to your project.**

If you use [NuGet][1] you can use the [Unity-package][2]. Run `Install-Package Unity` in Package Manager Console. This will add the Unity library (and it's dependencies) to your project.


**2. Create an implementation of [`IDependencyResolver`][3].**

For example:

    public class UnityResolver : IDependencyResolver
    {
        protected IUnityContainer Container;
    
        public UnityResolver(IUnityContainer container)
        {
            if (container == null)
            {
                throw new ArgumentNullException("container");
            }
            this.Container = container;
        }
    
        public object GetService(Type serviceType)
        {
            try
            {
                return Container.Resolve(serviceType);
            }
            catch (ResolutionFailedException)
            {
                return null;
            }
        }
    
        public IEnumerable<object> GetServices(Type serviceType)
        {
            try
            {
                return Container.ResolveAll(serviceType);
            }
            catch (ResolutionFailedException)
            {
                return new List<object>();
            }
        }
    
        public IDependencyScope BeginScope()
        {
            var child = Container.CreateChildContainer();
            return new UnityResolver(child);
        }
    
        public void Dispose()
        {
            Container.Dispose();
        }
    }

**3. Register your `IDependencyResolver` in your `WebApiConfig`.**

    public static class WebApiConfig
    {
        public static void Register(HttpConfiguration config)
        {
            // Routes goes here..

            // Create your container.
            var container = new UnityContainer();

            // Do registrations here...

            // Assign your container.
            config.DependencyResolver = new UnityResolver(container);
        }
    }


  [1]: https://www.nuget.org
  [2]: https://www.nuget.org/packages/Unity/
  [3]: https://msdn.microsoft.com/en-us/library/system.web.http.dependencies.idependencyresolver(v=vs.118).aspx

