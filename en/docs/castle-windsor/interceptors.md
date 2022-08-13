---
title: "Interceptors"
slug: "interceptors"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Creating custom interceptors
Interceptors require the `IInterceptor` interface. Any public method within the intercepted class will be intercepted _(including getters and setters)_

    public class MyInterceptor : IInterceptor
    {
        public void Intercept(IInvocation invocation)
        {
           //Calls the next method in the chain - The last one will be the 
           //original method that was intercepted
           invocation.Proceed();
        }
    }

Read this for [information about registering an interceptor][1] to components


  [1]: https://www.wikiod.com/castle-windsor/installers#Interceptors

## Interceptors doesn't have to call Proceed every time
Interceptors are a good tool for implementing [cross-cutting concerns](https://en.wikipedia.org/wiki/Cross-cutting_concern) such as logging or authentication. Let's say we have a following service:

    public interface IService
    {
        string CreateOrder(NetworkCredential credentials, Order orderToCreate);
        string DeleteOrder(NetworkCredential credentials, int orderId);
    }

    public class Service : IService
    {
        public string CreateOrder(NetworkCredential credentials, Order orderToCreate)
        {
            // ...

            return "Order was created succesfully.";
        }

        public string DeleteOrder(NetworkCredential credentials, int orderId)
        {
            // ...

            return "Order was deleted succesfully.";
        }
    }

We can create following interceptor:

    public class AuthorizationInterceptor : IInterceptor
    {
        public void Intercept(IInvocation invocation)
        {
            var userCredentials = invocation.Arguments[0] as NetworkCredential;
            
            if (userCredentials.UserName == "tom" && userCredentials.Password == "pass123")
                // this ^ verification is obviously silly, never do real security like this
            {
                invocation.Proceed();
            }
            else
            {
                invocation.ReturnValue = $"User '{userCredentials.UserName}' was not authenticated.";
            }
        }
    }

That can be registered and utilized like this:

    var container = new WindsorContainer();
    container.Register(
        Component.For<AuthorizationInterceptor>(),
        Component.For<IService>().ImplementedBy<Service>().Interceptors<AuthorizationInterceptor>());

    var service = container.Resolve<IService>();

    System.Diagnostics.Debug.Assert(
        service.DeleteOrder(new NetworkCredential { UserName = "paul", Password = "pass321" }, 8)
            == "User 'paul' was not authenticated.");

    System.Diagnostics.Debug.Assert(
        service.CreateOrder(new NetworkCredential { UserName = "tom", Password = "pass123" }, new Order())
            == "Order was created succesfully.");

The important lesson is that interceptor can decide to pass call and if it doesn't, it can supply arbitrary return value using `ReturnValue` property.

## Interceptors can be chained in a fixed order
For registration like this:

    var container = new WindsorContainer();
    container.Register(
        Component.For<FirstInterceptor>(),
        Component.For<SecondInterceptor>(),
        Component.For<ThirdInterceptor>(),

        Component.For<IService>()
            .ImplementedBy<Service>()
            .Interceptors<FirstInterceptor>()
            .Interceptors<SecondInterceptor>()
            .Interceptors<ThirdInterceptor>());

    var service = container.Resolve<IService>();
    service.CreateOrder(new Order());

With interceptors in the following spirit:

    public class FirstInterceptor : IInterceptor
    {
        public void Intercept(IInvocation invocation)
        {
            Console.WriteLine("First!");
            invocation.Proceed();
            Console.WriteLine("First.");
        }
    }

And with service implemented in the following way:

    public interface IService
    {
        void CreateOrder(Order orderToCreate);
    }

    public class Service : IService
    {
        public void CreateOrder(Order orderToCreate)
        {
            Console.WriteLine("Creating order...");

            // ...
        }
    }

We can observe output that demonstrates interceptors created in a fixed order are going to be executed in that order:

    First!
    Second!
    Third!
    Creating order...
    Third.
    Second.
    First.

## Interceptors can have dependencies
Interceptors are registered like regular components in Windsor. Like other components, they can depend on another components.

With following service for validating credentials:

    public interface ICredentialsVerifier
    {
        bool IsAuthorizedForService(NetworkCredential credentials);
    }

    public class MockCredentialsVerifier : ICredentialsVerifier
    {
        public bool IsAuthorizedForService(NetworkCredential credentials)
            => credentials.UserName == "tom" && credentials.Password == "pass123";
            // this ^ verification is obviously silly, never do real security like this
    }

We can use the following interceptor:

    public class AuthorizationInterceptor : IInterceptor
    {
        private readonly ICredentialsVerifier _credentialsVerifier;

        public AuthorizationInterceptor(ICredentialsVerifier credentialsVerifier)
        {
            _credentialsVerifier = credentialsVerifier;
        }

        public void Intercept(IInvocation invocation)
        {
            var userCredentials = invocation.Arguments[0] as NetworkCredential;

            if (_credentialsVerifier.IsAuthorizedForService(userCredentials))
            {
                invocation.Proceed();
            }
            else
            {
                invocation.ReturnValue = $"User '{userCredentials.UserName}' was not authenticated.";
            }
        }
    }

We just have to properly register it in the composition root like this:

    var container = new WindsorContainer();
    container.Register(
        Component.For<AuthorizationInterceptor>(),

        Component.For<ICredentialsVerifier>().ImplementedBy<MockCredentialsVerifier>(),
        Component.For<IService>().ImplementedBy<Service>().Interceptors<AuthorizationInterceptor>());

    var service = container.Resolve<IService>();

