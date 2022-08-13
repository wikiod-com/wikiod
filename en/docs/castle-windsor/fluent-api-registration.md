---
title: "Fluent API Registration"
slug: "fluent-api-registration"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Registering Several Types of Same Interface
When registering types to the container `Castle` uses the type of the class in order to resolve. In the case that there is more than one registration for a specific type the `Name` property must be set:

    public void Install(IWindsorContainer container, IConfigurationStore store)
    {
        container.Register(
            Component.For<IFoo>().ImplementedBy<Foo>().Named("Registration1"),
            Component.For<IBar>().ImplementedBy<Bar().Named("Registration2"));
    } 

## Specifying Lifestyle
A `Lifestyle` is the "how" Castle controls the scope in which a component is used and when to clean it up. The built-in lifestyles are `Singelton`, `Transient`, `PerWebRequest`, `Scoped`, `Bound`, `PerThread` and `Pooled`

    container.Register(
       Component.For<IFoo>()
                .ImplementedBy<Foo>()
                .LifestyleSingleton(),

        Component.For<IBar>()
                 .ImplementedBy<Bar>()
                 .LifestyleTransient());

## DependsOn - Specify Dependencies
The entire idea with dependency injection is that a class does not instantiate its dependencies but requests them (through constructor or property). Using Castle the way for specifying the way to resolve a dependency is by using the `DependsOn`:

    public class Foo : IFoo
    {
        public Foo(IBar bar, string val)
        {
            Bar = bar;
            Val = val;
        }
        public IBar Bar { get; set; }
        public string Val { get; set; }
    }

    container.Register(
        Component.For<IBar>().ImplementedBy<Bar>().Named("bar1"),
        Component.For<IBar>().ImplementedBy<Bar>().Named("bar2"),

        Component.For<IFoo>()
                 .ImplementedBy<Foo>()
                 .DependsOn(Dependency.OnComponent("bar", "bar1"),
                            Dependency.OnValue("val","some value")));

## Interceptors
When registering a component use the `Interceptors()` method to specify what are the interceptors/types of interceptors to be used for this component:

_The `TInterceptor` must implement the `IInterceptor` interface_

**A single interceptor by type:**

    container.Register(
        Component.For<MyInterceptor>(),
        Component.For<IFoo>()
                 .ImplementedBy<Foo>()
                 .Interceptors<MyInterceptor>());

**Two interceptors by type:**

    container.Register(
        Component.For<MyInterceptor1>(),
        Component.For<MyInterceptor2>(),
        Component.For<IFoo>()
                 .ImplementedBy<Foo>()
                 .Interceptors<MyInterceptor1, MyInterceptor2>());

**More than 2 interceptors by type:**

    container.Register(
        Component.For<MyInterceptor1>(),
        Component.For<MyInterceptor2>(),
        Component.For<MyInterceptor3>(),
        Component.For<IFoo>()
                 .ImplementedBy<Foo>()
                 .Interceptors(typeof(MyInterceptor1), 
                               typeof(MyInterceptor2), 
                               typeof(MyInterceptor3)));

