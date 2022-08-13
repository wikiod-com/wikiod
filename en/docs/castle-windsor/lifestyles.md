---
title: "Lifestyles"
slug: "lifestyles"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Standard Lifestyles
When a `Component` is resolved from the Windsor container it must have a definition of the scope it is in. By scope meaning if and how it is reused and when to release the object for the Garbage Collector to destroy. This is the `LifeStlye` of the `Component`.

The way to specify a LifeStyle is by registering a component. The two most common `LifeStyles` are:

 1.  [`Transient`][1] - Each time the component is resolved a new instance of it is produced by the container.

         Container.Register(Component.For<Bar>().LifestyleTransient());

 2.  [`Singleton`][2] - Each time the component is resolved the same instance will be returned by the container

         Container.Register(Component.For<Foo>().LifestyleSingleton());
    

>Singleton is the default lifestyle, which will be use if you don't specify any explicitly.

Other built-in `LifeStyles` include `PerWebRequest`,`Scoped`, `Bound`, `PerThread`, `Pooled`

For more details about the different lifestyles and for what each is good for, refer to [Castle's Documentation][3]


  [1]: https://github.com/castleproject/Windsor/blob/master/docs/lifestyles.md#transient
  [2]: https://github.com/castleproject/Windsor/blob/master/docs/lifestyles.md#singleton
  [3]: https://github.com/castleproject/Windsor/blob/master/docs/lifestyles.md#lifestyles

## Custom LifeStyle - IScopeAccessor
By implementing your custom `IScopeAccessor` you can create  different types of scopes. For the following example I have the two classes `Foo` and `Bar` in which `Bar` will be registered with a custom `LifeStyle`.

_Each have an Id to assist with testing_

    public class Foo
    {
        public Guid FooId { get; } = Guid.NewGuid();
    }

    public class Bar
    {
        public Guid BarId { get; } = Guid.NewGuid();
    }

To register `Bar` as a `LifestyleScoped<T>` I implemented `FooScopeAccessor`:

    public class FooScopeAccessor : IScopeAccessor
    {
        private static readonly ConcurrentDictionary<Foo, ILifetimeScope> collection = new ConcurrentDictionary<Foo, ILifetimeScope>();

        public ILifetimeScope GetScope(CreationContext context)
        {
            return collection.GetOrAdd(context.AdditionalArguments["scope"] as Foo, new DefaultLifetimeScope());
        }

        public void Dispose()
        {
            foreach (var scope in collection)
            {
                scope.Value.Dispose();
            }
            collection.Clear();
        }
    }

Registering and Resolving:

    WindsorContainer container = new WindsorContainer();
    container.Register(Component.For<Foo>().LifestyleTransient());

    var foo1 = container.Resolve<Foo>(); // FooId = 004350ac-40ff-4d1a-8022-7977f94eb418
    var foo2 = container.Resolve<Foo>(); // FooId = 714aad8a-e4a2-4950-9017-e387c1c56133

    container.Register(Component.For<Bar>().LifestyleScoped<FooScopeAccessor>());

    var bar1 = container.Resolve<Bar>(new Dictionary<string, Foo> { ["scope"] = foo1 });     
    //                                              c144ba90-ce37-45c2-89d4-593d127fb723

    var bar2 = container.Resolve<Bar>(new Dictionary<string, Foo> { ["scope"] = foo1 });
    //                                              c144ba90-ce37-45c2-89d4-593d127fb723

    var bar3 = container.Resolve<Bar>(new Dictionary<string, Foo> { ["scope"] = foo2 }); 
    //                                              bcfe7ba4-cfb3-4b6e-8ecc-a3a3e5055bea
 
    var bar4 = container.Resolve<Bar>(new Dictionary<string, Foo> { ["scope"] = foo1 }); 
    //                                              c144ba90-ce37-45c2-89d4-593d127fb723

As seen above `bar1`, `bar2` and `bar3` which were Resolved using `Foo1` are all reference to the same object while `bar4` has been Resolved with a new instance of `Bar`

For more details about implementing a custom `IScopeAccessor` refer to [Castle's Documentation][1]


  [1]: https://github.com/castleproject/Windsor/blob/master/docs/implementing-custom-scope.md#implementing-custom-scope

