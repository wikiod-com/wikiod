---
title: "Abstract Factory"
slug: "abstract-factory"
draft: false
images: []
weight: 9979
type: docs
toc: true
---

## Address validator factory - requires a parameter to select an implementation
Scenario: You need to select an implementation of address validation when a sales order is submitted, and the validator is determined by the country to which the order is shipping. The factory needs to inspect some value passed as an argument to select the correct implementation.

First, write an interface for the factory:

    public interface IAddressValidatorFactory
    {
        IAddressValidator GetAddressValidator(Address address);
        void Release(IAddressValidator created);
    }

The interface indicates that the implementation of `IAddressValidator` will be determined by the `Address` passed as an argument to `GetAddressValidator`.

When we register multiple implementations of the same interface with Windsor we typically name them. So the factory will need to return the name of an implementation. In this case, let's say we have several implementations of `IAddressValidator` registered with our container:

    container.Register(
        Component.For<IAddressValidator,UnitedStatesAddressValidator>()
            .Named("AddressValidatorFor_USA"),
        Component.For<IAddressValidator, FinlandAddressValidator>()
            .Named("AddressValidatorFor_FIN"),
        Component.For<IAddressValidator, MalawiAddressValidator>()
            .Named("AddressValidatorFor_MWI"),
        Component.For<IAddressValidator, CommonCountryAddressValidator>()
            .Named("FallbackCountryAddressValidator")
            .IsDefault()            
        );

The job of our factory will to take an `Address` and return the name of the implementation that Windsor will resolve. That requires a *component selector.*

    public class AddressValidatorSelector : DefaultTypedFactoryComponentSelector
    {
        public AddressValidatorSelector() 
            : base(fallbackToResolveByTypeIfNameNotFound: true) { }

        protected override string GetComponentName(MethodInfo method, object[] arguments)
        {
            return "AddressValidatorFor_" + ((Address)arguments[0]).CountryCode;
        }
    }

This class tells Windsor to look for an implementation of `IAddressValidator` named according to the country code of the order. For Finland that's "AddressValidatorFor_Fin".  
And if there is no validator for that specific country we can fall back to a default validator. We could also use the same validator for multiple countries by registering the same implementation more than once with different names. 

We wrote the code for the component selector, but we don't write the code for the factory itself. Windsor provides the factory, and we tell it to use our `AddressValidatorSelector`.

    // Add the factory facility once.
    container.AddFacility<TypedFactoryFacility>();
    container.Register(
        Component.For<IAddressValidatorFactory>()
            .AsFactory(new AddressValidatorSelector()));

When a class has a dependency on `IAddressValidatorFactory` Windsor will inject its own implementation using the component selector we supplied.

## Simple abstract factory - factory takes no parameters
Scenario: You need to resolve a dependency when a method is called, *not* in the constructor.

Solution: Inject an abstract factory into the constructor. When the method is called, it requests the dependency from the abstract factory, which in turn resolves it from the container. (Your class depends on the factory but never calls the container itself.)

Declare an interface for your factory*:

    public interface IFooFactory
    {
        IFoo CreateFoo();
        void Release(IFoo foo);
    }

Add the `TypedFactoryFacility` to your container:

    container.AddFacility<TypedFactoryFacility>();

Instruct the container to use the `TypedFactoryFacility` to resolve dependencies on `IFooFactory`:

    container.Register(
        Component.For<IFooFactory>().AsFactory(),
        Component.For<IFoo, MyFooImplementation>());

You don't need to create an instance of a factory. Windsor does that.

You can now inject the factory into a class and use it like this:

    public class NeedsFooFactory
    {
        private readonly IFooFactory _fooFactory;

        public NeedsFooFactory(IFooFactory fooFactory)
        {
            _fooFactory = fooFactory;
        }

        public void MethodThatNeedsFoo()
        {
            var foo = _fooFactory.CreateFoo();
            foo.DoWhatAFooDoes();
            _fooFactory.Release(foo);
        }
    }

Calling the `Release` method causes the container to release the component it resolved. Otherwise it won't be released until `_fooFactory` is released (which is whenever `NeedsFooFactory` is released.)

*Windsor infers which method is the "create" method and which is the "release" method. If a method returns something then it's assumed that the container must resolve it. If a method returns nothing (`void`) then it's the "release" method.

