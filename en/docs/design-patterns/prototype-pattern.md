---
title: "Prototype Pattern"
slug: "prototype-pattern"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

The Prototype pattern is a creational pattern that creates new objects by cloning existing prototype object. The prototype pattern speeds up the instantiation of classes when copying objects is faster.

The prototype pattern is a creational design pattern. It is used when the type of objects to create is determined by a prototypical instance, which is *"cloned"* to produce new objects.

This pattern is used when a class need a *"polymorphic (copy) constructor"*.



## Prototype Pattern (C#)
The prototype pattern can be implemented using the [ICloneable][1] interface in .NET.

    class Spoon {
    }
    class DessertSpoon : Spoon, ICloneable {
      ...
      public object Clone() {
        return this.MemberwiseClone();
      }
    }
    class SoupSpoon : Spoon, ICloneable {
      ...
      public object Clone() {
        return this.MemberwiseClone();
      }
    }

  [1]: https://msdn.microsoft.com/en-us/library/system.icloneable(v=vs.110).aspx


## Prototype Pattern (JavaScript)
In the classical languages like Java, C# or C++ we start by creating a class and then we can create new objects from the class or we can extend the class.

In JavaScript first we create an object, then we can augment the object or create new objects from it. So i think, JavaScript demonstrates actual prototype than the classical language.

Example :

    var myApp = myApp || {};

    myApp.Customer = function (){
        this.create = function () {
            return "customer added";
        }
    };
    
    myApp.Customer.prototype = {
        read: function (id) {
            return "this is the customer with id = " + id;
        },
        update: function () {
            return "customer updated";
        },
        remove: function () {
            return "customer removed";
        }
    };
Here, we create an object named `Customer`, and then without creating *new object* we extended the existing `Customer object` using *prototype* keyword. This technique is known as **Prototype Pattern**.

## Prototype Pattern (C++)
    class IPrototype  {
    public:
        virtual ~IPrototype() = default;
    
        auto Clone() const { return std::unique_ptr<IPrototype>{DoClone()}; }
        auto Create() const { return std::unique_ptr<IPrototype>{DoCreate()}; }

    private:
        virtual IPrototype* DoClone() const = 0;
        virtual IPrototype* DoCreate() const = 0;
    };

    class A : public IPrototype {
    public:
        auto Clone() const { return std::unique_ptr<A>{DoClone()}; }
        auto Create() const { return std::unique_ptr<A>{DoCreate()}; }
    private:
        // Use covariant return type :)
        A* DoClone() const override { return new A(*this); }
        A* DoCreate() const override { return new A; }
    };
    
    class B : public IPrototype {
    public:
        auto Clone() const { return std::unique_ptr<B>{DoClone()}; }
        auto Create() const { return std::unique_ptr<B>{DoCreate()}; }
    private:
        // Use covariant return type :)
        B* DoClone() const override { return new B(*this); }
        B* DoCreate() const override { return new B; }
    };
    
    
    class ChildA : public A {
    public:
        auto Clone() const { return std::unique_ptr<ChildA>{DoClone()}; }
        auto Create() const { return std::unique_ptr<ChildA>{DoCreate()}; }

    private:
        // Use covariant return type :)
        ChildA* DoClone() const override { return new ChildA(*this); }
        ChildA* DoCreate() const override { return new ChildA; }
    };

That allows to construct the derived class from a base class pointer:

    ChildA childA;
    A& a = childA;
    IPrototype& prototype = a;

    // Each of the following will create a copy of `ChildA`:
    std::unique_ptr<ChildA> clone1 = childA.Clone();
    std::unique_ptr<A> clone2 = a.Clone();
    std::unique_ptr<IPrototype> clone3 = prototype.Clone();

    // Each of the following will create a new default instance `ChildA`:
    std::unique_ptr<ChildA> instance1 = childA.Create();
    std::unique_ptr<A> instance2 = a.Create();
    std::unique_ptr<IPrototype> instance3 = prototype.Create();


