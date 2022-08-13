---
title: "Dependency Injection"
slug: "dependency-injection"
draft: false
images: []
weight: 9931
type: docs
toc: true
---

The general idea behind Dependency Injection is that you design your application around loosely coupled components while adhering to the Dependency Inversion Principle. By not depending on concrete implementations, allows to design highly flexible systems.

The basic idea behind dependency injection is to create more loosely coupled code.  When a class, rather than newing up its own dependencies, takes in its dependencies instead, the class becomes more simple to test as a unit ([unit testing][1]).

To further elaborate on loose coupling - the idea is that classes become dependent on abstractions, rather than concretions.   If class `A` depends on another concrete class `B`, then there is no real testing of `A` without `B`.  While this sort of test can be OK, it does not lend itself to unit testable code.  A loosely coupled design would define an abstraction `IB` (as an example) that class `A` would depend on.  `IB` can then be mocked to provide testable behavior, rather than relying on the real implementation of `B` to be able to provide testable scenarios to `A`.

Tightly coupled example (C#):

<!-- language-all: c# -->

    public class A
    {
        public void DoStuff()
        {
            B b = new B();
            b.Foo();
        }
    }

In the above, class `A` depends on `B`.  There is no testing `A` without the concrete `B`.  While this is fine in an integration testing scenario, it is difficult to unit test `A`.

A more loosely coupled implementation of the above could look like:

    public interface IB
    {
        void Foo();
    }

    public class A
    {
        private readonly IB _iB;

        public A(IB iB)
        {
            _iB = iB;
        }

        public void DoStuff()
        {
            _b.Foo();
        }
    }

The two implementations seem quite similar, there is however an important difference.  Class `A` is no longer directly dependent on class `B`, it is now dependent on `IB`.  Class `A` no longer has the [responsibility][2] of newing up its own depedencies - they must now be provided ***to*** `A`.


  [1]: https://www.wikiod.com/unit-testing
  [2]: https://en.wikipedia.org/wiki/Single_responsibility_principle

## Setter injection (C#)
<!-- language: c# -->

    public class Foo
    {
        private IBar _iBar;
        public IBar iBar { set { _iBar = value; } }

        public void DoStuff()
        {
            _iBar.DoSomething();
        }
    }

    public interface IBar
    {
        void DoSomething();
    }

## Constructor Injection (C#)
<!-- language: c# -->

    public class Foo
    {
        private readonly IBar _iBar;

        public Foo(IBar iBar)
        {
            _iBar = iBar;
        }

        public void DoStuff()
        {
            _bar.DoSomething();
        }
    }

    public interface IBar
    {
        void DoSomething();
    }

