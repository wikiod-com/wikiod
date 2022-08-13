---
title: "Property Injection"
slug: "property-injection"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## A Very Simple Example of Property Injection with C# with a Lazy-loaded Local Default

    using System;

    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                var foo = new ClassWithDependency();

                foo.DoSomething();

                var bar = new InjectedDependency();

                foo.Dependency = bar; //Injecting the dependency via a property

                foo.DoSomething();

                Console.ReadLine();
            }

        }

        public interface IDependency
        {
            void DoSomething();
        }

        public class DefaultDependency: IDependency
        {
            public void DoSomething()
            {
                Console.WriteLine("Default behaviour");
            }
        }

        public class InjectedDependency: IDependency
        {
            public void DoSomething()
            {
                Console.WriteLine("Different behaviour");
            }
        }

        public class ClassWithDependency
        {
            private IDependency _dependency;

            public IDependency Dependency
            {
                get
                {
                    if (_dependency == null) Dependency = new DefaultDependency();
                    return _dependency;
                }
                set { _dependency = value; }
            }
 
            public void DoSomething()
            {
                Dependency.DoSomething();
            }
        }
    }


## A Simple Example of Property Injection, setting the default via constructor injection
    using System;

    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                var dep = new DefaultDependency();
                var foo = new ClassWithDependency(dep);

                foo.DoSomething();

                var bar = new InjectedDependency();

                foo.Dependency = bar; //Injecting the dependency via a property

                foo.DoSomething();

                Console.ReadLine();
            }

        }

        public interface IDependency
        {
            void DoSomething();
        }

        public class DefaultDependency: IDependency
        {
            public void DoSomething()
            {
                Console.WriteLine("Default behaviour");
            }
        }

        public class InjectedDependency: IDependency
        {
            public void DoSomething()
            {
                Console.WriteLine("Different behaviour");
            }
        }

        public class ClassWithDependency
        {
            public ClassWithDependency(IDependency dependency)
            {
                Dependency = dependency;
            }

            public IDependency Dependency { get; set; }

            public void DoSomething()
            {
                Dependency.DoSomething();
            }
        }
    }

