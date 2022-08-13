---
title: "Method injection"
slug: "method-injection"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## A Simple Example of Method Injection in c#
    using System;

    namespace ConsoleApplication1
    {
        class Program
        {
            static void Main(string[] args)
            {
                var foo = new Dependency();
                var bar = new ClassWithDependency();

                bar.DoSomething(foo); //Inject dependency as method parameter

                Console.ReadLine();
            }
        }

        public interface IDependency
        {
            void DoSomething();
        }

        public class Dependency: IDependency
        {
            public void DoSomething()
            {
                Console.WriteLine("Hello");
            }
        }

        public class ClassWithDependency
        {
            public void DoSomething(IDependency dependency)
            {
                dependency.DoSomething();
            }
        }
    }


