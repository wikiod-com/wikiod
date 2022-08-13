---
title: "Getting started with unity-container"
slug: "getting-started-with-unity-container"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
In order to get started, you just need to install the **Unity** nuget package.
Run the following command from the package manager console:

    PM> Install-Package Unity

Alternatively, you can use Visual Studio to install Unity on a particular project using the *Manage NuGet Packages for Solution* option under Tools -> NuGet Package Manager.

## Hello World
    interface IGreeter
    {
        void Greet();
    }

    class Greeter : IGreeter
    {
        public void Greet()
        {
            Console.WriteLine("Hello World");
        }
    }

    class SpanishGreeter : IGreeter
    {
        public void Greet()
        {
            Console.WriteLine("Hola Mundo");
        }
    }

    class FrenchGreeter : IGreeter
    {
        public void Greet()
        {
            Console.WriteLine("Bonjour le Monde");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var container = new UnityContainer()
                .RegisterType<IGreeter, SpanishGreeter>("spanish")
                .RegisterType<IGreeter, FrenchGreeter>("french")
                .RegisterType<IGreeter, Greeter>();

            //Get default registration. Outputs "Hello World"
            var greeter = container.Resolve<IGreeter>();
            greeter.Greet();

            //Get specific named registration. Outputs "Hola Mundo"
            greeter = container.Resolve<IGreeter>("spanish");
            greeter.Greet();

            //Get all named registrations (excludes the default one)
            //Outputs "Hola Mundo" and "Bonjour le Monde"
            foreach (var g in container.ResolveAll<IGreeter>())
            {
                g.Greet();
            }

            Console.ReadLine();
        }
    }

## Constructor Injection
    interface IService
    {
        void ProcessRequest();
    }

    interface IRepository
    {
        IEnumerable<string> GetData();
    }

    class HelloWorldRepository : IRepository
    {
        public IEnumerable<string> GetData()
        {
            return new[] { "Hello", "World" };
        }
    }

    class HelloWorldService : IService
    {
        private readonly IRepository repo;
        public HelloWorldService(IRepository repo)
        {
            this.repo = repo;
        }
        public void ProcessRequest()
        {
            Console.WriteLine(String.Join(" ", this.repo.GetData()));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            var container = new UnityContainer()
                .RegisterType<IRepository, HelloWorldRepository>()
                .RegisterType<IService, HelloWorldService>();

            //Unity automatically resolves constructor parameters that knows about.
            //It will return a HelloWorldService with a HelloWorldRepository
            var greeter = container.Resolve<IService>();
            //Outputs "Hello World"
            greeter.ProcessRequest();            

            Console.ReadLine();
        }
    }

