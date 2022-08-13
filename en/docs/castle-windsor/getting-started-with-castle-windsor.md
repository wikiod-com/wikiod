---
title: "Getting started with castle-windsor"
slug: "getting-started-with-castle-windsor"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World - Castle Windsor
<!-- language: c# -->

    class Program
    {
        static void Main(string[] args)
        {
            //Initialize a new container
            WindsorContainer container = new WindsorContainer();

            //Register IService with a specific implementation and supply its dependencies
            container.Register(Component.For<IService>()
                                        .ImplementedBy<SomeService>()
                                        .DependsOn(Dependency.OnValue("dependency", "I am Castle Windsor")));

            //Request the IService from the container
            var service = container.Resolve<IService>();
            
            //Will print to console: "Hello World! I am Castle Windsor
            service.Foo();
    }

Services:

<!-- language: c# -->

    public interface IService
    {
        void Foo();
    }

    public class SomeService : IService
    {
        public SomeService(string dependency)
        {
            _dependency = dependency;
        }

        public void Foo()
        {
            Console.WriteLine($"Hello World! {_dependency}");
        }

        private string _dependency;
    }

## Installation
Castle Windsor is available via [NuGet][1]

 1. Use the "Manage NuGet Packages" and search for "castle windsor"
    - To download for [Visual Studio 2015][2]
    - To download for [previous versions][3]
 2. Use Package Manager Console to execute:

        Install-Package Castle.Windsor


Now you can use it to handle dependencies in your project.

<!-- language: c# -->

    var container = new WindsorContainer(); // create instance of the container
    container.Register(Component.For<IService>().ImplementedBy<Service>()); // register depndency
    var service = container.Resolve<IService>(); // resolve with Resolve method

See [official documentation][4] for more details.

_`Castle.Windsor` package depends on `Castle.Core` package and it will install it too_


  [1]: https://www.nuget.org/packages/Castle.Windsor
  [2]: https://visualstudiogallery.msdn.microsoft.com/5d345edc-2e2d-4a9c-b73b-d53956dc458d
  [3]: https://visualstudiogallery.msdn.microsoft.com/27077b70-9dad-4c64-adcf-c7cf6bc9970c
  [4]: https://github.com/castleproject/Windsor/blob/master/docs/README.md

