---
title: "Installers"
slug: "installers"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Basics - Creating and using an Installer
Installers are custom types that implement the `IWindsorInstaller` interface and are used to register `Component`s to the container, using the fluent registration API.

    public class MyInstaller : IWindsorInstaller
    {
        public void Install(IWindsorContainer container, IConfigurationStore store)
        {
            container.Register(Component.For<IMyType>()
                                        .ImplementedBy<ConcreteMyType1>());

            //Registering several components in one call to .Register
            container.Register(
                Component.For<IFoo>().ImplementedBy<Foo>(),
                Component.For<IBar>().ImplementedBy<Bar());
        }
    }

    //To use the installer:
    WindsorContainer container = new WindsorContainer();
    container.Install(new MyInstaller());

    container.Resolve<IFoo>();


 **Keep in mind**
> Installers must have public default constructor: When installers are
> instantiated by Windsor, they must have public default constructor.
> Otherwise an exception will be thrown.

## FromAssembly Class
Another way to `.Install` installers is by using Castle's `FromAssembly` class. It gives an array of functions to locate installers in the loaded assemblies. For example:

    //Will locate IInstallers in the current assembly that is calling the method
    container.Install(FromAssembly.This()); 

For more details see [Castle's Documentation][1]


  [1]: https://github.com/castleproject/Windsor/blob/master/docs/installers.md#fromassembly-class

## Installing from Configuration
Castle enables to register components also via XML Registration.

    //To install from the app/web.config
    container.Install(Configuration.FromAppConfig());

    //To install from an xml file
    Configuration.FromXmlFile("relative_path_to_file.xml");

Read Castle's documentation for ["What is it for"][1]


  [1]: https://github.com/castleproject/Windsor/blob/master/docs/xml-registration-reference.md#what-to-use-it-for

