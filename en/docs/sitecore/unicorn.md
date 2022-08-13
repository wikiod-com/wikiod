---
title: "Unicorn"
slug: "unicorn"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Unicorn is a utility for Sitecore that solves the issue of moving templates, renderings, and other database items between Sitecore instances. This becomes problematic when developers have their own local instances - packages are error-prone and tend to be forgotten on the way to production. Unicorn solves this issue by writing serialized copies of Sitecore items to disk along with the code - this way, a copy of the necessary database items for a given codebase accompanies it in source control.

Useful Information can be found here:
- [Unicorn on Github][1]
- [Founder of Unicorn - Kam Figy's Blog][2]
- [Sitecore: Getting Started with Unicorn!][3]


  [1]: https://github.com/kamsar/Unicorn
  [2]: http://kamsar.net/index.php/category/Unicorn/
  [3]: http://blog.karbyn.com/articles/sitecore-getting-started-with-unicorn/

## Initial Setup
+ You'll need [Sitecore 6.6][1] or later (including [Sitecore 8.x][2]). Note that for Sitecore 6.6 compatibility you must have .NET 4.5 installed.
+ Install Unicorn. This is as simple as adding the [Unicorn NuGet package][3] to your project.

PM> Install-Package Unicorn

+ When you install the NuGet package, a [README file][4] will come up in Visual Studio with help to get you started.


  [1]: https://sdn.sitecore.net/SDN5/Products/Sitecore%20V5/Sitecore%20CMS%206/Update/6_6_0_rev_140410.aspx
  [2]: https://dev.sitecore.net/Downloads.aspx
  [3]: https://www.nuget.org/packages/Unicorn/
  [4]: https://github.com/kamsar/Unicorn/blob/master/Build/Unicorn.nuget/readme.txt

## Manual Installation/Install from Source
 - Clone the repository
- Place a copy of your Sitecore.Kernel.dll assembly in /lib/sitecore/v7 (for v7/v8)
- Build the project for your Sitecore version using Visual Studio 2012 or later
- Copy Unicorn.dll, Rainbow.dll, Rainbow.Storage.Sc.dll, Rainbow.Storage.Yaml.dll and Kamsar.WebConsole.dll to your main project in whatever fashion you wish (project reference, as binary references, etc)
- Copy Standard Config Files\*.config to the App_Config\Include\Unicorn folder
- Configure to your liking; the [setup README file][1] is a good starting point.
- Hit $yoursite/unicorn.aspx to perform initial serialization of your configured predicate


  [1]: https://github.com/kamsar/Unicorn/blob/master/Build/Unicorn.nuget/readme.txt

## Data Provider Architecture
There are two components to the Unicorn data provider: the database-specific implementation, and the Unicorn implementation.

The Unicorn implementation is an individual configuration of Unicorn dependencies that get automatic serialization. For example, if you were serializing two presets you'd need two instances of `UnicornDataProvider` - one for each `IPredicate` implementation.

The database specific implementation is a subclass of the original Sitecore data provider that provides a container for one or more `UnicornDataProvider` instances. Out of the box, a `UnicornSqlServerDataProvider` is provided. You could roll your own if you're on Oracle. This provider is effectively an unblockable event handler that allows Unicorn to trap item changes even if the evil `EventDisabler` class is being used.

If you want to wire multiple Unicorn data providers to your database, you create a class that derives from `UnicornSqlServerDataProvider`. In this class you can select to:
- Create a constructor that injects your provider(s) using the base constructor:

    
    public MyDataProvider(string connectionString) :
    base(connectionString, new UnicornDataProvider(), new
    UnicornDataProvider(), ...)

- Create a constructor that injects your provider(s) using code (this is better if you have to construct dependencies, etc that don't fit well in a base call):


    public MyDataProvider(string connectionString) : base(connectionString, null)
     {
        AddUnicornDataProvider(new UnicornDataProvider());
        // ...
     }

