---
title: "Getting started with Entity Framework"
slug: "getting-started-with-entity-framework"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installing The Entity Framework NuGet Package
In your Visual Studio open the **Solution Explorer** window then <kbd>right click</kbd> on your project then choose *Manage NuGet Packages* from the menu:

[![Manage nuget packages][1]][1]

In the window that opens type `EntityFramework` in the search box in the top right. 

[![enter image description here][2]][2]

Or if you are using Visual Studio 2015 you'll see something like this:

[![enter image description here][3]][3]

Then click Install.

We can also install entity framework using the package manager console. To do you have first to open it using the *Tools menu -> NuGet Package Manager -> Package Manager Console* then enter this:

    Install-Package EntityFramework

[![enter image description here][4]][4]

This will install Entity Framework and automatically add a reference to the assembly in your project.


  [1]: http://i.stack.imgur.com/Wx3Hk.png
  [2]: http://i.stack.imgur.com/NgmOs.png
  [3]: http://i.stack.imgur.com/ln0Z9.png
  [4]: http://i.stack.imgur.com/6iSJR.png

## Using Entity Framework from C# (Code First)
Code first allows you to create your entities (classes) without using a GUI designer or a .edmx file. It is named *Code first*, because you can create your models *first* and *Entity framework* will create database according to mappings for you automatically. Or you can also use this approach with existing database, which is called *code first with existing database*  For example, if you want a table to hold a list of planets:

    public class Planet
    {
        public string Name { get; set; }
        public decimal AverageDistanceFromSun { get; set; }
    }

Now create your context which is the bridge between your entity classes and the database. Give it one or more `DbSet<>` properties:

    using System.Data.Entity;

    public class PlanetContext : DbContext
    {
        public DbSet<Planet> Planets { get; set; }
    }

We can use this by doing the following:

    using(var context = new PlanetContext())
    {
        var jupiter = new Planet 
        {
            Name = "Jupiter", 
            AverageDistanceFromSun = 778.5
        };
    
        context.Planets.Add(jupiter);
        context.SaveChanges();
    }

In this example we create a new `Planet` with the `Name` property with the value of `"Jupiter"` and the `AverageDistanceFromSun` property with the value of `778.5`

We can then add this `Planet` to the context by using the `DbSet`'s `Add()` method and commit our changes to the database by using the `SaveChanges()` method.

Or we can retrieve rows from the database:

    using(var context = new PlanetContext())
    {
        var jupiter = context.Planets.Single(p => p.Name == "Jupiter");
        Console.WriteLine($"Jupiter is {jupiter.AverageDistanceFromSun} million km from the sun.");
    }

## What is Entity Framework ?
Writing and managing ADO.Net code for data access is a tedious and monotonous job. Microsoft has provided an **O/RM framework called "Entity Framework"** to automate database related activities for your application.

Entity framework is an Object/Relational Mapping (O/RM) framework. It is an enhancement to ADO.NET that gives developers an automated mechanism for accessing & storing the data in the database.

**What is O/RM?**

ORM is a tool for storing data from domain objects to the relational database like MS SQL Server, in an automated way, without much programming. 
O/RM includes three main parts: 

 1. Domain class objects
 2. Relational database objects 
 3. Mapping information on how domain objects map to relational database objects(**e.x** tables, views & stored procedures) 

ORM allows us to keep our database design separate from our domain class design. This makes the application maintainable and extendable. It also automates standard CRUD operation (Create, Read, Update & Delete) so that the developer doesn't need to write it manually.

