---
title: "Mappings"
slug: "mappings"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Xml Mappings
The xml mapping uses a `hbm.xml` file which is a hibernate mapping file. It is a syntax xml file which contains the metadata required for the object/relational mapping. The metadata includes declaration of persistent classes and the mapping of properties (to columns and foreign key relationships to other entities) to database tables.

Add a file named Entity.hbm.xml into the project and set it as `embedded resource` on the properties tab. For sample, Customer.hbm.xml:

    <?xml version="1.0" encoding="utf-8" ?>
    <hibernate-mapping xmlns="urn:nhibernate-mapping-2.2"
        namespace="Project" assembly="Project">
    
        <class name="Customer" table="CUSTOMERS">
    
            <id name="Id">
                <column name="Customer_Id" sql-type="int" not-null="true"/>
                <generator class="native" />
            </id>
    
            <!-- A cat has to have a name, but it shouldn' be too long. -->
            <property name="Name">
                <column name="Name" length="60" not-null="true" />
            </property>
            <property name="Sex" />
            <property name="Weight" />
            <property name="Active" />
            <property name="Birthday" />
        </class>
    
    </hibernate-mapping>

The `hibernate-mapping` tag contains the namespace and assembly project information. The `class` tag contains the name of the entity on the project and the table which is been mapped. The `id` tag contains the mapping for the `primary key` where the column is specified by the `column` tag and `generator` tag define how the id is generated. The `property` tag contains information for the other columns in the database.




## Fluent NHibernate Mappings
The [`Fluent NHibernate`](http://www.fluentnhibernate.org/) is a library to help you to map the entities using C# code instead of xml mappings. Fluent NHibernate uses the [`fluent pattern`](https://en.wikipedia.org/wiki/Fluent_interface) and it is based on conventions to create the mappings and it gives you the power of the visual studio tools (such as intellisense) to improve the way you map your entities. 

Add the reference of the [Fluent NHibernate from Nuget](https://www.nuget.org/packages/FluentNHibernate/) on your project and add a class CustomerMap.cs:

    namespace Project.Mappings
    {
        public class CustomerMap : ClassMap<Customer>
        {
            public CustomerMap()
            {
                Table("CUSTOMERS");

                Id(x => x.Id).Column("Customer_Id").GeneratedBy.Native();
                
                //map a property while specifying the max-length as well as setting 
                //it as not nullable. Will result in the backing column having
                //these characteristics, but this will not be enforced in the model!
                Map(x => x.Name)
                    .Length(16)
                    .Not.Nullable();

                Map(x => x.Sex);

                Map(x => x.Weight);

                Map(x => x.Active);

                //Map a property while specifying the name of the column in the database
                Map(x => x.Birthday, "BIRTHDAY");

                //Maps a many-to-one relationship
                References(x => x.Company);

                //Maps a one-to-many relationship, while also defining which 
                //column to use as key in the foreign table.
                HasMany(x => x.Orders).KeyColumn("CustomerPk");
            }
        }
    }

The `CustomerMap` class inhirits from `ClassMap<T>` that is the base class for mapping and contains all methods necessary to create the map of your `T` entity. The method `Table` define the table name you are mapping. The `Id` method is used to map the `primery key` column. The `Map` method is used to map other columns.

## A sample of Model to Map
NHibernate uses classes to map into tables or views. Creating a [`Plain Old CLR Object`][1] (POCOs, sometimes called Plain Ordinary CLR Objects) is a good practice for persistent classes. A [POCO][1] has its data accessible through the standard .NET property mechanisms, shielding the internal representation from the publicly visible interface.

    namespace Project
    {
        public class Customer
        {
            public virtual string Id { get; set; }
    
            public virtual string Name { get; set; }
    
            public virtual char Sex { get; set; }
    
            public virtual float Weight { get; set;}
    
            public virtual bool Active { get; set;}
    
            public virtual DateTime Birthday { get; set;}
    
            public Customer()
            {
            }
        }
    }

NHibernate is not restricted in its usage of property types: all .NET types and primitives (like string, char and DateTime) can be mapped, including classes from the `System.Collections` and `System.Collections.Generics` namespaces. You can also map a relation between the entities, having properties that refer to another entity type. You can map them as values, collections of values, or associations to other entities. The property named `Id` here is a special property that represents the database identifier (primary key) of that class, which is highly recommended for entities like a Cat. NHibernate can use identifiers internally only, without having to declare them on the class, but we would lose some of the flexibility in our application architecture.

No special interface has to be implemented for persistent classes nor do we have to subclass from a special root persistent class. NHibernate also doesn't use any build time processing, such as IL manipulation; it relies solely on .NET reflection and runtime class enhancement. So, without any dependency in the POCO class on NHibernate, we can map it to a database table or view.

For the above mentioned runtime class enhancement to work, NHibernate requires that all public properties of an entity class are declared as `virtual`. The entity class must have a no-arguments constructor (`protected` or `public`) for NHibernate to create the objects.

  [1]: https://en.wikipedia.org/wiki/Plain_Old_CLR_Object

