---
title: "Code First - Fluent API"
slug: "code-first---fluent-api"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

There are two general ways of specifying HOW Entity Framework will map POCO classes to database tables, columns, etc.: **Data Annotations** and **Fluent API**.

While Data Annotations are a simple to read and understand, they lack of certain features such as specifying the "Cascade on Delete" behavior for an entity. The Fluent API on the other hand is a bit more complex to use, but provides a far more advanced set of features.

## Mapping models


## Composite Primary Key
By using the .HasKey() method, a set of properties can be explicitly configured as the composite primary key of the entity.

    using System.Data.Entity;    
    // ..
    
    public class PersonContext : DbContext
    {
        // ..

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            // ..

            modelBuilder.Entity<Person>().HasKey(p => new { p.FirstName, p.LastName });
        }
    }

## Maximum Length
By using the .HasMaxLength() method, the maximum character count can be configured for a property.

    using System.Data.Entity;    
    // ..
    
    public class PersonContext : DbContext
    {
        // ..
    
        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            // ..
    
            modelBuilder.Entity<Person>()
                        .Property(t => t.Name)
                        .HasMaxLength(100);
        }
    }

The resulting column with the specified column length:

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/KEYue.png

## Primary Key
By using the .HasKey() method, a property can be explicitly configured as primary key of the entity.

    using System.Data.Entity;    
    // ..
    
    public class PersonContext : DbContext
    {
        // ..

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            // ..

            modelBuilder.Entity<Person>().HasKey(p => p.PersonKey);
        }
    }

## Required properties (NOT NULL)
By using the .IsRequired() method, properties can be specified as mandatory, which means that the column will have a NOT NULL constraint.

    using System.Data.Entity;    
    // ..

    public class PersonContext : DbContext
    {
        // ..
    
        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            // ..
    
            modelBuilder.Entity<Person>()
                        .Property(t => t.Name)
                        .IsRequired();
        }
    }

The resulting column with the NOT NULL constraint:

[![enter image description here][1]][1]



 


  [1]: http://i.stack.imgur.com/VJm33.png

## Explict Foreign Key naming
When a navigation property exist on a model, Entity Framework will automatically create a Foreign Key column. If a specific Foreign Key name is desired but is not contained as a property in the model, it can be set explicitly using the Fluent API. By utilizing the `Map` method while establishing the Foreign Key relationship, any unique name can be used for Foreign Keys.

    public class Company
    {
        public int Id { get; set; }
    }

    public class Employee
    {
        property int Id { get; set; }
        property Company Employer { get; set; }
    }

    public class EmployeeContext : DbContext
    {
        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            modelBuilder.Entity<Employee>()
                        .HasRequired(x => x.Employer)
                        .WithRequiredDependent()
                        .Map(m => m.MapKey("EmployerId"));
        }
    }
 After specifying the relationship, the `Map` method allows the Foreign Key name to be explicitly set by executing `MapKey`. In this example, what would have resulted in a column name of Employer_Id is now EmployerId.

