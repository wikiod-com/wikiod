---
title: "Mapping relationship with Entity Framework Code First One-to-many and Many-to-many"
slug: "mapping-relationship-with-entity-framework-code-first-one-to-many-and-many-to-many"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

The topic discusses how you can map one-to-many and many-to-many relationships using Entity Framework Code First.

## Mapping one-to-many
So let's say you have two different entities, something like this:

    public class Person
    {
       public int PersonId { get; set; }
       public string Name { get; set; }
    }
    
    public class Car
    {
       public int CarId { get; set; }
       public string LicensePlate { get; set; }
    }
    
    public class MyDemoContext : DbContext
    {
       public DbSet<Person> People { get; set; }
       public DbSet<Car> Cars { get; set; }
    }

And you want to setup a one-to-many relationship between them, that is, one person can have zero, one or more cars, and one car belongs to one person exactly. Every relationship is bidirectional, so if a person has a car, the car belongs to that person. 

To do this just modify your model classes:

    public class Person
    {
       public int PersonId { get; set; }
       public string Name { get; set; }
       public virtual ICollection<Car> Cars { get; set; } // don't forget to initialize (use HashSet)
    }
    
    public class Car
    {
       public int CarId { get; set; }
       public string LicensePlate { get; set; }
       public int PersonId { get; set; }
       public virtual Person Person { get; set; }
    }


And that's it :) You already have your relationship set up. In the database, this is represented with foreign keys, of course.

![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v01.png)

## Mapping one-to-many: against the convention
In the last example, you can see that EF figures out which column is the foreign key and where should it point to. How? By using conventions. Having a property of type `Person` that is named `Person` with a `PersonId` property leads EF to conclude that `PersonId` is a foreign key, and it points to the primary key of the table represented by the type `Person`. 

But what if you were to change _PersonId_ to _OwnerId_ and _Person_ to _Owner_ in the __Car__ type?
```csharp
public class Car
{
   public int CarId { get; set; }
   public string LicensePlate { get; set; }
   public int OwnerId { get; set; }
   public virtual Person Owner { get; set; }
}
```
Well, unfortunately in this case, the conventions are not enough to produce the correct DB schema:
![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v03.png)

No worries; you can help EF with some hints about your relationships and keys in the model. Simply configure your `Car` type to use the `OwnerId` property as the FK. Create an entity type configuration and apply it in your `OnModelCreating()`:

    public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
    {
      public CarEntityTypeConfiguration()
      {
         this.HasRequired(c => c.Owner).WithMany(p => p.Cars).HasForeignKey(c => c.OwnerId);
      }
    }

This basically says that `Car` has a required property, `Owner` ([_HasRequired()_](https://msdn.microsoft.com/en-us/library/gg671317(v=vs.113).aspx)) and in the type of `Owner`, the `Cars` property is used to refer back to the car entities ([_WithMany()_](https://msdn.microsoft.com/en-us/library/gg696499(v=vs.113).aspx)). And finally the property representing the foreign key is specified ([_HasForeignKey()_](https://msdn.microsoft.com/en-us/library/mt137400(v=vs.113).aspx)). This gives us the schema we want:
![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v04.png)

You could configure the relationship from the `Person` side as well:

    public class PersonEntityTypeConfiguration : EntityTypeConfiguration<Person>
    {
      public PersonEntityTypeConfiguration()
      {
        this.HasMany(p => p.Cars).WithRequired(c => c.Owner).HasForeignKey(c => c.OwnerId);
      }
    }

The idea is the same, just the sides are different (note how you can read the whole thing: 'this person has many cars, each car with a required owner'). Doesn't matter if you configure the relationship from the `Person` side or the `Car` side. You can even include both, but in this case be careful to specify the same relationship on both sides!

## Mapping zero or one-to-many
In the previous examples a car cannot exist without a person. What if you wanted the person to be optional from the car side? Well, it's kind of easy, knowing how to do one-to-many. Just change the `PersonId` in `Car` to be nullable:

    public class Car
    {
        public int CarId { get; set; }
        public string LicensePlate { get; set; }
        public int? PersonId { get; set; }
        public virtual Person Person { get; set; }
    }

And then use the [_HasOptional()_](https://msdn.microsoft.com/en-us/library/gg671230(v=vs.113).aspx) (or [_WithOptional()_](https://msdn.microsoft.com/en-us/library/gg696231(v=vs.113).aspx), depending from which side you do the configuration):

    public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
    {
      public CarEntityTypeConfiguration()
      {
         this.HasOptional(c => c.Owner).WithMany(p => p.Cars).HasForeignKey(c => c.OwnerId);
      }
    }

## Many-to-many
Let's move on to the other scenario, where every person can have multiple cars and every car can have multiple owners (but again, the relationship is bidirectional). This is a many-to-many relationship. The easiest way is to let EF do it's magic using conventions. 

Just change the model like this:

     public class Person
    {
       public int PersonId { get; set; }
       public string Name { get; set; }
       public virtual ICollection<Car> Cars { get; set; }
    }
    
    public class Car
    {
       public int CarId { get; set; }
       public string LicensePlate { get; set; }        
       public virtual ICollection<Person> Owners { get; set; }
    }


And the schema:

![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v05.png)
Almost perfect. As you can see, EF recognized the need for a join table, where you can keep track of person-car pairings.

## Many-to-many: customizing the join table
You might want to rename the fields in the join table to be a little more friendly. You can do this by using the usual configuration methods (again, it doesn't matter which side you do the configuration from):


    public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
    {
       public CarEntityTypeConfiguration()
       {
          this.HasMany(c => c.Owners).WithMany(p => p.Cars)
              .Map(m =>
                  {
                     m.MapLeftKey("OwnerId");
                     m.MapRightKey("CarId");
                     m.ToTable("PersonCars");
                  }
            );
      }
    }


Quite easy to read even: this car has many owners ([_HasMany()_](https://msdn.microsoft.com/en-us/library/gg671281(v=vs.113).aspx)), with each owner having many cars ([_WithMany()_](https://msdn.microsoft.com/en-us/library/gg696499(v=vs.113).aspx)). Map this so that you map the left key to OwnerId ([_MapLeftKey()_](https://msdn.microsoft.com/en-us/library/system.data.entity.modelconfiguration.configuration.manytomanyassociationmappingconfiguration.mapleftkey(v=vs.113).aspx)), the right key to CarId ([_MapRightKey()_](https://msdn.microsoft.com/en-us/library/system.data.entity.modelconfiguration.configuration.manytomanyassociationmappingconfiguration.maprightkey(v=vs.113).aspx)) and the whole thing to the table PersonCars ([_ToTable()_](https://msdn.microsoft.com/en-us/library/gg679488(v=vs.113).aspx)). And this gives you exactly that schema:

![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v06.png)

## Many-to-many: custom join entity
I have to admit, I'm not really a fan of letting EF infer the join table wihtout a join entity. You cannot track extra information to a person-car association (let's say the date from which it is valid), because you can't modify the table. 

Also, the `CarId` in the join table is part of the primary key, so if the family buys a new car, you have to first delete the old associations and add new ones. EF hides this from you, but this means that you have to do these two operations instead of a simple update (not to mention that frequent inserts/deletes might lead to index fragmentation &mdash; good thing [there is an easy fix](http://www.dotnetfalcon.com/azure-automation-job-for-index-maintenance/) for that).

In this case what you can do is create a join entity that has a reference to both one specific car and one specific person. Basically you look at your many-to-many association as a combinations of two one-to-many associations:

    public class PersonToCar
    {
       public int PersonToCarId { get; set; }
       public int CarId { get; set; }
       public virtual Car Car { get; set; }
       public int PersonId { get; set; }
       public virtual Person Person { get; set; }
       public DateTime ValidFrom { get; set; }
    }
    
    public class Person
    {
      public int PersonId { get; set; }
      public string Name { get; set; }
      public virtual ICollection<PersonToCar> CarOwnerShips { get; set; }
    }
    
    public class Car
    {
      public int CarId { get; set; }
      public string LicensePlate { get; set; }        
      public virtual ICollection<PersonToCar> Ownerships { get; set; }
    }
    
    public class MyDemoContext : DbContext
    {
      public DbSet<Person> People { get; set; }
      public DbSet<Car> Cars { get; set; }
      public DbSet<PersonToCar> PersonToCars { get; set; }
    }


This gives me much more control and it's a lot more flexible. I can now add custom data to the association and every association has its own primary key, so I can update the car or the owner reference in them.

![](https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v07.png)

Note that this really is just a combination of two one-to-many relationships, so you can use all the configuration options discussed in the previous examples.

