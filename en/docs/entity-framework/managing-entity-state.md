---
title: "Managing entity state"
slug: "managing-entity-state"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Entities in Entity Framework can have various states that are listed by the [System.Data.Entity.EntityState](https://msdn.microsoft.com/en-us/library/system.data.entity.entitystate(v=vs.113).aspx) enumeration. These states are:

    Added    
    Deleted    
    Detached    
    Modified    
    Unchanged


Entity Framework works with POCOs. That means that entities are simple classes that have no properties and methods to manage their own state. Entity state is managed by a context itself, in the `ObjectStateManager`.

This topic covers various ways to set entity state.

## Setting state Added of a single entity
`EntityState.Added` can be set in two fully equivalent ways:

1. By setting the state of its entry in the context:

        context.Entry(entity).State = EntityState.Added;

1. By adding it to a `DbSet` of the context:

        context.Entities.Add(entity);

When calling `SaveChanges`, the entity will be inserted into the database. When it's got an identity column (an auto-set, auto-incrementing primary key), then after `SaveChanges`, the primary key property of the entity will contain the newly generated value, *even when this property already had a value*.

## Setting state Added of an object graph
Setting the state of an *object graph* (a collection of related entities) to `Added` is different than setting a single entity as `Added` (see [this example](https://www.wikiod.com/entity-framework/managing-entity-state#Setting state Added of a single entity)).

In the example, we store planets and their moons:

*Class model*

    public class Planet
    {
        public Planet()
        {
            Moons = new HashSet<Moon>();
        }
        public int ID { get; set; }
        public string Name { get; set; }
        public ICollection<Moon> Moons { get; set; }
    }
    
    public class Moon
    {
        public int ID { get; set; }
        public int PlanetID { get; set; }
        public string Name { get; set; }
    }

*Context*

    public class PlanetDb : DbContext
    {
        public property DbSet<Planet> Planets { get; set; }
    }

We use an instance of this context to add planets and their moons:

## Example ##

    var mars = new Planet { Name = "Mars" };
    mars.Moons.Add(new Moon { Name = "Phobos" });
    mars.Moons.Add(new Moon { Name = "Deimos" });
    
    context.Planets.Add(mars);
    
    Console.WriteLine(context.Entry(mars).State);
    Console.WriteLine(context.Entry(mars.Moons.First()).State);

Output:

    Added
    Added

What we see here is that adding a `Planet` also sets the state of a moon to `Added`.

When setting an entity's state to `Added`, all entities in its navigation properties (properties that "navigate" to other entities, like `Planet.Moons`) are also marked as `Added`, *unless they already are attached to the context*.

