---
title: "Performance tuning"
slug: "performance-tuning"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Use composition instead of inheritance
Hibernate has some strategies of inheritance. The `JOINED` inheritance type do a JOIN between the child entity and parent entity.

The problem with this approach is that Hibernate **always** bring the data of all involved tables in the inheritance. 

Per example, if you have the entities `Bicycle` and `MountainBike` using the `JOINED` inheritance type:


    @Entity
    @Inheritance(strategy = InheritanceType.JOINED)
    public abstract class Bicycle {
    
    }

And:

    @Entity
    @Inheritance(strategy = InheritanceType.JOINED)
    public class MountainBike extends Bicycle {
    
    }

Any JPQL query that hit `MountainBike` will brings the `Bicycle` data, creating a SQL query like:

    select mb.*, b.* from MountainBike mb JOIN Bicycle b ON b.id = mb.id WHERE ...

If you have another parent for `Bicycle` (like `Transport`, per example), this above query will brings the data from this parent too, doing an extra JOIN.

As you can see, this is a kind of `EAGER` mapping too. You don't have the choice to bring only the data of the `MountainBike` table using this inheritance strategy.

The best for performance is use composition instead of inheritance.

To accomplish this, you can mapping the `MountainBike` entity to have a field `bicycle`:

    @Entity
    public class MountainBike {
    
        @OneToOne(fetchType = FetchType.LAZY)
        private Bicycle bicycle;
    
    }

And `Bicycle`:

    @Entity
    public class Bicycle {
    
    }

Every query now will bring only the `MountainBike` data by default.

## Don't use EAGER fetch type
Hibernate can use two types of fetch when you are mapping the relationship between two entities: `EAGER` and `LAZY`.

In general, the `EAGER` fetch type is not a good idea, because it tells JPA to _always_ fetch the data, even when this data is not necessary.

Per example, if you have a `Person` entity and the relationship with `Address` like this:

    @Entity
    public class Person {

      @OneToMany(mappedBy="address", fetch=FetchType.EAGER)
      private List<Address> addresses;

    }

Any time that you query a `Person`, the list of `Address` of this `Person` will be returned too.

So, instead of mapping your entity with:

    @ManyToMany(mappedBy="address", fetch=FetchType.EAGER)

Use:

    @ManyToMany(mappedBy="address", fetch=FetchType.LAZY)

Another thing to pay attention is the relationships `@OneToOne` and `@ManyToOne`. Both of them are EAGER _by default_. So, if you are concerned about the performance of your application, you need to set the fetch for this type of relationship:

    @ManyToOne(fetch=FetchType.LAZY)

And:

    @OneToOne(fetch=FetchType.LAZY)

