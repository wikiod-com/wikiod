---
title: "Hibernate Entity Relationships using Annotations"
slug: "hibernate-entity-relationships-using-annotations"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Parameters
| Annotation | Details |  
| --------- | ------- |  
| `@OneToOne` | Specifies a one to one relationship with a corresponding object. |
| `@OneToMany`| Specifies a single object that maps to many objects.|
| `@ManyToOne`| Specifies a collection of objects that map to a single object.|
| `@Entity`| Specifies an object that maps to a database table.|
| `@Table`| Specifies which database table this object maps too.|
| `@JoinColumn`| Specifies which column a foregin key is stored in.
| `@JoinTable`| Specifies an intermediate table that stores foreign keys.|


## Bi-Directional Many to Many using user managed join table object
    @Entity
    @Table(name="FOO")
    public class Foo {
        private UUID fooId;
        
        @OneToMany(mappedBy = "bar")
        private List<FooBar> bars;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;
        
        @OneToMany(mappedBy = "foo")
        private List<FooBar> foos;
    }

    @Entity
    @Table(name="FOO_BAR")
    public class FooBar {
        private UUID fooBarId;

        @ManyToOne
        @JoinColumn(name = "fooId")
        private Foo foo;

        @ManyToOne
        @JoinColumn(name = "barId")
        private Bar bar;

        //You can store other objects/fields on this table here.
    }

Specifies a two-way relationship between many `Foo` objects to many `Bar` objects using an intermediate join table that the user manages.

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. The relationships between `Foo` and `Bar` objects are stored in a table called `FOO_BAR`. There is a `FooBar` object as part of the application.

Commonly used when you want to store extra information on the join object such as the date the relationship was created.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/zR6jB.png

## Bi-Directional Many to Many using Hibernate managed join table
    @Entity
    @Table(name="FOO")
    public class Foo {
        private UUID fooId;
        
        @OneToMany
        @JoinTable(name="FOO_BAR",
            joinColumns = @JoinColumn(name="fooId"),
            inverseJoinColumns = @JoinColumn(name="barId"))
        private List<Bar> bars;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;
        
        @OneToMany
        @JoinTable(name="FOO_BAR",
            joinColumns = @JoinColumn(name="barId"),
            inverseJoinColumns = @JoinColumn(name="fooId"))
        private List<Foo> foos;
    }

Specifies a relationship between many `Foo` objects to many `Bar` objects using an intermediate join table that Hibernate manages. 

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. The relationships between `Foo` and `Bar` objects are stored in a table called `FOO_BAR`. However this implies that there is no `FooBar` object as part of the application.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/1qk3Z.png

## Bi-directional One to Many Relationship using foreign key mapping
    @Entity
    @Table(name="FOO") 
    public class Foo {
        private UUID fooId;
        
        @OneToMany(mappedBy = "bar")
        private List<Bar> bars;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;
        
        @ManyToOne
        @JoinColumn(name = "fooId")
        private Foo foo;
    }

Specifies a two-way relationship between one `Foo` object to many `Bar` objects using a foreign key.

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. The foreign key is stored on the `BAR` table in a column called `fooId`.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/Jh9TL.png

## Bi-Directional One to One Relationship managed by Foo.class
    @Entity
    @Table(name="FOO")    
    public class Foo {
        private UUID fooId;
        
        @OneToOne(cascade = CascadeType.ALL)
        @JoinColumn(name = "barId")
        private Bar bar;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;
        
        @OneToOne(mappedBy = "bar")
        private Foo foo;
    }


Specifies a two-way relationship between one `Foo` object to one `Bar` object using a foreign key.

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. The foreign key is stored on the `FOO` table in a column called `barId`.

Note that the `mappedBy` value is the field name on the object, not the column name.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ZmICg.png

## Uni-Directional One to Many Relationship using user managed join table
    @Entity
    @Table(name="FOO")
    public class Foo {
        private UUID fooId;
        
        @OneToMany
        @JoinTable(name="FOO_BAR",
            joinColumns = @JoinColumn(name="fooId"),
            inverseJoinColumns = @JoinColumn(name="barId", unique=true))
        private List<Bar> bars;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;

        //No Mapping specified here.
    }

    @Entity
    @Table(name="FOO_BAR")
    public class FooBar {
        private UUID fooBarId;

        @ManyToOne
        @JoinColumn(name = "fooId")
        private Foo foo;

        @ManyToOne
        @JoinColumn(name = "barId", unique = true)
        private Bar bar;

        //You can store other objects/fields on this table here.
    }

Specifies a one-way relationship between one `Foo` object to many `Bar` objects using an intermediate join table that the user manages. 

This is similar to a `ManyToMany` relationship, but if you add a `unique` constraint to the target foreign key you can enforce that it is `OneToMany`.

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. The relationships between `Foo` and `Bar` objects are stored in a table called `FOO_BAR`. There is a `FooBar` object as part of the application.

Notice that there is no mapping of `Bar` objects back to `Foo` objects. `Bar` objects can be manipulated freely without affecting `Foo` objects.

Very commonly used with Spring Security when setting up a `User` object who has a list of `Role`'s that they can perform. You can add and remove roles to a user without having to worry about cascades deleting `Role`'s.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/ffpGs.png

## Uni-directional One to One Relationship
    @Entity
    @Table(name="FOO")
    public class Foo {
        private UUID fooId;
        
        @OneToOne
        private Bar bar;
    }

    @Entity
    @Table(name="BAR")
    public class Bar {
        private UUID barId;
        //No corresponding mapping to Foo.class
    }

Specifies a one-way relationship between one `Foo` object to one `Bar` object.

The `Foo` objects are stored as rows in a table called `FOO`. The `Bar` objects are stored as rows in a table called `BAR`. 

Notice that there is no mapping of `Bar` objects back to `Foo` objects. `Bar` objects can be manipulated freely without affecting `Foo` objects.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/5nJYw.png

