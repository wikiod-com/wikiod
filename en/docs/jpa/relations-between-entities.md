---
title: "Relations between entities"
slug: "relations-between-entities"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Relations Between Entities Basics

A **foreign key** can be one or more columns that reference a unique key, usually the primary key, in another table.

A foreign key and the primary parent key it references must have the same number and type of fields.

Foreign keys represents **relationships** from a column or columns in one table to a column or columns in another table.



## @JoinTable Annotation Example
When mapping many-to-many relationships in JPA, configuration for the table used for the joining foreign-keys can be provided using the `@JoinTable` annotation:

    @Entity
    public class EntityA {
       @Id
       @Column(name="id")
       private long id;
       [...]
       @ManyToMany
       @JoinTable(name="table_join_A_B",
                  joinColumns=@JoinColumn(name="id_A"), referencedColumnName="id"
                  inverseJoinColumns=@JoinColumn(name="id_B", referencedColumnName="id"))
       private List<EntityB> entitiesB;
       [...]
    }

    @Entity
    public class EntityB {
       @Id
       @Column(name="id")
       private long id;
       [...]
    }
In this example, which consists of EntityA having a many-to-many relation to EntityB, realized by the `entitiesB` field, we use the @JoinTable annotation to specify that the table name for the join table is `table_join_A_B`, composed by the columns `id_A` and `id_B`, foreign keys respectively referencing column `id` in EntityA's table and in EntityB's table; `(id_A,id_B)` will be a composite primary-key for `table_join_A_B` table.


## Multiplicity in Entity Relationships
## Multiplicity in Entity Relationships
Multiplicities are of the following types:
- **One-to-one**: Each entity instance is related to a single instance of another entity.
- **One-to-many**: An entity instance can be related to multiple instances of the other entities.
- **Many-to-one**: multiple instances of an entity can be related to a single instance of the other entity.
- **Many-to-many**: The entity instances can be related to multiple instances of each other.

## One-to-One Mapping
One-to-one mapping defines a single-valued association to another entity that has one-to-one multiplicity. This relationship mapping use the `@OneToOne` annotation on the corresponding persistent property or field.

 *Example: `Vehicle` and `ParkingPlace` entities.*

## One-to-Many Mapping
An entity instance can be related to multiple instances of the other entities.

 One-to-many relationships use the `@OneToMany` annotation on the corresponding persistent property or field.

 The `mappedBy` element is needed to refer to the attribute annotated by ManyToOne in the corresponding entity:

     @OneToMany(mappedBy="attribute")

 A one-to-many association needs to map the collection of entities.

## Many-to-One Mapping
A many-to-one mapping is defined by annotating the attribute in the source entity (the attribute that refers to the target entity) with the `@ManyToOne` annotation.

 A `@JoinColumn(name="FK_name")` annotation discribes a foreing key of a relationship.

## Many-to-Many Mapping
The entity instances can be related to multiple instances of each other.

 Many-to-many relationships use the `@ManyToMany` annotation on the corresponding persistent property or field.

 We must use a third table to associate the two entity types (join table).

