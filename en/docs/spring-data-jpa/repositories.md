---
title: "Repositories"
slug: "repositories"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

The Spring Data project allows application programmers to work with data stores using a consistent interface that makes use of an abstraction called `Repository`. A Spring Data `Repository` is modeled after the [Repository pattern](http://martinfowler.com/eaaCatalog/repository.html) made popular by [domain-driven design](https://en.wikipedia.org/wiki/Domain-driven_design). Spring Data provides a central Java interface named `Repository` that subprojects can extend to provide features specific to data stores.

In addition to the `Repository` interface, Spring Data also provides two more core interfaces - `CrudRepository` that defines the contract for basic *CRUD* (*create*, *read*, *update* and *delete*) functionality; and `PagingAndSortingRepository` that extends `CrudRepository` by defining a contract for pagination and sorting.

These three core interfaces (`Repository`, `CrudRepository` and `PagingAndSortingRepository`) ensure that:

1. Application programmers can access data stores (such as relational databases, document based NoSQL databases, graph databases, etc.) in a consistent way.
1. It is easy to switch the underlying storage for a *domain entity* (see [domain-driven design](https://en.wikipedia.org/wiki/Domain-driven_design)) without having to also change the way in which the application interacts with the data store.
1. Specific implementations can provide features specific to data stores.

## Creating a repository for a JPA-managed entity
> **Entity** class
     
    @Entity
    @Table(name = "USER")
    public class User {
        @Id
        @Column(name = "ID")
        private Long id;
        
        @Column(name = "USERNAME")
        private String username;
        
        @ManyToOne
        @JoinColumn("ORGANIZATION_ID")
        private Organization organization;
    }

> **Repository** interface

    @Repository
    public interface UserRepository extends CrudRepository<User, Long> {
        public User findByUsername(String username);
    }

The method declaration in the interface will generate the following jpql query:

    select u from User u where u.username = :username

alternatively we can define a custom query:

    @Query("select u from User u where u.username = :username")
    public User findByUsername(@Param("username") String username) 

we can easily add sorting to the method declaration:

    public interface UserRepository extends PagingAndSortingRepository<User, Long> {
        public User findByUsernameOrderByUsernameAsc(String username);
    }

we can also use in-built pagination support:

    public Page<User> findByOrganizationPaged(Organization organization, Pageable pageable);

the service layer (or whoever calls this method) will then pass a PageRequest to the method:

    public Page<User> getByOrganizationPagedOrderByUsername(Organization organization, int page, int size, String direction){
        return userRepository.findByOrganizationPaged(organization, new PageRequest(page, size, Direction.valueOf(direction),
                    "username")
    }

## Finding all instances of an entity class
All instances (objects) of an entity class can be loaded from the underlying database table as follows (akin to retrieving all rows from the table):

    Iterable<Foo> foos = fooRepository.findAll();

The `findAll` method is provided by the `CrudRepository` interface. It returns an `Iterable` instead of a more concrete type like `List` or `Set` because [some implementations of the interface may be unable to return a `Collection` type](https://jira.spring.io/browse/DATACMNS-21) and therefore using a `Collection` type for the returned value will result in loss of functionality for them.

Invoking the `findAll` method results in the [JPA query](https://docs.oracle.com/javaee/6/tutorial/doc/bnbtg.html) `select foo from Foo foo` being executed on the underlying database.

## Finding a particular instance of an entity class by the identifier
A particular instance of an entity class can be loaded as follows:

    Foo foo = fooRepository.findOne(id);

The `findOne` method is provided by the `CrudRepository` interface. It expects an identifier that uniquely identifies an entity instance (for instance, a primary key in a database table). The Java type for the `id` parameter must match the type assigned to the entity attribute annotated with the JPA `@Id` annotation.

Invoking the `findOne` method results in the [JPA query](https://docs.oracle.com/javaee/6/tutorial/doc/bnbtg.html) `select foo from Foo foo where foo.[primary-key-column] = :id` being executed on the underlying database.

## Finding all instances of an entity class with an attribute matching a specified value
All instances of an entity class with one of the class attributes matching a specified value can be retrieved as follows:

    public interface FooRepository extends CrudRepository<Foo, Long> {
      List<Foo> findAllByName(String name);
    }

Invoking the `findAllByName` method results in the [JPA query](https://docs.oracle.com/javaee/6/tutorial/doc/bnbtg.html) `select foo from Foo foo where foo.name = :name` will be executed on the underlying database.

**Points to note**
1. `name` must be an attribute on the `Foo` entity class.
1. The method name must begin with `find`, `get` or `read`. Other keywords like `select` will not work.
1. There is no guarantee on the order in which the results will be returned.

