---
title: "Joined Inheritance strategy"
slug: "joined-inheritance-strategy"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## Parameters
| Annotation| Purpose |
| ------ | ------ |
| @Inheritance | Specifies type of inheritance strategy used |
| @DiscriminatorColumn | Specifies a column in database which will be used to identify different entities based on certain ID assigned to each entity|
|@MappedSuperClass| mapped super classes are not persistent and only used to hold state for its subclasses. Generally abstract java classes are marked with @MapperSuperClass|


## Joined inheritance strategy
A Sample class diagram based on which we will see JPA implementation. 
[![enter image description here][1]][1]

    
    @Entity
    @Table(name = "VEHICLE")
    @Inheritance(strategy = InheritanceType.JOINED)
    @DiscriminatorColumn(name = "VEHICLE_TYPE")
    public abstract class Vehicle {
    
        @TableGenerator(name = "VEHICLE_GEN", table = "ID_GEN", pkColumnName = "GEN_NAME", valueColumnName = "GEN_VAL", allocationSize = 1)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "VEHICLE_GEN")
        private int idVehicle;
        private String manufacturer;
    
        // getters and setters
    }

**TransportationVehicle.java**

    
    @MappedSuperclass
    public abstract class TransportationVehicle extends Vehicle {
    
        private int loadCapacity;
    
        // getters and setters    
    }

**Truck.java**

    @Entity
    public class Truck extends TransportationVehicle {
    
        private int noOfContainers;
    
        // getters and setters
    
    }

**PassengerVehicle.java**

    @MappedSuperclass
    public abstract class PassengerVehicle extends Vehicle {
    
        private int noOfpassengers;
    
        // getters and setters
    }

**Car.java**

    @Entity
    public class Car extends PassengerVehicle {
    
        private int noOfDoors;
    
        // getters and setters    
    }

**Bike.java**

    @Entity
    public class Bike extends PassengerVehicle {
    
        private int saddleHeight;
    
        // getters and setters
    
    }

**Test Code**

    /* Create EntityManagerFactory */
    EntityManagerFactory emf = Persistence
            .createEntityManagerFactory("AdvancedMapping");
    
    /* Create EntityManager */
    EntityManager em = emf.createEntityManager();
    EntityTransaction transaction = em.getTransaction();
    
    transaction.begin();
    
    Bike cbr1000rr = new Bike();
    cbr1000rr.setManufacturer("honda");
    cbr1000rr.setNoOfpassengers(1);
    cbr1000rr.setSaddleHeight(30);
    em.persist(cbr1000rr);
    
    Car aventador = new Car();
    aventador.setManufacturer("lamborghini");
    aventador.setNoOfDoors(2);
    aventador.setNoOfpassengers(2);
    em.persist(aventador);
    
    Truck truck = new Truck();
    truck.setLoadCapacity(1000);
    truck.setManufacturer("volvo");
    truck.setNoOfContainers(2);
    em.persist(truck);
    
    transaction.commit();

Database diagram would be as below. 
[![enter image description here][2]][2]



  [1]: http://i.stack.imgur.com/jPJMW.png
  [2]: http://i.stack.imgur.com/Bi39t.png

The advantage of joined inheritance strategy is that it does not waste database space as in single table strategy. On the other hand, because of multiple joins involved for every insertion and retrieval, performance becomes an issue when inheritance hierarchies become wide and deep.

Full example with explanation can be read [here][1]


  [1]: http://www.thejavageek.com/2014/05/17/jpa-joined-inheritance-example/

