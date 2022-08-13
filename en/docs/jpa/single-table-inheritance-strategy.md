---
title: "Single Table Inheritance Strategy"
slug: "single-table-inheritance-strategy"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Parameters
| Annotation| Purpose |
| ------ | ------ |
| @Inheritance   | Specifies type of inheritance strategy used   |
| @DiscriminatorColumn | Specifies a column in database which will be used to identify different entities based on certain ID assigned to each entity|
|@MappedSuperClass| mapped super classes are not persistent and only used to hold state for its subclasses. Generally abstract java classes are marked with @MapperSuperClass|
|@DiscriminatorValue| A value specified in column defined by @DiscriminatorColumn. This value helps identify the type of entity|


The advantage of single table strategy is it does not require complex joins for retrieval and insertion of entities, but on the other hand it wastes database space as many columns need to be nullable and there isn’t any data for them.

Complete example and article can be found [here][1] 


  [1]: http://www.thejavageek.com/2014/05/14/jpa-single-table-inheritance-example/

## Single table inheritance strategy
A simple example of Vehicle hierarchy can be taken for single table inheritance strategy. 

[![enter image description here][1]][1]

Abstract Vehicle class:

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.DiscriminatorColumn;
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.Inheritance;
    import javax.persistence.InheritanceType;
    import javax.persistence.Table;
    import javax.persistence.TableGenerator;
    
    @Entity
    @Table(name = "VEHICLE")
    @Inheritance(strategy = InheritanceType.SINGLE_TABLE)
    @DiscriminatorColumn(name = "VEHICLE_TYPE")
    public abstract class Vehicle {
    
        @TableGenerator(name = "VEHICLE_GEN", table = "ID_GEN", pkColumnName = "GEN_NAME", valueColumnName = "GEN_VAL", allocationSize = 1)
        @Id
        @GeneratedValue(strategy = GenerationType.TABLE, generator = "VEHICLE_GEN")
        private int idVehicle;
        private String manufacturer;
    
        public int getIdVehicle() {
            return idVehicle;
        }
    
        public void setIdVehicle(int idVehicle) {
            this.idVehicle = idVehicle;
        }
    
        public String getManufacturer() {
            return manufacturer;
        }
    
        public void setManufacturer(String manufacturer) {
            this.manufacturer = manufacturer;
        }
    
    }

**TransportableVehicle.java**
package com.thejavageek.jpa.entities;

import javax.persistence.MappedSuperclass;

    @MappedSuperclass
    public abstract class TransportationVehicle extends Vehicle {
    
        private int loadCapacity;
    
        public int getLoadCapacity() {
            return loadCapacity;
        }
    
        public void setLoadCapacity(int loadCapacity) {
            this.loadCapacity = loadCapacity;
        }
    
    }

**PassengerVehicle.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.MappedSuperclass;
    
    @MappedSuperclass
    public abstract class PassengerVehicle extends Vehicle {
    
        private int noOfpassengers;
    
        public int getNoOfpassengers() {
            return noOfpassengers;
        }
    
        public void setNoOfpassengers(int noOfpassengers) {
            this.noOfpassengers = noOfpassengers;
        }
    
    }

**Truck.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.DiscriminatorValue;
    import javax.persistence.Entity;
    
    @Entity
    @DiscriminatorValue(value = "Truck")
    public class Truck extends TransportationVehicle{
    
        private int noOfContainers;
    
        public int getNoOfContainers() {
            return noOfContainers;
        }
    
        public void setNoOfContainers(int noOfContainers) {
            this.noOfContainers = noOfContainers;
        }
    
    }

**Bike.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.DiscriminatorValue;
    import javax.persistence.Entity;
    
    @Entity
    @DiscriminatorValue(value = "Bike")
    public class Bike extends PassengerVehicle {
    
        private int saddleHeight;
    
        public int getSaddleHeight() {
            return saddleHeight;
        }
    
        public void setSaddleHeight(int saddleHeight) {
            this.saddleHeight = saddleHeight;
        }
    
    }

**Car.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.DiscriminatorValue;
    import javax.persistence.Entity;
    
    @Entity
    @DiscriminatorValue(value = "Car")
    public class Car extends PassengerVehicle {
    
        private int noOfDoors;
    
        public int getNoOfDoors() {
            return noOfDoors;
        }
    
        public void setNoOfDoors(int noOfDoors) {
            this.noOfDoors = noOfDoors;
        }
    
    }

**Test Code:**

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
    
    Car avantador = new Car();
    avantador.setManufacturer("lamborghini");
    avantador.setNoOfDoors(2);
    avantador.setNoOfpassengers(2);
    em.persist(avantador);
    
    Truck truck = new Truck();
    truck.setLoadCapacity(100);
    truck.setManufacturer("mercedes");
    truck.setNoOfContainers(2);
    em.persist(truck);
    
    transaction.commit();


  [1]: http://i.stack.imgur.com/e8DKZ.png

