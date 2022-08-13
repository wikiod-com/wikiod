---
title: "Table per concrete class inheritance strategy"
slug: "table-per-concrete-class-inheritance-strategy"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

 - Vehicle, TransportationVehicle and PassengerVehicle are abstract
   classes and they will not have separate table in database.
 - Truck, Car and Bike are concrete classes so they will be mapped to
   corresponding tables. These tables should include all the fields for
   classes annotated with @MappedSuperClass because they don’t have
   corresponding tables in database.
 - So, Truck table will have columns to store fields inherited from
   TransportationVehicle and Vehicle.
 - Similarly, Car and Bike will have columns to store fields inherited
   from PassengerVehicle and Vehicle.

Full example can be found [here][1]


  [1]: http://www.thejavageek.com/2014/05/17/jpa-table-per-concrete-class-example/

## Table per concrete class inheritance strategy
We will take vehicle hierarchy example as depicted below. 
[![enter image description here][1]][1]

**Vehicle.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.Entity;
    import javax.persistence.GeneratedValue;
    import javax.persistence.GenerationType;
    import javax.persistence.Id;
    import javax.persistence.Inheritance;
    import javax.persistence.InheritanceType;
    import javax.persistence.TableGenerator;
    
    @Entity
    @Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
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

**TransportationVehilcle.java**

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

**Truck.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.Entity;
    
    @Entity
    public class Truck extends TransportationVehicle {
    
        private int noOfContainers;
    
        public int getNoOfContainers() {
            return noOfContainers;
        }
    
        public void setNoOfContainers(int noOfContainers) {
            this.noOfContainers = noOfContainers;
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

**Car.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.Entity;
    
    @Entity
    public class Car extends PassengerVehicle {
    
        private int noOfDoors;
    
        public int getNoOfDoors() {
            return noOfDoors;
        }
    
        public void setNoOfDoors(int noOfDoors) {
            this.noOfDoors = noOfDoors;
        }
    
    }

**Bike.java**

    package com.thejavageek.jpa.entities;
    
    import javax.persistence.Entity;
    
    @Entity
    public class Bike extends PassengerVehicle {
    
        private int saddleHeight;
    
        public int getSaddleHeight() {
            return saddleHeight;
        }
    
        public void setSaddleHeight(int saddleHeight) {
            this.saddleHeight = saddleHeight;
        }
    
    }

Database representation will be as below
[![enter image description here][2]][2]


  [1]: http://i.stack.imgur.com/Kx9zd.png
  [2]: http://i.stack.imgur.com/MHMBh.png

