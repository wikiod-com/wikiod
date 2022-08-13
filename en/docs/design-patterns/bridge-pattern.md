---
title: "Bridge Pattern"
slug: "bridge-pattern"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Bridge pattern implementation in java
Bridge pattern decouples abstraction from implementation so that both can vary independently. It has been achieved with composition rather than inheritance.

Bridge UML diagram from wikipedia:

[![enter image description here][1]][1]

You have four components in this pattern.

`Abstraction`: It defines an interface

`RefinedAbstraction`: It implements abstraction:

`Implementor`: It defines an interface for implementation

`ConcreteImplementor`: It implements Implementor interface.

***`The crux of Bridge pattern :`*** Two orthogonal class hierarchies using composition (and no inheritance). The Abstraction hierarchy and Implementation hierarchy can vary independently. Implementation  never refers Abstraction. Abstraction contains Implementation interface as a member (through composition).  This composition reduces one more level of inheritance hierarchy. 

Real word Use case:

*Enable different vehicles to have both versions of manual and auto gear system.*

Example code:

<!-- language: java -->
      
    /* Implementor interface*/
    interface Gear{
        void handleGear();
    }
    
    /* Concrete Implementor - 1 */
    class ManualGear implements Gear{
        public void handleGear(){
            System.out.println("Manual gear");
        }
    }
    /* Concrete Implementor - 2 */
    class AutoGear implements Gear{
        public void handleGear(){
            System.out.println("Auto gear");
        }
    }
    /* Abstraction (abstract class) */
    abstract class Vehicle {
        Gear gear;
        public Vehicle(Gear gear){
            this.gear = gear;
        }
        abstract void addGear();
    }
    /* RefinedAbstraction - 1*/
    class Car extends Vehicle{
        public Car(Gear gear){
            super(gear);
            // initialize various other Car components to make the car
        }
        public void addGear(){
            System.out.print("Car handles ");
            gear.handleGear();
        }
    }
    /* RefinedAbstraction - 2 */
    class Truck extends Vehicle{
        public Truck(Gear gear){
            super(gear);
            // initialize various other Truck components to make the car
        }
        public void addGear(){
            System.out.print("Truck handles " );
            gear.handleGear();
        }
    }
    /* Client program */
    public class BridgeDemo {    
        public static void main(String args[]){
            Gear gear = new ManualGear();
            Vehicle vehicle = new Car(gear);
            vehicle.addGear();
            
            gear = new AutoGear();
            vehicle = new Car(gear);
            vehicle.addGear();
            
            gear = new ManualGear();
            vehicle = new Truck(gear);
            vehicle.addGear();
            
            gear = new AutoGear();
            vehicle = new Truck(gear);
            vehicle.addGear();
        }
    }

 
output:

    Car handles Manual gear
    Car handles Auto gear
    Truck handles Manual gear
    Truck handles Auto gear


Explanation: 

1. `Vehicle` is an abstraction. 
2. `Car` and `Truck` are two concrete implementations of `Vehicle`.
3. `Vehicle` defines an abstract method : `addGear()`. 
4. `Gear` is implementor interface
5. `ManualGear` and `AutoGear` are two implementations of  `Gear`
8. `Vehicle` contains `implementor` interface rather than implementing the interface. `Compositon` of implementor interface is crux of this pattern : *It allows abstraction and implementation to vary independently.* 
7. `Car` and `Truck` define implementation ( redefined abstraction) for abstraction : `addGear()` : It contains  `Gear` - Either `Manual` or `Auto`

**Use case(s) for Bridge pattern**:

1. **Abstraction** and **Implementation** can change independent each other and they are not bound at compile time
2. Map orthogonal hierarchies - One for *Abstraction* and one for *Implementation*. 

  [1]: http://i.stack.imgur.com/9LeNi.png



