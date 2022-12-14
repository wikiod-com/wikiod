---
title: "Mediator Pattern"
slug: "mediator-pattern"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Mediator pattern example in java
*Mediator* pattern defines an object (Mediator) that encapsulates how a set of objects interact. It enables many-to-many communication.

*UML diagram:* 

[![Mediator Diagram][1]][1]

Key components:

`Mediator:` Defines an interface for communication between Colleagues.

`Colleague`: Is an abstract class, which defines the events to be communicated between Colleagues

`ConcreteMediator`: Implements cooperative behavior by coordinating `Colleague` objects and maintains its colleagues

`ConcreteColleague`: Implements the notification operations received through `Mediator`, which has been generated by other `Colleague`
    
***One real world example:***

You are maintaining a network of computers in `Mesh` topology.

A mesh network is a network topology in which each node relays data for the network. All mesh nodes cooperate in the distribution of data in the network.

If a new computer is added Or existing computer is removed, all other computers in that network should know about these two events. 

Let's see how Mediator pattern fits into it.

Code snippet:

    import java.util.List;
    import java.util.ArrayList;
    
    /* Define the contract for communication between Colleagues. 
       Implementation is left to ConcreteMediator */
    interface Mediator{
        void register(Colleague colleague);
        void unregister(Colleague colleague);
    }
    /* Define the contract for notification events from Mediator. 
       Implementation is left to ConcreteColleague
    */
    abstract class Colleague{
        private Mediator mediator;
        private String name;
        
        public Colleague(Mediator mediator,String name){
            this.mediator = mediator;
            this.name = name;
        }
        public String toString(){
            return name;
        }
        public abstract void receiveRegisterNotification(Colleague colleague);
        public abstract void receiveUnRegisterNotification(Colleague colleague);    
    }
    /*  Process notification event raised by other Colleague through Mediator.   
    */
    class ComputerColleague extends Colleague {
        private Mediator mediator;
        
        public ComputerColleague(Mediator mediator,String name){
            super(mediator,name);
        }
        public  void receiveRegisterNotification(Colleague colleague){
            System.out.println("New Computer register event with name:"+colleague+
            ": received @"+this);
            // Send further messages to this new Colleague from now onwards
        }
        public  void receiveUnRegisterNotification(Colleague colleague){
            System.out.println("Computer left unregister event with name:"+colleague+
            ":received @"+this);
            // Do not send further messages to this Colleague from now onwards
        }
    }
    /* Act as a central hub for communication between different Colleagues. 
       Notifies all Concrete Colleagues on occurrence of an event
    */
    class NetworkMediator implements Mediator{
        List<Colleague> colleagues = new ArrayList<Colleague>();
        
        public NetworkMediator(){
        
        }
        
        public void register(Colleague colleague){
            colleagues.add(colleague);
            for (Colleague other : colleagues){
                if ( other != colleague){
                    other.receiveRegisterNotification(colleague);
                }
            }
        }
        public void unregister(Colleague colleague){
            colleagues.remove(colleague);
            for (Colleague other : colleagues){
                other.receiveUnRegisterNotification(colleague);
            }
        }
    }
    
    public class MediatorPatternDemo{
        public static void main(String args[]){
            Mediator mediator = new NetworkMediator();
            ComputerColleague colleague1 = new ComputerColleague(mediator,"Eagle");
            ComputerColleague colleague2 = new ComputerColleague(mediator,"Ostrich");
            ComputerColleague colleague3 = new ComputerColleague(mediator,"Penguin");
            mediator.register(colleague1);
            mediator.register(colleague2);
            mediator.register(colleague3);
            mediator.unregister(colleague1);
        }
    }

output:

    New Computer register event with name:Ostrich: received @Eagle
    New Computer register event with name:Penguin: received @Eagle
    New Computer register event with name:Penguin: received @Ostrich
    Computer left unregister event with name:Eagle:received @Ostrich
    Computer left unregister event with name:Eagle:received @Penguin

Explanation:

1. `Eagle` is added to network at first through register event. No notifications to any other colleagues since Eagle is the first one. 
2. When `Ostrich` is added to the network, `Eagle` is notified : Line 1 of output is rendered now.
3. When `Penguin` is added to network, both `Eagle` and `Ostrich`  have been notified : Line 2 and Line 3 of output is rendered now.
4. When `Eagle` left the network through unregister event, both `Ostrich` and `Penguin` have been notified. Line 4 and Line 5 of output is rendered now.



  [1]: http://i.stack.imgur.com/sK7yu.gif

