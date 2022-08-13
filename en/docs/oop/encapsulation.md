---
title: "Encapsulation"
slug: "encapsulation"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Information hiding
The state of an object at a given time is represented by the information that it holds at that point. In an OO language, the state is implemented as member variables.

In a properly designed object, the state can be changed only by means of calls to its methods and not by direct manipulation of its member variables. This is achieved by providing public methods that operate on the values of private member variables. The hiding of information in this manner is known as *encapsulation*.

Therefore, encapsulation ensures that private information is not exposed and cannot be modified except through calls to accessors and methods, respectively.

In the following example, you cannot set an `Animal` to be no longer hungry by changing the `hungry` private field; instead, you have to invoke the method `eat()`, which alters the state of the `Animal` by setting the `hungry` flag to `false`.

    public class Animal {
        private boolean hungry;
    
        public boolean isHungry() {
            return this.hungry;
        }
        
        public void eat() {
            this.hungry = false;
        }
    }

