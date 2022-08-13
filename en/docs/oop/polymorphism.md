---
title: "Polymorphism"
slug: "polymorphism"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

## Method Overriding
Method overriding is the way of using polymorphism between classes. if one class is inherited from another, the former (sub class) can override the latter's (super class's) methods, and change the implementation.

this is used where the super class defines the more general implementation of the method while the sub class uses a more specific one.

Consider following example:

We have a class for Mammals:

    class Mammal {
      void whoIam () {
        System.out.println("I am a Mammal");
      }
    }

Then we have a class for Dog, which is a Mammal:

    class Dog extends Mammal {
      @Override
      void whoIam () {
        super.whoIam();
        System.out.println("I am a Dog!");
      }
    }

In this example, we define `whoIam()` method in `Mammal` class, where the mammal say it is a Mammal. But this is a general term, as there are a lot of Mammals out there. Then we can inherit `Dog` class from `Mammal` class, as Dog is a Mammal. But, to be more specific, Dog is a Dog as well as a Mammal. Hence, Dog should say, `I am a Mammal` and also `I am a Dog`. Hence we can __Override__ the `whoIam()` method in super class (`Mammal` class, that is) from sub class (the `Dog` class).

We can also call the super class's method using `super.whoIam()` in Java. Then, the `Dog` will behave like a Dog, while also behaving like a Mammal.

## Introduction
Polymorphism is one of the basic concepts in __OOP (Object Oriented Programming)__. Main idea of the polymorphism is that an object have the ability to take on different forms. To achieve that (polymorphism), we have two main approaches.
1. Method overloading

    * Occures when there are two or more methods with the same name, with different input parameters. __The return type should be the same for all the methods with the same name__
2. Method overriding
    * Occures when child object uses same method definition (same name with same parameters), but have different implementations.

Using these two approaches we can use the same method/function to behave differently. Let's see more details on this in following examples.

## Method Overloading
Method overloading is the way of using polymorphism inside a class. We can have two or more methods inside the same class, with different input parameters.

Difference of input parameters can be either:
* Number of parameters
* Type of parameters (Data type)
* Order of the parameters

Let's take a look at them separately (These examples in java, as I am more familiar with it - Sorry about that):

1. Number of Parameters

        public class Mathematics { 
            public int add (int a, int b) {
                return (a + b);
            }

            public int add (int a, int b, int c) {
                return (a + b + c); 
            }

            public int add (int a, int b, int c, int c) {
                return (a + b + c + d);
            }
        }

    Look carefully, you can see the method's return type is the same - `int`, but theree of these methods having different number of inputs. This is called as method over loading with different number of parameters.

    __PS:__ This is a just an _example_, there's no need to define add functions like this.


2. Type of parameters

       public class Mathematics { 
           public void display (int a) {
               System.out.println("" + a);
           }

           public void display (double a) {
               System.out.println("" + a);
           }

           public void display (float a) {
               System.out.println("" + a);
           }
       }

    Note that every method has the same name and same return type, while they have different input data types.

    __PS:__ This example is only for explaining purpose only.
 

3. Order of the parameters

       public class Mathematics {
           public void display (int a, double b) {
               System.out.println("Numbers are " + a + " and " + b);
           }

           public void display (double a, int b) {
               System.out.println("Numbers are " + a + " and " + b);
           }
       }

    __PS:__ This example is also for explaining purpose only.

