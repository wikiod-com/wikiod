---
title: "Class"
slug: "class"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Introduction
Class is the piece of code where we define the attributes and/or behaviors of an object. You can define variables, constants, methods and constructors to the object, inside the class. In another words, class is the blueprint of an object.

Let's see a sample class in Java, which defines a (simple) Car:

    public class Car {
      private Engine engine;
      private Body body;
      private Tire [] tire;
      private Interior interior;

      // Constructor 
      public Car (Engine engine, Body body, Tire[] tires, Interior interior) {
        
      }

      // Another constructor
      public Car () {
        
      }

      public void drive(Direction d) {
        // Method to drive
      }

      public void start(Key key) {
        // Start 
      }
    }

This is just for an example. You can model real world object like this, as per your requirement.

