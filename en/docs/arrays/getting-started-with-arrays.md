---
title: "Getting started with arrays"
slug: "getting-started-with-arrays"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Basic array in Java
In Java, any object or primitive type can be an array. Array indicies are accessed via arrayName[index], e.g. `myArray[0]`. Values in an array are set via myArray[0] = value, e.g. if myArray is an array of type String[] `myArray[0] = "test";`

    public class CreateBasicArray{
        public static void main(String[] args){

            // Creates a new array of Strings, with a length of 1
            String[] myStringArray = new String[1]; 
            // Sets the value at the first index of myStringArray to "Hello World!"
            myStringArray[0] = "Hello World!";
            // Prints out the value at the first index of myStringArray,
            // in this case "Hello World!"
            System.out.println(myStringArray[0]);
                
            // Creates a new array of ints, with a length of 1
            int[] myIntArray = new int[1];
            // Sets the value at the first index of myIntArray to 1
            myIntArray[0] = 1;       
            // Prints out the value at the first index of myIntArray,
            // in this case 1
            System.out.println(myIntArray[0]);     
            
            // Creates a new array of Objects with a length of 1
            Object[] myObjectArray = new Object[1];
            // Constructs a new Java Object, and sets the value at the first     
            // index of myObjectArray to the new Object.
            myObjectArray[0] = new Object();
        }       
    }

## Availability
Arrays are available in most programming languages, often using square `[]` or round `()` brackets to access the elements, e.g. `Carray[6]` or `VBarray(6)`.

