---
title: "primitive arrays and object arrays"
slug: "primitive-arrays-and-object-arrays"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

## Java create an array with initial values
    public class CreateArrayWithValues {
        public static void main(String[] args){
            // Initializes an array of Strings with values
            String[] myArray = {"this", "array", "has", "six", "initial", "values"};
            System.out.println("myArray.length = "+myArray.length);
            // Print out each value of myArray
            for(int index = 0; index < myArray.length; index++){
                System.out.print(myArray[index]);
            }
        }
    }

## Create a basic array in Java
    public class CreateAnArray{    
        public static void main(String[] args){
            // Creates a new array of Strings with a length of 3
            // This length cannot be changed later
            String[] myStringArray = new String[3];
            myStringArray[0] = "Hello"; // Java array indicies start at 0
            myStringArray[1] = "World";
            myStringArray[2] = "!"; // The array is now full
            try{
                myStringArray[3] = "This will cause an error."; // Index 3 requires an array 
                // of size 4 or greater
            }
            catch(ArrayIndexOutOfBoundsException out){
                System.out.println("Java arrays cannot be expanded.");
            }
            // Print out "Hello World!" to the console
            System.out.println(myStringArray[0]+" "+myStringArray[1]+" "+myStringArray[2]);
        }
    }

