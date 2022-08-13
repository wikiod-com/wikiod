---
title: "Inheritance"
slug: "inheritance"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Note: Multi-level inheritance is allowed in Java but not multiple inheritance. Find out more at http://beginnersbook.com/2013/04/oops-concepts/

## Inheritance - Definition
Inheritance is one of the main concepts in __Object Oriented Programming (OOP)__. Using inheritance, we can model a problem properly and we can reduce the number of lines we have to write. Let's see inheritance using a popular example.

Consider you have to model animal kingdom (Simplified animal kingdom, of course. Biologists, pardon me) using OOP. There are a lots of species of animals, some have unique features, while some share same features.

There a are main families of animals. Let's say, `Mammals`, `Reptiles`.

Then we have children of those families. For an example,
* `Cat`, `Dog`, and `Lion` are mammals.
* `Cobra` and `Python` are reptiles.

Every animal shares some basic features like `eat`, `drink`, `move`. Hence we can say that we can have a parent called `Animal` from which they can inherit those basic features.

Then those families also shares some features. For an example reptiles use _crawling_ to move. Every mammal is `fed milk` at early stages of life.

Then there are some unique features for each and every animal.

Consider if we are to create these animal species separately. We have to write same code again and again in every animal species. Instead of that, we use inheritance. We can model the Animal Kingdom as follows:

* We can have parent Object called `Animal`, which have basic features of all the animals.
* `Mammal` and `Reptile` (of course the other animal families also) objects with their common features while inheriting the basic features from parent object, `Animal`.
* Animal species objects: `Cat` and `Dog` inherits from `Mammal`object, `Cobra` and `Python` inherits from `Reptile` object, and so on.

In this form we can reduce the code we write, as we do not need to define basic features of Animals in each animal species, as we can define them in the `Animal` object and then inherit them. Same thing goes with the animal families.

## Inheritance Example - Consider below two classes
Teacher Class:    

    class Teacher {
           private String name;
           private double salary;
           private String subject;
           public Teacher (String tname)  {
               name = tname;
           }
           public String getName()  {
               return name;
           }
           private double getSalary()  {
               return salary;
           }
           private String  getSubject()  {
                return  subject;
           }
        }

OfficeStaff Class:

    class  OfficeStaff{
       private String name;
       private double salary;
       private String dept;
       public OfficeStaff (String sname)  {
          name = sname;
       }
       public String getName()  {
           return name;
       }
       private double  getSalary()  {
           return salary;
       }
       private String  getDept ()  {
           return dept;
       }
    }

1) Both the classes share few common properties and methods. Thus repetition of code.
2) Creating a class which contains the common methods and properties.
3) The classes Teacher and OfficeStaff can inherit the all the common properties and methods from below Employee class.

Employee Class:

    class Employee{
       private String name;
       private double salary;
       public Employee(String ename){
          name=ename;
       }
       public String getName(){
          return name;
       }
       private double getSalary(){
          return salary;
       } 
    }

4) Add individual methods and properties to it Once we have created a super class that defines the attributes common to a set of objects, it can be used to create any number of more specific subclasses
5) Any similar classes like Engineer, Principal can be generated as subclasses from the Employee class.
6) The parent class is termed super class and the inherited class is the sub class
7) A sub class is the specialized version of a super class – It inherits all of the instance variables and methods defined by the super class and adds its own, unique elements.
8) Although a sub class includes all of the members of its super class it can not access those members of the super class that have been declared as private.
9) A reference variable of a super class can be assigned to a reference to any sub class derived from that super class
i.e. Employee emp = new Teacher();

