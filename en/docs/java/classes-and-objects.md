---
title: "Classes and Objects"
slug: "classes-and-objects"
draft: false
images: []
weight: 9723
type: docs
toc: true
---

 Objects have states and behaviors. Example: A dog has states - color, name, breed as well as behaviors – wagging the tail, barking, eating. An object is an instance of a class.

Class − A class can be defined as a template/blueprint that describes the behavior/state that the object of its type support.

## Syntax
 - class Example {} //class keyword, name, body

## Overloading Methods
Sometimes the same functionality has to be written for different kinds of inputs. At that time, one can use the same method name with a different set of parameters. Each different set of parameters is known as a method signature. As seen per the example, a single method can have multiple signatures.

    public class Displayer {
    
        public void displayName(String firstName) {
            System.out.println("Name is: " + firstName);
        }
    
        public void displayName(String firstName, String lastName) {
            System.out.println("Name is: " + firstName + " " + lastName);
        }
    
        public static void main(String[] args) {
            Displayer displayer = new Displayer();
            displayer.displayName("Ram");          //prints "Name is: Ram"
            displayer.displayName("Jon", "Skeet"); //prints "Name is: Jon Skeet"
        }
    }

The advantage is that the same functionality is called with two different numbers of inputs. While invoking the method according to the input we are passing, (In this case either one string value or two string values) the corresponding method is executed.

**Methods can be overloaded:**

 1. Based on the **number of parameters** passed.

    Example: `method(String s)` and `method(String s1, String s2)`.


 2. Based on the **order of parameters**.

    Example: `method(int i, float f)` and `method(float f, int i))`.

***Note:** Methods cannot be overloaded by changing *just* the return type (`int method()` is considered the same as `String method()` and will throw a `RuntimeException` if attempted). If you change the return type you must also change the parameters in order to overload.*

## Explaining what is method overloading and overriding.


## Simplest Possible Class
    class TrivialClass {}

A class consists at a minimum of the `class` keyword, a name, and a body, which might be empty.

You instantiate a class with the `new` operator.

    TrivialClass tc = new TrivialClass();

## Object Member vs Static Member
With this class:

    class ObjectMemberVsStaticMember {
    
        static int staticCounter = 0;
        int memberCounter = 0;
    
        void increment() {
            staticCounter ++;
            memberCounter++;
        }
    }

the following code snippet:

    final ObjectMemberVsStaticMember o1 = new ObjectMemberVsStaticMember();
    final ObjectMemberVsStaticMember o2 = new ObjectMemberVsStaticMember();

    o1.increment();

    o2.increment();
    o2.increment();

    System.out.println("o1 static counter " + o1.staticCounter);
    System.out.println("o1 member counter " + o1.memberCounter);
    System.out.println();

    System.out.println("o2 static counter " + o2.staticCounter);
    System.out.println("o2 member counter " + o2.memberCounter);
    System.out.println();

    System.out.println("ObjectMemberVsStaticMember.staticCounter = " + ObjectMemberVsStaticMember.staticCounter);

    // the following line does not compile. You need an object
    // to access its members
    //System.out.println("ObjectMemberVsStaticMember.staticCounter = " + ObjectMemberVsStaticMember.memberCounter);


produces this output:
<!-- language: lang-none -->

    o1 static counter 3
    o1 member counter 1
    
    o2 static counter 3
    o2 member counter 2

    ObjectMemberVsStaticMember.staticCounter = 3

**Note:** You should not call `static` members on objects, but on classes. While it does not make a difference for the JVM, human readers will appreciate it.

`static` members are part of the class and exists only once per class. Non-`static` members exist on instances, there is an independent copy for each instance. This also means that you need access to an object of that class to access its members.

## Basic Object Construction and Use
Objects come in their own class, so a simple example would be a car (detailed explanations below):

    public class Car {
        
        //Variables describing the characteristics of an individual car, varies per  object
       private int milesPerGallon;
       private String name;
       private String color;
       public int numGallonsInTank; 
        
        public Car(){
            milesPerGallon = 0;
            name = "";
            color = "";
            numGallonsInTank = 0;
        }
        
        //this is where an individual object is created
        public Car(int mpg, int, gallonsInTank, String carName, String carColor){
            milesPerGallon = mpg;
            name = carName;
            color = carColor;
            numGallonsInTank = gallonsInTank;
        }

        //methods to make the object more usable

        //Cars need to drive
        public void drive(int distanceInMiles){
            //get miles left in car
            int miles = numGallonsInTank * milesPerGallon;
            
            //check that car has enough gas to drive distanceInMiles
            if (miles <= distanceInMiles){
                numGallonsInTank = numGallonsInTank - (distanceInMiles / milesPerGallon)
                System.out.println("Drove " + numGallonsInTank + " miles!");
            } else {
                System.out.println("Could not drive!");
            }
        }

        public void paintCar(String newColor){
            color = newColor;
        }
            //set new Miles Per Gallon
        public void setMPG(int newMPG){
            milesPerGallon = newMPG;
        }

           //set new number of Gallon In Tank
        public void setGallonsInTank(int numGallons){
            numGallonsInTank = numGallons;
        }
        
        public void nameCar(String newName){
            name = newName;
        }

        //Get the Car color
        public String getColor(){
            return color;
        }

        //Get the Car name
        public String getName(){
            return name;
        }

        //Get the number of Gallons
        public String getGallons(){
            return numGallonsInTank;
        }
        
    }  

Objects are **instances of** their class. So, the way you would **create an object** would be by calling the Car class in **one of two ways** in your main class (main method in Java or onCreate in Android). 

**Option 1**

    `Car newCar = new Car(30, 10, "Ferrari", "Red");

Option 1 is where you essentially tell the program everything about the Car upon creation of the object. Changing any property of the car would require calling one of the methods such as the `repaintCar` method. Example: 

     newCar.repaintCar("Blue");

**Note:** Make sure you pass the correct data type to the method. In the example above, you may also pass a variable to the `repaintCar` method **as long as the data type is correct`**.

That was an example of changing properties of an object, receiving properties of an object would require using a method from the Car class that has a return value (meaning a method that is not `void`).  Example:

    String myCarName = newCar.getName();  //returns string "Ferrari"

Option 1 is the **best** option when you have **all the object's data** at the time of creation.

**Option 2**

    `Car newCar = new Car();

Option 2 gets the same effect but required more work to create an object correctly. I want to recall this Constructor in the Car class:

    public void Car(){
            milesPerGallon = 0;
            name = "";
            color = "";
            numGallonsInTank = 0;
        }
Notice that you do not have to actually pass any parameters into the object to create it. This is very useful for when you do not have all the aspects of the object but you need to use the parts that you do have. This sets generic data into each of the instance variables of the object so that, if you call for a piece of data that does not exist, no errors are thrown. 

**Note:** Do not forget that you have to set the parts of the object later that you did not initialize it with. For example, 

    Car myCar = new Car();
    String color = Car.getColor(); //returns empty string
This is a common mistake amongst objects that are not initialized with all their data. Errors were avoided because there is a Constructor that allows an empty Car object to be created with **stand-in variables** (`public Car(){} `), but no part of the myCar was actually customized. **Correct example of creating Car Object:**

    Car myCar = new Car();
    myCar.nameCar("Ferrari");
    myCar.paintCar("Purple");
    myCar.setGallonsInTank(10);
    myCar.setMPG(30);

And, as a reminder, get an object's properties by calling a method in your main class. Example:

    String myCarName = myCar.getName(); //returns string "Ferrari"

## Constructors
Constructors are special methods named after the class and without a return type, and are used to construct objects. Constructors, like methods, can take input parameters. Constructors are used to initialize objects. Abstract classes can have constructors also.

    public class Hello{
        // constructor
        public Hello(String wordToPrint){
            printHello(wordToPrint);
        }
        public void printHello(String word){
            System.out.println(word);
        }
    }
    // instantiates the object during creating and prints out the content
    // of wordToPrint

It is important to understand that constructors are different from methods in several ways:

1. Constructors can only take the modifiers `public`, `private`, and `protected`, and cannot be declared `abstract`, `final`, `static`, or `synchronized`. 

2. Constructors do not have a return type.

3. Constructors MUST be named the same as the class name. In the `Hello` example, the `Hello` object's constructor name is the same as the class name. 

4. The `this` keyword has an additional usage inside constructors. `this.method(...)` calls a method on the current instance, while `this(...)` refers to another constructor in the current class with different signatures. 


Constructors also can be called through inheritance using the keyword `super`. 

    public class SuperManClass{

        public SuperManClass(){
            // some implementation
        }
        
        // ... methods
    }
    

    public class BatmanClass extends SupermanClass{
        public BatmanClass(){
            super();
        }
        //... methods...
    }

See [Java Language Specification #8.8](http://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html#jls-8.8) and [#15.9](http://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.9)

## Initializing static final fields using a static initializer
To initialize a `static final` fields that require using more than a single expression, a `static` initializer can be used to assign the value. The following example initializes a unmodifiable set of `String`s:

    public class MyClass {
    
        public static final Set<String> WORDS;
        
        static {
            Set<String> set = new HashSet<>();
            set.add("Hello");
            set.add("World");
            set.add("foo");
            set.add("bar");
            set.add("42");
            WORDS = Collections.unmodifiableSet(set);
        }
    }

