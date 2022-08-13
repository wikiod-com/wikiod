---
title: "Object Oriented Programming"
slug: "object-oriented-programming"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Method overriding
When you `extend` a class, you can `override` methods that the inherited class defines using the `override` keyword:

    public class Example {
        public function test():void {
            trace('It works!');
        }
    }

    public class AnotherExample extends Example {
        public override function test():void {
            trace('It still works!');
        }
    }

Example:

    var example:Example = new Example();
    var another:AnotherExample = new AnotherExample();

    example.test(); // Output: It works!
    another.test(); // Output: It still works!

You can use the `super` keyword to reference the original method from the class being inherited. For example, we could change the body of `AnotherExample.test()` to:

    public override function test():void {
        super.test();
        trace('Extra content.');
    }

Resulting in:

    another.test(); // Output: It works!
                    //         Extra content.

Overriding class constructors is a little bit different. The `override` keyword is omitted and accessing the inherited constructor is done simply with `super()`:

    public class AnotherClass extends Example {
        public function AnotherClass() {
            super(); // Call the constructor in the inherited class.
        }
    }

You can also override `get` and `set` methods.

## "Overloaded" Constructor via static method
Constructor overloading is not available in As3.

In order to provide a different way to retrieve an instance of a class, a `public static` method can be provided to serve as an alternative "constructor".

An example for that is `flash.geom.Point`, which represents a 2D point object.
The coordinates to define the point can be

  - **cartesian** in the regular constructor

        public function Point(x:Number = 0, y:Number = 0) 

    example usage:
        
        var point:Point = new Point(2, -.5);

  - **polar** in a static method

        public static function polar(len:Number, angle:Number):Point

    example usage:
        
        var point:Point = Point.polar(12, .7 * Math.PI);

    Because it is not an actual constructor, there's no `new` keyword.

## Packages
Packages are bundles of classes. Every class must be declared within a package using the `package` statement. The `package` statement is followed by the name of your package, or followed by nothing in the case of adding classes to the top-level package. Sub-packages are created using dot (`.`) delimitation. The package statement is followed by a block which will contain *a single `class` definition*. Examples:

    package {
        // The top level package.
    }

    package world {
        // A package named world.
    }

    package world.monsters {
        // A package named monsters within a package named world.
    }

Packages should correlate to the file structure of the classes relative to the source root. Assuming you have a source root folder named `src`, the above could be correctly represented in the filesystem as:

    src
        TopLevelClass.as
        
        world
            ClassInWorldPackage.as
            AnotherClassInWorldPackage.as
            
            monsters
                Zombie.as

## getter and setter
Getters and setters are methods that are behaved like properties. it means they have function structure but when used, they are used same as properties:

**Structure of getter functions**:  

they should have `get` keyword after `function` keyword and before function name, with no argument, a return type specified and must return a value:

    public function get myValue():Type{
        //anything here
        return _desiredValue;
    }
**Syntax**:

to get the value from a getter,the syntax is the same as getting a value from a property(no parens `()` are used).

    trace(myValue);

**Structure of setter functions**:  

they should have `set` keyword after `function` keyword and before function name, with one argument, and no value return.

    public function set myValue(value:Type):void{
        //anything here
        _desiredProperty=value;
    }

**Syntax**:

to set the value of a setter,the syntax is the same as setting a value to a property(using equal sign `=` then value).  

    myValue=desiredValue;

**setting a getter and setter for one value**:
>Note: if you create only getter or only setter with a name, that property would be read-only or set-only.  

to make a property both readable and setable, should create a getter and a setter with:  

1.the same name.  
2.the same type(type of return value for the getter and type of input value(argument) for the setter,
>Note: getters and setters should not have a name same as other properties or methods  .

**Usage of getters and setters:**

Using getters and setters rather than normal properties has many pros:

1.**making read-only or set-only properties:**  
  for example number of children in a display object. it can't be setable.

2.**accessing private properties:**  
an example:  

    private var _private:Type=new Type();
    //note that function name "private" is not same as variable name "_private"  
    public function get private():Type{
        return _private;
    }
3.**when some change is required after setting a value:**  
in this example, changing this property must be notified:  

    public static function set val:(input:Type):void{
        _desiredProperty=input;
        notifyValueChanged();
    }
and many other usages
    


## set & get functions
To ensure encapsulation, member variables of a class should be `private` and only be accessible to `public` via public `get`/`set` access methods. It is a common practice to prefix private fields with `_`

    public class Person
    {
        private var _name:String = "";
    
        public function get name():String{
            return _name;
            //or return some other value depending on the inner logic of the class
        }


        public function set name(value:String):void{
            //here you may check if the new value is valid
            //or maybe dispatch some update events or whatever else
            _name = value;
        }

Sometimes you don't even need to create a `private` field for a `get`/`set` pair.<br>
For example in a control like a custom radio group you need to know which radio button is selected, however outside the class you need just a way to `get`/`set` only the selected value:

    public function get selectedValue():String {
        //just the data from the element
        return _selected ? _selected.data : null;
    }
    public function set selectedValue(value:String):void {
        //find the element with that data
        for (var i:int = 0; i < _elems.length; i++) {
            if (_elems[i].data == value) {
                _selected = _elems[i];//set it 
                processRadio();//redraw
                return;
            }
        }
    }

