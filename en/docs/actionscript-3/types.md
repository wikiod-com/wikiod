---
title: "Types"
slug: "types"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

## Checking types
You can use the `is` operator to validate whether a value is of a certain type:

    var sprite:Sprite = new Sprite();

    trace(sprite is Sprite); // true
    trace(sprite is DisplayObject); // true, Sprite inherits DisplayObject
    trace(sprite is IBitmapDrawable); // true, DisplayObject implements IBitmapDrawable
    trace(sprite is Number); // false
    trace(sprite is Bitmap); // false, Bitmap inherits DisplayObject
                             // but is not inherited by Sprite.

There is also an `instanceof` operator (deprecated) which works almost identical to `is` except that it *returns `false` when checking for implemented interfaces* and int/uint types.

The `as` operator can also by used just like `is` operator. This is especially usefull if you use some smart IDE like FlashDevelop which will give you a list of all possible properties of explicit object type. Example:

    for (var i:int = 0; i < a.length; i++){
        var d:DisplayObject = a[i] as DisplayObject;
        if (!d) continue;
        d.//get hints here
        stage.addChild(d);
    }

To get the same effect with `is` you would write (sligthly less convenient):

    for (var i:int = 0; i < a.length; i++){
        if (a[i] is DisplayObject != true) continue;
        var d:DisplayObject = a[i] as DisplayObject;
        stage.addChild(d);
    }

Just keep in mind that when checking coditions with `as` operator, given value will be fist converted to specified type and then result of that operation will be checked if not false, so be careful when using it with possible false/NaN values:

    if(false as Boolean) trace("This will not be executed");
    if(false as Boolean != null) trace("But this will be");

Below table shows some basic values and types with result of type operators. Green cells will evaluate to true, red to false and greay will cause compile/runtime errors.

[![enter image description here][1]][1]


  [1]: http://i.stack.imgur.com/k8hrE.png

## Type Casting
Type casting is done with either the `as` operator:

    var chair:Chair = furniture as Chair;

Or by wrapping the value in `Type()`:

    var chair:Chair = Chair(furniture);

If the cast fails with `as`, the result of that cast is `null`. If the cast fails by wrapping in `Type()`, a `TypeError` is thrown.

## The Function type
Functions are of the type `Function`:

    function example():void { }
    trace(example is Function); // true

They can be referenced by other variables with the type `Function`:

    var ref:Function = example;
    ref(); // ref.call(), ref.apply(), etc.

And they can be passed in as arguments for parameters whose type is `Function`:

    function test(callback:Function):void {
        callback();
    }

    test(function() {
        trace('It works!');
    }); // Output: It works!

    

## The Class type
References to class declarations are typed `Class`:

    var spriteClass:Class = Sprite;

You can use variables typed `Class` to instantiate instances of that class:

    var sprite:Sprite = new spriteClass();

This can be useful for passing an argument of type `Class` to a function that might create and instance of the provided class:

    function create(type:Class, x:int, y:int):* {
        var thing:* = new type();
        
        thing.x = x;
        thing.y = y;
        
        return thing;
    }

    var sprite:Sprite = create(Sprite, 100, 100);

## Typed arrays
Unfortunately ActionScript 3 does not have a concept of generics, so it follows that there is no way to define a typed array as `Array<T>`. There is however a special class [`Vector.<T>`](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Vector.html) which works in a similar way except you *must provide a reference to a concrete class* when instantiating the vector. This means there is no way to build abstractions on top of the `Vector.<T>` type (e.g. extend it and add new functionality) which is a huge drawback.

A simpler way to look at is is as though every class you define automatically had a companion class named `Vector.<NameOfYourClass>`.

With that said, there are still huge advantages to the `Vector.<T>` type over a conventional array:

* There is a performance increase when working with `Vector.<T>` vs arrays<sup>1</sup>.
* You receive compile-type `TypeError`s if you attempt to insert non-T values into the collection.
* IDEs provide useful type hinting information for the objects inside a `Vector.<T>` instance.

Examples of creating a `Vector.<T>`:

    var strings:Vector.<String> = new Vector.<String>(); // or
    var numbers:Vector.<Number> = new <Number>[];

---

<sup>1</sup> Vectors actually only provide notable performance improvements over arrays when working with primitive types (`String`, `int`, `uint`, `Number`, etc).

## Annotating types
You can tell the compiler the type of a value by annotating it with `:Type`:

    var value:int = 10; // A property "value" of type "int".

Function parameters and return types can also be annotated:

    // This function accepts two ints and returns an int.
    function sum(a:int, b:int):int {
        return a + b;
    }

Attempting to assign a value with a mismatching type will result in a `TypeError`:

    var sprite:Sprite = 10; // 10 is not a Sprite.

