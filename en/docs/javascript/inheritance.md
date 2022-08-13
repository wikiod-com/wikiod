---
title: "Inheritance"
slug: "inheritance"
draft: false
images: []
weight: 9871
type: docs
toc: true
---

## Standard function prototype


## Difference between Object.key and Object.prototype.key


## Prototypal inheritance
Suppose we have a plain object called `prototype`:

    var prototype = { foo: 'foo', bar: function () { return this.foo; } };

Now we want another object called `obj` that inherits from `prototype`, which is the same as saying that `prototype` is the prototype of `obj`

    var obj = Object.create(prototype);

Now all the properties and methods from `prototype` will be available to `obj`

    console.log(obj.foo);
    console.log(obj.bar());

Console output

    "foo"
    "foo"


----------

Prototypal inheritance is made through object references internally and objects are completely mutable. This means any change you make on a prototype will immediately affect every other object that prototype is prototype of.

    prototype.foo = "bar";
    console.log(obj.foo);

Console output

    "bar"


----------


`Object.prototype` is the prototype of every object, so it's strongly recommended you don't mess with it, specially if you use any third party library, but we can play with it a little bit.

    Object.prototype.breakingLibraries = 'foo';
    console.log(obj.breakingLibraries);
    console.log(prototype.breakingLibraries);

Console output

    "foo"
    "foo"

**Fun fact** I've used the browser console to make these examples and broken this page by adding that `breakingLibraries` property.


----------



## Pseudo-classical inheritance

It's an emulation of classical inheritance using [prototypical inheritance][1] which shows how powerful prototypes are. It was made to make the language more attractive to programmers coming from other languages.

<!-- if version [lt 6] -->

**IMPORTANT NOTE**: Since ES6 it doesn't make sense to use pseudo-calssical inheritance since the language simulates [conventional classes][2]. If you're not using ES6, [you should][3]. If you still want to use the classical inheritance pattern and you're in a ECMAScript 5 or lower environment, then pseudo-classical is your best bet.



<!-- end version if -->



A "class" is just a function that is made to be called with the `new` operand and it's used as a constructor.

    function Foo(id, name) {
        this.id = id;
        this.name = name;
    }

    var foo = new Foo(1, 'foo');
    console.log(foo.id);

Console output

> 1

foo is an instance of Foo.The JavaScript coding convention says if a function begins with a capital letter case it can be called as a constructor (with the `new` operand).


----------

To add properties or methods to the "class" you have to add them to it's prototype, which can be found in the `prototype` property of the constructor.

    Foo.prototype.bar = 'bar';
    console.log(foo.bar);

Console output

> bar

In fact what Foo is doing as a "constructor" is just creating objects with `Foo.prototype` as it's prototype.


----------

You can find a reference to its constructor on every object

    console.log(foo.constructor);

> function Foo(id, name) { ...

    console.log({ }.constructor);

> function Object() { [native code] }


And also check if an object is an instance of a given class with the `instanceof` operator

    console.log(foo instanceof Foo);

> true

    console.log(foo instaceof Object);

> true


  [1]: https://www.wikiod.com/javascript/inheritance#Prototypal inheritance
  [2]: https://www.wikiod.com/javascript/classes
  [3]: http://www.2ality.com/2015/08/getting-started-es6.html

## Setting an Object's prototype


## New object from prototype
In JavaScript, any object can be the prototype of another. When an object is created as a prototype of another, it will inherit all of its parent's properties.

    var proto = { foo: "foo", bar: () => this.foo };

    var obj = Object.create(proto);

    console.log(obj.foo);
    console.log(obj.bar());

Console output:

    > "foo"
    > "foo"
     
**NOTE** `Object.create` is available from ECMAScript 5, but here's a polyfill if you need support for ECMAScript 3

    if (typeof Object.create !== 'function') {
        Object.create = function (o) {
            function F() {}
            F.prototype = o;
            return new F();
        };
    }

> Source: http://javascript.crockford.com/prototypal.html


----------


**Object.create()**

The **Object.create()** method creates a new object with the specified prototype object and properties.

Syntax: `Object.create(proto[, propertiesObject])`

**Parameters**: 

 - **proto** (The object which should be the prototype of the newly-created object.)
 - **propertiesObject** (Optional. If specified and not undefined, an object whose enumerable own properties (that is, those properties defined upon itself and not enumerable properties along its prototype chain) specify property descriptors to be added to the newly-created object, with the corresponding property names. These properties correspond to the second argument of Object.defineProperties().)

**Return value**

A new object with the specified prototype object and properties.

**Exceptions**

A *TypeError* exception if the proto parameter isn't *null* or an object.

