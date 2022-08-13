---
title: "Constructor functions"
slug: "constructor-functions"
draft: false
images: []
weight: 9953
type: docs
toc: true
---

Constructor functions are actually just regular functions, there's nothing special about them. It's only the `new` keyword which causes the special behavior shown in the examples above. Constructor functions can still be called like a regular function if desired, in which case you would need to bind the `this` value explicitly.

## Declaring a constructor function
Constructor functions are functions designed to construct a new object. Within a constructor function, the keyword `this` refers to a newly created object which values can be assigned to. Constructor functions "return" this new object automatically.

    function Cat(name) {
      this.name = name;
      this.sound = "Meow";
    }

Constructor functions are invoked using the `new` keyword:

    let cat = new Cat("Tom");
    cat.sound; // Returns "Meow"

Constructor functions also have a `prototype` property which points to an object whose properties are automatically inherited by all objects created with that constructor:

    Cat.prototype.speak = function() {
      console.log(this.sound);
    }
    
    cat.speak(); // Outputs "Meow" to the console

Objects created by constructor functions also have a special property on their prototype called `constructor`, which points to the function used to create them:

    cat.constructor // Returns the `Cat` function

Objects created by constructor functions are also considered to be "instances" of the constructor function by the `instanceof` operator:

    cat instanceof Cat // Returns "true"

