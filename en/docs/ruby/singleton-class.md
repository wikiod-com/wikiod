---
title: "Singleton Class"
slug: "singleton-class"
draft: false
images: []
weight: 9968
type: docs
toc: true
---

## Syntax
- singleton_class = class << object; self end

Singleton classes only have one instance: their corresponding object. This can be verified by querying Ruby's [`ObjectSpace`][ObjectSpace]:

    instances = ObjectSpace.each_object object.singleton_class

    instances.count            # => 1
    instances.include? object  # => true

Using [`<`][Module#<], they can also be verified to be subclasses of the object's actual class:

    object.singleton_class < object.class  # => true

[ObjectSpace]: http://ruby-doc.org/core/ObjectSpace.html
[Module#<]: http://ruby-doc.org/core/Module.html#method-i-3C

---

References:

- [Three implicit contexts in Ruby][1]

[1]: http://yugui.jp/articles/846

## Inheritance of Singleton Class
## Subclassing also Subclasses Singleton Class

    class Example
    end
    
    Example.singleton_class #=> #<Class:Example>
    
    def Example.foo
      :example
    end
    
    class SubExample < Example
    end

    SubExample.foo #=> :example
    
    SubExample.singleton_class.superclass #=> #<Class:Example>

## Extending or Including a Module does not Extend Singleton Class

    module ExampleModule
    end
    
    def ExampleModule.foo
      :foo
    end
    
    class Example
      extend ExampleModule
      include ExampleModule
    end
    
    Example.foo #=> NoMethodError: undefined method

## Singleton classes
All objects are instances of a class. However, that is not the whole truth. In Ruby, every object also has a somewhat hidden _singleton class_.

This is what allows methods to be defined on individual objects. The singleton class sits between the object itself and its actual class, so all methods defined on it are available for that object, and that object only.

    object = Object.new

    def object.exclusive_method
      'Only this object will respond to this method'
    end

    object.exclusive_method
    # => "Only this object will respond to this method"

    Object.new.exclusive_method rescue $!
    # => #<NoMethodError: undefined method `exclusive_method' for #<Object:0xa17b77c>>

The example above could have been written using [`define_singleton_method `][Object#define_singleton_method]:

    object.define_singleton_method :exclusive_method do
      "The method is actually defined in the object's singleton class"
    end

Which is the same as defining the method on `object`'s [`singleton_class`][Object#singleton_class]:

    # send is used because define_method is private
    object.singleton_class.send :define_method, :exclusive_method do
      "Now we're defining an instance method directly on the singleton class"
    end

Before the existence of `singleton_class` as part of Ruby's core API, singleton classes were known as _metaclasses_ and could be accessed via the following idiom:

    class << object
      self  # refers to object's singleton_class
    end

[Object#define_singleton_method]: http://ruby-doc.org/core/Object.html#method-i-define_singleton_method
[Object#singleton_class]: http://ruby-doc.org/core/Object.html#method-i-singleton_class

## Introduction
Ruby has three types of objects:
* Classes and modules which are instances of class Class or class Module.
* Instances of classes.
* Singleton Classes.

Each object has a class which contains its methods:

    class Example
    end
    
    object = Example.new

    object.class  # => Example
    Example.class # => Class
    Class.class   # => Class
    
Objects themselves can't contain methods, only their class can. But with singleton classes, it is possible to add methods to any object including other singleton classes.

    def object.foo
      :foo
    end
    object.foo #=> :foo

`foo` is defined on singleton class of `object`. Other `Example` instances can not reply to `foo`.

Ruby creates singleton classes on demand. Accessing them or adding methods to them forces Ruby to create them.

## Accessing Singleton Class
There are two ways to get singleton class of an object
* `singleton_class` method.
* Reopening singleton class of an object and returning `self`.


    object.singleton_class

---

    singleton_class = class << object
      self
    end

## Accessing Instance/Class Variables in Singleton Classes
Singleton classes share their instance/class variables with their object.

    class Example
      @@foo = :example
    end
    
    def Example.foo
      class_variable_get :@@foo
    end

    Example.foo #=> :example

---

    class Example
      def initialize
        @foo = 1
      end
    
      def foo
        @foo
      end
    end
    
    e = Example.new

    e.instance_eval <<-BLOCK
      def self.increase_foo
        @foo += 1
      end
    BLOCK

    e.increase_foo
    e.foo #=> 2

---

Blocks close around their instance/class variables target. Accessing instance or class variables using a block in `class_eval` or `instance_eval` isn't possible. Passing a string to `class_eval` or using `class_variable_get` works around the problem.

    class Foo
      @@foo = :foo
    end
    
    class Example
      @@foo = :example 
    
      Foo.define_singleton_method :foo do
        @@foo
      end
    end
    
    Foo.foo #=> :example

## Message Propagation with Singleton Class
Instances never contain a method they only carry data. However we can define a singleton class for any object including an instance of a class.

When a message is passed to an object (method is called) Ruby first checks if a singleton class is defined for that object and if it can reply to that message otherwise Ruby checks instance's class' ancestors chain and walks up on that.

    class Example
      def foo
        :example
      end
    end
    
    Example.new.foo #=> :example
    
    module PrependedModule
      def foo
        :prepend
      end
    end
    
    class Example
      prepend PrependedModule
    end
    
    Example.ancestors #=> [Prepended, Example, Object, Kernel, BasicObject]
    e = Example.new
    e.foo #=> :prepended
    
    def e.foo
      :singleton
    end
    
    e.foo #=> :singleton

## Reopening (monkey patching) Singleton Classes
There are three ways to reopen a Singleton Class
* Using `class_eval` on a singleton class.
* Using `class <<` block.
* Using `def` to define a method on the object's singleton class directly


    class Example
    end
    
    Example.singleton_class.class_eval do
      def foo
        :foo
      end
    end
    
    Example.foo #=> :foo

---

    class Example
    end
    
    class << Example
      def bar
        :bar
      end
    end
    
    Example.bar #=> :bar

---

    class Example
    end
    
    def Example.baz
      :baz
    end

    Example.baz #=> :baz

Every object has a singleton class which you can access

    class Example
    end
    ex1 = Example.new
    def ex1.foobar
      :foobar
    end
    ex1.foobar #=> :foobar

    ex2 = Example.new
    ex2.foobar #=> NoMethodError

