---
title: "Modules"
slug: "modules"
draft: false
images: []
weight: 9951
type: docs
toc: true
---

## Syntax
- Declaration 

      module Name;

          any ruby expressions;

      end

Module names in Ruby are constants, so they have to start with a capital letter.

    module foo; end # Syntax error: class/module name must be CONSTANT

## A simple mixin with include
    module SomeMixin
      def foo
        puts "foo!"
      end
    end

    class Bar
      include SomeMixin
      def baz
        puts "baz!"
      end
    end

    b = Bar.new
    b.baz         # => "baz!"
    b.foo         # => "foo!"
    # works thanks to the mixin

Now `Bar` is a mix of its own methods and the methods from `SomeMixin`.

Note that how a mixin is used in a class depends on how it is added: 
- the `include` keyword evaluates the module code in the class context (eg. method definitions will be methods on instances of the class),
- `extend` will evaluate the module code in the context of the singleton class of the object (methods are available directly on the extended object).

## Modules and Class Composition
You can use Modules to build more complex classes through *composition*.  The `include ModuleName` directive incorporates a module's methods into a class.

    module Foo
      def foo_method
        puts 'foo_method called!'
      end
    end
    
    module Bar
      def bar_method
        puts 'bar_method called!'
      end
    end
    
    class Baz
      include Foo
      include Bar
    
      def baz_method
        puts 'baz_method called!'
      end  
    end

`Baz` now contains methods from both `Foo` and `Bar` in addition to its own methods.
    
    new_baz = Baz.new
    new_baz.baz_method #=> 'baz_method called!'
    new_baz.bar_method #=> 'bar_method called!'
    new_baz.foo_method #=> 'foo_method called!'



## Module as Namespace
Modules can contain other modules and classes:

    module Namespace

        module Child
    
            class Foo; end
    
        end # module Child

        # Foo can now be accessed as:
        #
        Child::Foo

    end # module Namespace

    # Foo must now be accessed as:
    # 
    Namespace::Child::Foo

## A simple mixin with extend


