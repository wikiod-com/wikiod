---
title: "Object-oriented Perl"
slug: "object-oriented-perl"
draft: false
images: []
weight: 9921
type: docs
toc: true
---

## Defining classes in modern Perl
Although available, defining a class [from scratch](https://www.wikiod.com/perl/object-oriented-perl#Defining Classes) is not recommended in modern Perl. Use one of helper OO systems which provide more features and convenience. Among these systems are:

* [`Moose`](http://search.cpan.org/perldoc?Moose) - inspired by Perl 6 OO design

* [`Class::Accessor`](http://search.cpan.org/perldoc?Class%3A%3AAccessor) - a lightweight alternative to Moose

* [`Class::Tiny`](http://search.cpan.org/perldoc?Class%3A%3ATiny) - truly minimal class builder

**Moose**

    package Foo;
    use Moose;

    has bar => (is => 'ro');                 # a read-only property
    has baz => (is => 'rw', isa => 'Bool');  # a read-write boolean property

    sub qux {
        my $self = shift;
        my $barIsBaz = $self->bar eq 'baz';  # property getter
        $self->baz($barIsBaz);               # property setter
    }

**Class::Accessor (Moose syntax)**

    package Foo;
    use Class::Accessor 'antlers';
    
    has bar => (is => 'ro');                 # a read-only property
    has baz => (is => 'rw', isa => 'Bool');  # a read-write property (only 'is' supported, the type is ignored)

**Class::Accessor (native syntax)**

    package Foo;
    use base qw(Class::Accessor);
    
    Foo->mk_accessors(qw(bar baz));  # some read-write properties
    Foo->mk_accessors(qw(qux));      # a read-only property

**Class::Tiny**

    package Foo;
    use Class::Tiny qw(bar baz);  # just props



## Creating Objects
Unlike many other languages, Perl does not have constructors that allocate memory for your objects. Instead, one should write a class method that both create a data structure and populate it with data (you may know it as the Factory Method design pattern).

    package Point;
    use strict;

    sub new {
        my ($class, $x, $y) = @_;
        my $self = { x => $x, y => $y }; # store object data in a hash
        bless $self, $class;             # bind the hash to the class
        return $self;
    }

This method can be used as follows:

    my $point = Point->new(1, 2.5);

Whenever the arrow operator `->` is used with methods, its left operand is prepended to the given argument list. So, `@_` in `new` will contain values `('Point', 1, 2.5)`.

There is nothing special in the name `new`. You can call the factory methods as you prefer.

There is nothing special in hashes. You could do the same in the following way:

    package Point;
    use strict;

    sub new {
        my ($class, @coord) = @_;
        my $self = \@coord;
        bless $self, $class;
        return $self;
    }

In general, any reference may be an object, even a scalar reference. But most often, hashes are the most convenient way to represent object data.

## Defining Classes
In general, classes in Perl are just packages. They can contain data and methods, as usual packages.

    package Point;
    use strict;

    my $CANVAS_SIZE = [1000, 1000];

    sub new {
        ...
    }

    sub polar_coordinates {
        ...
    }

    1;

It is important to note that the variables declared in a package are class variables, not object (instance) variables. Changing of a package-level variable affects all objects of the class. How to store object-specific data, see in "Creating Objects".

What makes class packages specific, is the arrow operator `->`. It may be used after a bare word:

    Point->new(...);

or after a scalar variable (usually holding a reference):

    my @polar = $point->polar_coordinates;

What is to the left of the arrow is prepended to the given argument list of the method. For example, after call

    Point->new(1, 2);

array `@_` in `new` will contain three arguments: `('Point', 1, 2)`.

Packages representing classes should take this convention into account and expect that all their methods will have one extra argument.

## Inheritance and methods resolution
To make a class a subclass of another class, use `parent` pragma:

    package Point;
    use strict;
    ...
    1;

    package Point2D;
    use strict;
    use parent qw(Point);
    ...
    1;

    package Point3D;
    use strict;
    use parent qw(Point);
    ...
    1;

Perl allows for multiple inheritance:

    package Point2D;
    use strict;
    use parent qw(Point PlanarObject);
    ...
    1;

Inheritance is all about resolution which method is to be called in a particular situation. Since pure Perl does not prescribe any rules about the data structure used to store object data, inheritance has nothing to do with that.

Consider the following class hierarchy:

    package GeometryObject;
    use strict;
    
    sub transpose { ...}

    1;

    package Point;
    use strict;
    use parent qw(GeometryObject);

    sub new { ... };

    1;

    package PlanarObject;
    use strict;
    use parent qw(GeometryObject);

    sub transpose { ... }

    1;

    package Point2D;
    use strict;
    use parent qw(Point PlanarObject);
    
    sub new { ... }

    sub polar_coordinates { ... }

    1;

The method resolution works as follows:

1. The starting point is defined by the left operand of the arrow operator.

   * If it is a bare word:

         Point2D->new(...);

     ...or a scalar variable holding a string:

         my $class = 'Point2D';
         $class->new(...);

     ...then the starting point is the package with the corresponding name (`Point2D` in both examples).

   * If the left operand is a scalar variable holding a *blessed* reference:

         my $point = {...};
         bless $point, 'Point2D'; # typically, it is encapsulated into class methods
         my @coord = $point->polar_coordinates;

     then the starting point is the class of the reference (again, `Point2D`). The arrow operator cannot be used to call methods for *unblessed* references.

2. If the starting point contains the required method, it is simply called.

   Thus, since `Point2D::new` exists,

       Point2D->new(...);

   will simply call it.

3. If the starting point does not contain the required method, depth-first search in the `parent` classes is performed. In the example above, the search order will be as follows:

   * `Point2D`
   * `Point` (first parent of `Point2D`)
   * `GeometryObject` (parent of `Point`)
   * `PlanarObject` (second parent of `Point2D`)

   For example, in the following code:

       my $point = Point2D->new(...);
       $point->transpose(...);

   the method that will be called is `GeometryObject::transpose`, even though it would be overridden in `PlanarObject::transpose`.

4. You can set the starting point explicitly.

   In the previous example, you can explicitly call `PlanarObject::transpose` like so:

       my $point = Point2D->new(...);
       $point->PlanarObject::transpose(...);

5. In a similar manner, `SUPER::` performs method search in parent classes of the current class.

   For example,

       package Point2D;
       use strict;
       use parent qw(Point PlanarObject);
    
       sub new {
           (my $class, $x, $y) = @_;
           my $self = $class->SUPER::new;
           ...
       }
    
       1;

   will call `Point::new` in the course of the `Point2D::new` execution.

## Class and Object Methods
In Perl, the difference between class (static) and object (instance) methods is not so strong as in some other languages, but it still exists.

The left operand of the arrow operator `->` becomes the first argument of the method to be called. It may be either a string:

    # the first argument of new is string 'Point' in both cases
    Point->new(...);

    my $class = 'Point';
    $class->new(...);

or an object reference:

    # reference contained in $point is the first argument of polar_coordinates
    my $point = Point->new(...);
    my @coord = $point->polar_coordinates;

Class methods are just the ones that expect their first argument to be a string, and object methods are the ones that expect their first argument to be an object reference.

Class methods typically do not do anything with their first argument, which is just a name of the class. Generally, it is only used by Perl itself for method resolution. Therefore, a typical class method can be called for an object as well:

    my $width = Point->canvas_width;

    my $point = Point->new(...);
    my $width = $point->canvas_width;

Although this syntax is allowed, it is often misleading, so it is better to avoid it.

Object methods receive an object reference as the first argument, so they can address the object data (unlike class methods):

    package Point;
    use strict;

    sub polar_coordinates {
        my ($point) = @_;
        my $x = $point->{x};
        my $y = $point->{y};
        return (sqrt($x * $x + $y * $y), atan2($y, $x));
    }

    1;

The same method can track both cases: when it is called as a class or an object method:

    sub universal_method {
        my $self = shift;
        if (ref $self) {
            # object logic
            ...
        }
        else {
            # class logic
            ...
        }
    }


## Roles
A role in Perl is essentially 
 - a set of methods and attributes which 
 - injected into a class directly.

A role provides a piece of functionality which can be _composed_ into (or _applied_ to) any class (which is said to _consume_ the role).
A role cannot be inherited but may be consumed by another role.

A role may also _require_ consuming classes to implement some methods instead of implementing the methods itself (just like interfaces in Java or C#).

Perl does not have built-in support for roles but there are CPAN classes which provide such support.

**[Moose::Role](https://metacpan.org/pod/Moose::Role)**

    package Chatty;
    use Moose::Role;

    requires 'introduce';  # a method consuming classes must implement

    sub greet {            # a method already implemented in the role
        print "Hi!\n";
    }
    

    package Parrot;
    use Moose;

    with 'Chatty';

    sub introduce {
        print "I'm Buddy.\n";
    }


**[Role::Tiny](https://metacpan.org/pod/Role::Tiny)**

Use if your OO system does not provide support for roles (e.g. `Class::Accessor` or `Class::Tiny`).
Does not support attributes.

    package Chatty;
    use Role::Tiny;
    
    requires 'introduce';  # a method consuming classes must implement

    sub greet {            # a method already implemented in the role
        print "Hi!\n";
    }

    package Parrot;
    use Class::Tiny;
    use Role::Tiny::With;

    with 'Chatty';

    sub introduce {
        print "I'm Buddy.\n";
    }


