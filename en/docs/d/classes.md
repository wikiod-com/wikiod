---
title: "Classes"
slug: "classes"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Syntax
- class Foo { } // inherits from Object
- class Bar: Foo { } // Bar is a Foo too
- Foo f = new Foo(); // instantiate new objects on the heap

See the [specification](https://dlang.org/spec/class.html), browse a book chapter on [classes](http://ddili.org/ders/d.en/class.html), [inheritance](http://ddili.org/ders/d.en/inheritance.html) and
[play interactively](http://tour.dlang.io/tour/en/basics/classes).

## Inheritance
<!-- language: lang-d -->

    class Animal
    {
        abstract int maxSize(); // must be implemented by sub-class
        final float maxSizeInMeters() // can't be overridden by base class
        {
            return maxSize() / 100.0;
        }
    }

    class Lion: Animal
    {
        override int maxSize() { return 350; }
    }

    void main()
    {
        import std.stdio : writeln;
        auto l = new Lion();
        assert(l.maxSizeInMeters() == 3.5);

        writeln(l.maxSizeInMeters()); // 3.5
    }

## Instantiation
<!-- language: lang-d -->

    class Lion
    {
        private double weight; // only accessible with-in class

        this(double weight)
        {
            this.weight = weight;
        }

        double weightInPounds() const @property // const guarantees no modifications
        // @property functions are treated as fields
        {
            return weight * 2.204;
        }
    }

    void main()
    {
        import std.stdio : writeln;
        auto l = new Lion(100);
        assert(l.weightInPounds == 220.4);

        writeln(l.weightInPounds); // 220.4
    }

