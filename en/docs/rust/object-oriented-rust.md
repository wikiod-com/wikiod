---
title: "Object-oriented Rust"
slug: "object-oriented-rust"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

Rust is object oriented in that its algebraic data types can have associated methods, making them objects in the sense of data stored along with code that knows how to work with it. 

Rust does not, however, support inheritance, favoring composition with Traits. This means many OO patterns don't work as-is and must be modified. Some are totally irrelevant.

## Inheritance with Traits
In Rust, there is no concept of "inheriting" the properties of a struct. Instead, when you are designing the relationship between objects do it in a way that one's functionality is defined by an interface (a **trait** in Rust). This promotes [composition over inheritance](https://en.wikipedia.org/wiki/Composition_over_inheritance), which is considered more useful and easier to extend to larger projects.

Here's an example using some example inheritance in Python:

    class Animal:
        def speak(self):
            print("The " + self.animal_type + " said " + self.noise)

    class Dog(Animal):
        def __init__(self):
            self.animal_type = 'dog'
            self.noise = 'woof'

In order to translate this in to Rust, we need to take out what makes up an animal and put that functionality into traits.

    trait Speaks {
         fn speak(&self);
    
         fn noise(&self) -> &str;
    }
    
    trait Animal {
        fn animal_type(&self) -> &str;
    }
    
    struct Dog {}
    
    impl Animal for Dog {
        fn animal_type(&self) -> &str {
            "dog"
        }
    }  
    
    impl Speaks for Dog {
        fn speak(&self) {
            println!("The dog said {}", self.noise());
        }
    
        fn noise(&self) -> &str {
            "woof"
        }
    }
    
    fn main() {
        let dog = Dog {};
        dog.speak();
    }

Note how we broke that abstract parent class into two separate components: the part that defines the struct as an animal, and the part that allows it to speak.

Astute readers will notice this isn't quite one to one, as every implementer has to reimplement the logic to print out a string in the form "The {animal} said {noise}". You can do this with a slight redesign of the interface where we implement `Speak` for `Animal`:


    trait Speaks {
         fn speak(&self);
    }
    
    trait Animal {
        fn animal_type(&self) -> &str;
        fn noise(&self) -> &str;
    }
    
    impl<T> Speaks for T where T: Animal {
        fn speak(&self) {
            println!("The {} said {}", self.animal_type(), self.noise());
        }
    }
    
    struct Dog {}
    struct Cat {}
    
    impl Animal for Dog {
        fn animal_type(&self) -> &str {
            "dog"
        }
        
        fn noise(&self) -> &str {
            "woof"
        }
    }

    impl Animal for Cat {
        fn animal_type(&self) -> &str {
            "cat"
        }

        fn noise(&self) -> &str {
            "meow"
        }
    }
    
    fn main() {
        let dog = Dog {};
        let cat = Cat {};
        dog.speak();
        cat.speak();
    }

Notice now the animal makes a noise and speaks simply now has a implementation for anything that is an animal. This is much more flexible than both the previous way and the Python inheritance. For example, if you want to add a `Human` that has a different sound, we can instead just have another implementation of `speak` for something `Human`:

    trait Human {
        fn name(&self) -> &str;
        fn sentence(&self) -> &str;
    }

    struct Person {}

    impl<T> Speaks for T where T: Human {
        fn speak(&self) {
            println!("{} said {}", self.name(), self.sentence());
        }
    }

## Visitor Pattern
The typical Visitor example in Java would be:

    interface ShapeVisitor {
        void visit(Circle c);
        void visit(Rectangle r);
    }

    interface Shape {
        void accept(ShapeVisitor sv);
    }

    class Circle implements Shape {
        private Point center;
        private double radius;

        public Circle(Point center, double radius) {
            this.center = center;
            this.radius = radius;
        }

        public Point getCenter() { return center; }
        public double getRadius() { return radius; }

        @Override
        public void accept(ShapeVisitor sv) {
            sv.visit(this);
        }
    }

    class Rectangle implements Shape {
        private Point lowerLeftCorner;
        private Point upperRightCorner;

        public Rectangle(Point lowerLeftCorner, Point upperRightCorner) {
            this.lowerLeftCorner = lowerLeftCorner;
            this.upperRightCorner = upperRightCorner;
        }

        public double length() { ... }
        public double width() { ... }

        @Override
        public void accept(ShapeVisitor sv) {
            sv.visit(this);
        }
    }

    class AreaCalculator implements ShapeVisitor {
        private double area = 0.0;

        public double getArea() { return area; }

        public void visit(Circle c) {
            area = Math.PI * c.radius() * c.radius();
        }

        public void visit(Rectangle r) {
             area = r.length() * r.width();
        }
    }

    double computeArea(Shape s) {
        AreaCalculator ac = new AreaCalculator();
        s.accept(ac);
        return ac.getArea();
    }

This can be easily translated to Rust, in two ways.

The first way uses run-time polymorphism:

    trait ShapeVisitor {
        fn visit_circle(&mut self, c: &Circle);
        fn visit_rectangle(&mut self, r: &Rectangle);
    }

    trait Shape {
        fn accept(&self, sv: &mut ShapeVisitor);
    }

    struct Circle {
        center: Point,
        radius: f64,
    }

    struct Rectangle {
        lowerLeftCorner: Point,
        upperRightCorner: Point,
    }

    impl Shape for Circle {
        fn accept(&self, sv: &mut ShapeVisitor) {
            sv.visit_circle(self);
        }
    }
    
    impl Rectangle {
        fn length() -> double { ... }
        fn width() -> double { ... }
    }

    impl Shape for Rectangle {
        fn accept(&self, sv: &mut ShapeVisitor) {
            sv.visit_rectangle(self);
        }
    }

    fn computeArea(s: &Shape) -> f64 {
        struct AreaCalculator {
            area: f64,
        }

        impl ShapeVisitor for AreaCalculator {
            fn visit_circle(&mut self, c: &Circle) {
                self.area = std::f64::consts::PI * c.radius * c.radius;
            }
            fn visit_rectangle(&mut self, r: &Rectangle) {
                self.area = r.length() * r.width();
            }
        }
        
        let mut ac = AreaCalculator { area: 0.0 };
        s.accept(&mut ac);
        ac.area
    }

The second way uses compile-time polymorphism instead, only the differences are shown here:

    trait Shape {
        fn accept<V: ShapeVisitor>(&self, sv: &mut V);
    }

    impl Shape for Circle {
        fn accept<V: ShapeVisitor>(&self, sv: &mut V) {
            // same body
        }
    }
    
    impl Shape for Rectangle {
        fn accept<V: ShapeVisitor>(&self, sv: &mut V) {
            // same body
        }
    }

    fn computeArea<S: Shape>(s: &S) -> f64 {
        // same body
    }



