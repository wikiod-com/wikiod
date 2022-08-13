---
title: "Visitor Pattern"
slug: "visitor-pattern"
draft: false
images: []
weight: 9948
type: docs
toc: true
---

## Visitor pattern example in java
`Visitor` pattern allows you to add new operations or methods to a set of classes without modifying the structure of those classes.

This pattern is especially useful when you want to centralise a particular operation on an object without extending the object Or without modifying the object.

UML diagram from wikipedia:

[![enter image description here][1]][1]

Code snippet:

    import java.util.HashMap;
    
    interface Visitable{
        void accept(Visitor visitor);
    }
    
    interface Visitor{
        void logGameStatistics(Chess chess);
        void logGameStatistics(Checkers checkers);
        void logGameStatistics(Ludo ludo);    
    }
    class GameVisitor implements Visitor{
        public void logGameStatistics(Chess chess){
            System.out.println("Logging Chess statistics: Game Completion duration, number of moves etc..");    
        }
        public void logGameStatistics(Checkers checkers){
            System.out.println("Logging Checkers statistics: Game Completion duration, remaining coins of loser");    
        }
        public void logGameStatistics(Ludo ludo){
            System.out.println("Logging Ludo statistics: Game Completion duration, remaining coins of loser");    
        }
    }
    
    abstract class Game{
        // Add game related attributes and methods here
        public Game(){
        
        }
        public void getNextMove(){};
        public void makeNextMove(){}
        public abstract String getName();
    }
    class Chess extends Game implements Visitable{
        public String getName(){
            return Chess.class.getName();
        }
        public void accept(Visitor visitor){
            visitor.logGameStatistics(this);
        }
    }
    class Checkers extends Game implements Visitable{
        public String getName(){
            return Checkers.class.getName();
        }
        public void accept(Visitor visitor){
            visitor.logGameStatistics(this);
        }
    }
    class Ludo extends Game implements Visitable{
        public String getName(){
            return Ludo.class.getName();
        }
        public void accept(Visitor visitor){
            visitor.logGameStatistics(this);
        }
    }
    
    public class VisitorPattern{
        public static void main(String args[]){
            Visitor visitor = new GameVisitor();
            Visitable games[] = { new Chess(),new Checkers(), new Ludo()};
            for (Visitable v : games){
                v.accept(visitor);
            }
        }
    }


Explanation:

1. `Visitable` (`Element`) is an interface and this interface method has to be added to a set of classes. 
2. `Visitor` is an interface, which contains methods to perform an operation on `Visitable` elements.
3. `GameVisitor` is a class, which implements `Visitor` interface ( `ConcreteVisitor`).
4. Each `Visitable` element accept `Visitor` and  invoke a relevant method of `Visitor` interface.
5. You can treat `Game` as `Element` and concrete games like `Chess,Checkers and Ludo` as `ConcreteElements`.

In above example, `Chess, Checkers and Ludo` are three different games ( and `Visitable` classes). On one fine day, I have encountered with a scenario to log statistics of each game. So without modifying individual class to implement statistics functionality, you can centralise that responsibility in `GameVisitor` class, which does the trick for you without modifying the structure of each game.

output:

    Logging Chess statistics: Game Completion duration, number of moves etc..
    Logging Checkers statistics: Game Completion duration, remaining coins of loser
    Logging Ludo statistics: Game Completion duration, remaining coins of loser


Use cases/Applicability:

1. *Similar operations have to be performed* on objects of different types grouped in a structure 
2. You need to execute many distinct and unrelated operations. *It separates Operation from   objects Structure*
3. New operations have to be added *without change in object structure*
4. *Gather related operations into a single class* rather than force you to change or derive classes
5. Add functions to class libraries for which you *either do not have the source or cannot change the source*

Additional references:

[oodesign][2]

[sourcemaking][3]


  [1]: http://i.stack.imgur.com/FHLxn.png
  [2]: http://www.oodesign.com/visitor-pattern.html
  [3]: https://sourcemaking.com/design_patterns/visitor

## Visitor Pattern example in C++
Instead of

    struct IShape
    {
        virtual ~IShape() = default;

        virtual void print() const = 0;
        virtual double area() const = 0;
        virtual double perimeter() const = 0;
        // .. and so on
    };

Visitors can be used:

    // The concrete shapes
    struct Square;
    struct Circle;
    
    // The visitor interface
    struct IShapeVisitor
    {
        virtual ~IShapeVisitor() = default;
        virtual void visit(const Square&) = 0;
        virtual void visit(const Circle&) = 0;
    };
    
    // The shape interface
    struct IShape
    {
        virtual ~IShape() = default;
    
        virtual void accept(IShapeVisitor&) const = 0;
    };

Now the concrete shapes:

    struct Point {
        double x;
        double y;
    };
    
    struct Circle : IShape
    {
        Circle(const Point& center, double radius) : center(center), radius(radius) {}
        
        // Each shape has to implement this method the same way
        void accept(IShapeVisitor& visitor) const override { visitor.visit(*this); }
    
        Point center;
        double radius;
    };

    struct Square : IShape
    {
        Square(const Point& topLeft, double sideLength) :
             topLeft(topLeft), sideLength(sideLength)
        {}
    
        // Each shape has to implement this method the same way
        void accept(IShapeVisitor& visitor) const override { visitor.visit(*this); }
    
        Point topLeft;
        double sideLength;
    };

then the visitors:

    struct ShapePrinter : IShapeVisitor
    {
        void visit(const Square&) override { std::cout << "Square"; }
        void visit(const Circle&) override { std::cout << "Circle"; }
    };
    
    struct ShapeAreaComputer : IShapeVisitor
    {
        void visit(const Square& square) override
        {
            area = square.sideLength * square.sideLength;
        }

        void visit(const Circle& circle) override
        {
             area = M_PI * circle.radius * circle.radius;
        }
    
        double area = 0;
    };
    
    struct ShapePerimeterComputer : IShapeVisitor
    {
        void visit(const Square& square) override { perimeter = 4. * square.sideLength; }
        void visit(const Circle& circle) override { perimeter = 2. * M_PI * circle.radius; }
    
        double perimeter = 0.;
    };

And use it:

    const Square square = {{-1., -1.}, 2.};
    const Circle circle{{0., 0.}, 1.};
    const IShape* shapes[2] = {&square, &circle};

    ShapePrinter shapePrinter;
    ShapeAreaComputer shapeAreaComputer;
    ShapePerimeterComputer shapePerimeterComputer;

    for (const auto* shape : shapes) {
        shape->accept(shapePrinter);
        std::cout << " has an area of ";

        // result will be stored in shapeAreaComputer.area
        shape->accept(shapeAreaComputer);

        // result will be stored in shapePerimeterComputer.perimeter
        shape->accept(shapePerimeterComputer); 

        std::cout << shapeAreaComputer.area
                  << ", and a perimeter of "
                  << shapePerimeterComputer.perimeter
                  << std::endl;
    }

Expected output:

    Square has an area of 4, and a perimeter of 8
    Circle has an area of 3.14159, and a perimeter of 6.28319

[Demo](http://coliru.stacked-crooked.com/a/4e5a6b0873c32410)

**Explanation**:

- In `void Square::accept(IShapeVisitor& visitor) const override { visitor.visit(*this); }`, the static type of `this` is known, and so the chosen (at compile time) overload 
is `void IVisitor::visit(const Square&);`.

- For `square.accept(visitor);` call, the dynamic dispatch through `virtual` is used to know which `accept` to call.

**Pros**:

- You may add new functionality (`SerializeAsXml`, ...) to the class `IShape` just by adding a new visitor.

**Cons**:

- Adding a new concrete shape (`Triangle`, ...) requires to modifying all visitors.


The alternative of putting all functionalities as `virtual` methods in `IShape` has opposite pros and cons: Adding new functionality requires to modify all existing shapes, but adding a new shape doesn't impact existing classes.

## Visitor Example in C++


## Traversing large objects


