---
title: "Open Closed Principle (OCP)"
slug: "open-closed-principle-ocp"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Software entities (class, modules, functions etc) should be open for extension but closed for modification.

## Open Closed Principle C#
Here, we try to explain OCP using codebase. First we'll show a scenario that violate OCP and then we'll remove that violation.

Area Calculation (OCP violation Code) :

    public class Rectangle{
     public double Width {get; set;}
     public double Height {get; set;}
    }
    
    public class Circle{
     public double Radious {get; set;}
    }
    
    public double getArea (object[] shapes){
     double totalArea = 0;
    
     foreach(var shape in shapes){
      if(shape is Rectangle){
       Rectangle rectangle = (Rectangle)shape;
       totalArea += rectangle.Width * rectangle.Height;
      }
      else{
       Circle circle = (Circle)shape;
       totalArea += circle.Radious * circle.Radious * Math.PI;
      }
     }
    }
Now if we need to calculate another another type of object (say, Trapezium) then we've to add another condition. But from the rule's of OCP we know **Software entities should be closed for modification**. So it violates OCP.

Ok. Let's try to solve this violation implementing OCP.

    public abstract class shape{
     public abstract double Area();
    }
    
    public class Rectangle : shape{
     public double Width {get; set;}
     public double Height {get; set;}
    
     public override double Area(){
      return Width * Height;
     }
    }
    
    public class Circle : shape{
     public double Radious {get; set;}
    
     public override double Area(){
      return Radious * Radious * Math.PI;
     }
    }
    
    public double getArea (shape[] shapes){
     double totalArea = 0;
    
     foreach(var shape in shapes){
      totalArea += shape.Area();
     }
    
     return totalArea;
    }
Now if we need to calculate another type of object, we don't need to change logic (in getArea()), we just need to add another class like *Rectangle or Circle*.

