---
title: "Open Close Principle"
slug: "open-close-principle"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

The Open Close Principle states that the design and writing of the code should be done in a way that new functionality should be added with minimum changes in the existing code. The design should be done in a way to allow the adding of new functionality as new classes, keeping as much as possible existing code unchanged. Software entities like classes, modules and functions should be open for extension but closed for modifications.

Like every principle Open Close Principle is only a principle. Making a flexible design involves additional time and effort spent for it and it introduce new level of abstraction increasing the complexity of the code. So this principle should be applied in those area which are most likely to be changed. 
There are many design patterns that help us to extend code without changing it, for example decorator.

## Open Close Principle violation
    /* 
    * This design have some major issues
    * For each new shape added the unit testing
    * of the GraphicEditor should be redone                      
    * When a new type of shape is added the time
    * for adding it will be high since the developer
    * who add it should understand the logic
    * of the GraphicEditor.
    * Adding a new shape might affect the existing
    * functionality in an undesired way, 
    * even if the new shape works perfectly   
    */ 

    class GraphicEditor {
        public void drawShape(Shape s) {
            if (s.m_type==1)
                drawRectangle(s);
            else if (s.m_type==2)
                drawCircle(s);
        }
        public void drawCircle(Circle r) {....}
        public void drawRectangle(Rectangle r) {....}
    }
 
     class Shape {
         int m_type;
     }
 
     class Rectangle extends Shape {
         Rectangle() {
             super.m_type=1;
         }
     }
 
     class Circle extends Shape {
         Circle() {
             super.m_type=2;
         }
     }



## Open Close Principle support
    /*
    * For each new shape added the unit testing
    * of the GraphicEditor should not be redone
    * No need to understand the sourcecode
    * from GraphicEditor.
    * Since the drawing code is moved to the
    * concrete shape classes, it's a reduced risk
    * to affect old functionallity when new
    * functionallity is added.
    */ 
    class GraphicEditor {
         public void drawShape(Shape s) {
             s.draw();
         }
     }
 
     class Shape {
         abstract void draw();
     }
 
     class Rectangle extends Shape  {
         public void draw() {
             // draw the rectangle
         }
     } 

