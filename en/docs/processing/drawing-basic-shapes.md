---
title: "Drawing Basic Shapes"
slug: "drawing-basic-shapes"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

In Processing, drawing shapes is key to the program. Otherwise, nothing would appear on the screen. This section will tell you how basic shapes are drawn.

## Syntax
 - line(float x1, float y1, float x2, float y2)
 - line(float x1, float y1, float z1, float x2, float y2, float z2)
 - ellipse(float x, float y, float w, float h)
 - rect(float x, float y, float w, float h)
 - triangle(float x1, float y1, float x2, float y2, float x3, float y3)

## Parameters
| Parameter | Details |
| --------- | ------- |
| x1 | x-coordinate of the first point |
| y1 | y-coordinate of the first point |
| z1 | z-coordinate of the first point |
| x2 | x-coordinate of the second point |
| y2 | y-coordinate of the second point |
| z2 | z-coordinate of the second point |
| x3 | x-coordinate of the third point |
| y3 | y-coordinate of the third point |
| x | x-coordinate |
| y | y-coordinate |
| w | width |
| h | height |

You can find a reference on Processing's foundation.

[Processing homepage](https://processing.org/reference/)

## Drawing a Line
Processing provides a method named `line()` to draw a line on the screen. This code draws a white 10 pixel line on black background.

    void setup() {
        size(500, 500);
        background(0);
        stroke(255);
        strokeWeight(10);
    }

    void draw() {
        line(0, 0, 500, 500);
    }

The signature of method `line()` is this.

    line(x1, y1, x2, y2);

`x1` and `y1` is a coordinate of the starting point. `x2` and `y2` is a coordinate of the ending point.

Method `stroke()` is used to specify the color of the line you will draw.

Method `strokeWeight()` is used to specify the thickness of the line you will draw. (in pixels)

## Drawing a Rectangle
Processing provides method `rect()` to draw a rectangle. This code draws a white 50 X 50 rectangle on black background.

    void setup() {
        size(500, 500);
        background(0);
        fill(255);
        noStroke();
    }

    void draw() {
        rect(225, 225, 50, 50);
    }

The signature of method `rect()` is this.

    rect(x, y, w, h);

`x` and `y` is the coordinate of the rectangle. `w` and `h` is rectangle's width and height.

Method `fill()` is used to specify the filling color of the rectangle and other shapes such as ellipse, triangle, polygon.

Method `noStroke()` is used to specify that that there are no strokes around the rectangle. This method also affects other shapes such as ellipse, triangle, polygon.


## Drawing an Ellipse
Processing provides method `ellipse` in order to draw ellipse. This code draws a white circle which has radius of 25 pixels.

    void setup() {
        size(500, 500);
        background(0);
        fill(255);
        noStroke();
    }

    void draw() {
        ellipse(225, 225, 50, 50);
    }

The signature of method `ellipse()` is this.

    ellipse(x, y, w, h);

`x` and `y` is the coordinate of the ellipse. `w` and `h` is ellipse's width and height.

## Drawing a Triangle
Processing provides the method `triangle` in order to draw a triangle. The code below draws a nearly equilateral triangle of 25 pixels between each defining point.

    void setup() {
        size(500, 500);
        background(0);
    }
    void draw() {
        triangle(0, 0, 25, 0, 12, 12);
    }
The signature of `triangle` is as so:

    triangle(x1, y1, x2, y2, x3, y3);
Each `x` point corresponds to the point's x axis, and `y` to the y axis. The three points will be joined to form a triangle.

## Drawing a Triangle
Processing provides method `triangle()` to draw a triangle. This code draws a white triangle on black background.

    void setup() {
        size(500, 500);
        background(0);
        fill(255);
        noStroke();
    }

    void draw() {
        triangle(250, 225, 225, 275, 275, 275);
    }

