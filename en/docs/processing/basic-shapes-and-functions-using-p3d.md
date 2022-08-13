---
title: "Basic shapes and functions using P3D"
slug: "basic-shapes-and-functions-using-p3d"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Syntax
 - translate(float x, float y, float z)
 - rotateX(float angle)
 - rotateY(float angle)
 - rotateZ(float angle)
 - box(float size)
 - box(float w, float h, float d)

## Parameters
| Parameters | Details |
| ---------- | ------- |
| angle | the angle is in radians
| size | the dimension of the box to be used for all its dimensions
| w | the dimension of the box in the `x-axis`
| h | the dimension of the box in the `y-axis`
| d | the dimension of the box in the `z-axis`

## 3D Translation
Here's how to translate objects in P3D:

    size(200, 200, P3D); //Starting P3D renderer
    fill(255, 0, 0, 150); //transparent red
    rect(10, 10, 100, 100); //first rectangle
    fill(0, 0, 255, 150); //transparent blue
    translate(50, 50, 50); //translate x, y and z by 50 pixels
    rect(0, 0, 100, 100); //second rectangle (same dimensions as the first one)

[![Translation example][1]][1]

Red: first rectangle
Blue: second rectangle

As can be seen from the above sketch, the second rectangle only appears to be larger than the first one, when in reality it is "closer" to the screen as a result of translating the rectangle `50` pixels along the `z-axis` (and of course, the rectangle has been translated along the `x` and `y` axes).


  [1]: https://i.stack.imgur.com/GVX0P.png

## 3D Rotation
There are three functions for 3D rotation: `rotateX(angle)`, `rotateY(angle)` and `rotateZ(angle)` for rotation in their respective axes where `angle` is in radians. 

    size(200, 200, P3D); //Starting P3D renderer
    fill(255, 0, 0, 150); //transparent red
    translate(width/2, height/2);//translate to centre, ie (100, 100)
    rectMode(CENTER);//This makes the rectangle centre in (100, 100)
    rect(0, 0, 100, 100); //first rectangle
    fill(0, 0, 255, 150); //transparent blue
    rotateX(PI/4); //rotate in the x-axis by PI/4 radians (45 degrees)
    rect(0, 0, 100, 100); //second rectangle (same dimensions as the first one)

[![rotateX][1]][1]

    rotateY(radians(45)); //rotate in the y-axis by passing the radians conversion of 45 degrees

[![rotateY][2]][2]

    rotateZ(3*PI/4); //rotate in the z-axis by 3*PI/4 radians (270 degrees)

[![rotateZ][3]][3]

> Note: transformations (such as translations and rotations) add on to the previous transformation.


  [1]: https://i.stack.imgur.com/hQxCI.png
  [2]: https://i.stack.imgur.com/uV9xy.png
  [3]: https://i.stack.imgur.com/FD2H2.png

## Drawing a cuboid
To draw a cuboid, you have to use the `box()` function by giving its dimensions as its parameters.

    size(200, 200, P3D); //Starting the P3D renderer
    translate(width/2, height/2); //Translating to the centre of the sketch
    rotateY(PI/4); //rotate so that...
    rotateX(PI/6); //... it will be easy to see the box
    noFill(); //disabling the box's fill, so that we will be able to see its edges
    box(100, 50, 75); //the box function requires its dimensions as its parameters

[![box with three parameters][1]][1]

> Note that the `box()` function does *not* accept its position as the parameters

There also is a way to call the `box()` function with only one parameter. In this case, it will be a cube.

    stroke(0, 100, 255); //change the edges' colour
    fill(0, 0, 255); //fill the `box` in a blue colour
    box(100); //draw a cube

[![cube with box function][2]][2]


  [1]: https://i.stack.imgur.com/7YlxF.png
  [2]: https://i.stack.imgur.com/b1LvA.png

