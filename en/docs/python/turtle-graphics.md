---
title: "Turtle Graphics"
slug: "turtle-graphics"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

## Ninja Twist (Turtle Graphics)

Here a Turtle Graphics Ninja Twist:
[![Expected Output][1]][1]




    


    import turtle 
    
    ninja = turtle.Turtle()
    
    ninja.speed(10)
    
    for i in range(180):
        ninja.forward(100)
        ninja.right(30)
        ninja.forward(20)
        ninja.left(60)
        ninja.forward(50)
        ninja.right(30)
        
        ninja.penup()
        ninja.setposition(0, 0)
        ninja.pendown()
        
        ninja.right(2)
        
    turtle.done()


  [1]: https://i.stack.imgur.com/3YP3j.png

