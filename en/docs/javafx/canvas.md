---
title: "Canvas"
slug: "canvas"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

A `Canvas` is a JavaFX `Node`, represented as a blank, rectangular area, that can display images, shapes and text. Each `Canvas` contains exactly one `GraphicsContext` object, responsible for receiving and buffering the draw calls, which, at the end, are rendered on the screen by `Canvas`.

## Basic shapes
`GraphicsContext` provides a set of methods to draw and fill geometric shapes. Typically, these methods need coordinates to be passed as their parameters, either directly or in a form of an array of `double` values. The coordinates are always relative to the `Canvas`, whose origin is at the top left corner.

**Note:** `GraphicsContext` will not draw outside of `Canvas` boundaries, i.e. trying to draw outside the `Canvas` area defined by its size and resizing it afterwards will yield no result.

The example below shows how to draw three semi-transparent filled geometric shapes outlined with a black stroke.

    Canvas canvas = new Canvas(185, 70);
    GraphicsContext gc = canvas.getGraphicsContext2D();

    // Set stroke color, width, and global transparency
    gc.setStroke(Color.BLACK);   
    gc.setLineWidth(2d);
    gc.setGlobalAlpha(0.5d);

    // Draw a square
    gc.setFill(Color.RED);
    gc.fillRect(10, 10, 50, 50);
    gc.strokeRect(10, 10, 50, 50);

    // Draw a triangle
    gc.setFill(Color.GREEN);
    gc.fillPolygon(new double[]{70, 95, 120}, new double[]{60, 10, 60}, 3);
    gc.strokePolygon(new double[]{70, 95, 120}, new double[]{60, 10, 60}, 3);

    // Draw a circle
    gc.setFill(Color.BLUE);
    gc.fillOval(130, 10, 50, 50);
    gc.strokeOval(130, 10, 50, 50);

[![Result][1]][1]

  [1]: https://i.stack.imgur.com/OWvjq.png

