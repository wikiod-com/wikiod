---
title: "Getting started with processing"
slug: "getting-started-with-processing"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
The easiest way to write Processing code is to simply call a series of functions. Press the run button in the Processing editor, and Processing will run your code. Here's an example:

    size(200, 200);
    background(0, 0, 255);
    fill(0, 255, 0);
    ellipse(100, 100, 100, 100);

This code creates a `200x200` window, draws a blue background, changes the fill color to green, and then draws a circle in the middle of the screen.

![green circle on blue background](http://i.stack.imgur.com/Nh6bN.png)

However, most Processing sketches will use the predefined `setup()` and `draw()` functions.

- The `setup()` function is called automatically by Processing, once at the very beginning of the sketch. This function is used for doing the initial setup, such as `size`, and loading of resources such as image and sound files.
- The `draw()` function is called automatically by Processing 60 times per second. This function is used for drawing and getting user input.

      void setup() {
        size(200, 200);
      }

      void draw(){
        background(0);
        ellipse(mouseX, mouseY, 25, 25);
      }

This code creates a `200x200` window and then draws a circle at the current mouse position.

![circle at mouse](http://i.stack.imgur.com/q2qFl.gif)

## Installation and Setup
The easiest way to use Processing is by downloading the Processing editor from [the Processing download page](https://processing.org/download/).

That comes as a zip file. Unzip that file anywhere, and you'll have a directory that contains an executable (on Windows, that's `processing.exe`).

Running that executable opens up the Processing editor:

![Processing editor](http://i.stack.imgur.com/AVlzl.png)

The Processing editor (also called the Processing Development Environment, or PDE) contains many tools that do a lot of work for you. It allows you to write Processing code, which it automatically converts to Java and then compiles and runs for you.

The PDE contains many features, but for now just write your Processing code inside the white section of the editor, and then press the play button to run your code. See the Hello World section below for some example code.

You can also write Processing code using other basic code editors like [Atom](https://atom.io) or [Sublime Text](https://www.sublimetext.com), or with a more advanced  IDE like [eclipse](https://eclipse.org/).

