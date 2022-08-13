---
title: "Getting started with actionscript"
slug: "getting-started-with-actionscript"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World Example
ActionScript 2.0:

    _root.createTextField("message", 0, 5, 5, 300, 50);
    var tf:TextFormat = new TextFormat(); 
    tf.color = 0xFF0000;
    tf.size = 32;
    tf.bold = true;
    message.setTextFormat(tf);
    message.text = "Hello World!"; 

First function creates a TextField named "message" in the depth 0 of the _root(MainTimeline) at coordinates (5,5) having dimensions 300x50, where unit is pixel.

Then we create an instance of TextFormat class and assign color, size and bold properties and apply it to the TextField using second function to make our TextField red with fontsize 32 pixels and bold.

Finally, we assign the text property of our newly created TextField to "Hello World!".

ActionScript 3.0:

    import flash.text.TextField;
    import flash.text.TextFormat;

    var message:TextField = new TextField();
    message.x = message.y = 5;
    message.width = 300;
    message.height = 50;

    var tf:TextFormat = new TextFormat(); 
    tf.color = 0xFF0000;
    tf.size = 32;
    tf.bold = true;

    message.defaultTextFormat = tf;
    message.text = "Hello World!";
    MovieClip(root).addChild(message);

Both of the above examples should output something like this:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/HyzSs.png

## Installation or Setup
Detailed instructions on getting actionscript set up or installed.

