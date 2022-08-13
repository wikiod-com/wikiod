---
title: "FabricJS canvas Serialization"
slug: "fabricjs-canvas-serialization"
draft: false
images: []
weight: 9996
type: docs
toc: true
---

Once you have started building an application with FabricJS it won't be late when you realize that you will need to save results of the canvas/its contents to server. Or may exporting them as files to the client. For this very purpose you will need **canvas serialization**. Now some of you might ask Why Serialization we can save it as an image. So the answer would Saving image to a server needs bandwidth and text proves much better in this case.

## Syntax
 1. **JSON.stringify(canvas)** - implicitly calls toJSON method on passed object. Gives String representation
 2. **canvas.toObject()** - returns the same representation as toJSON, only in a form of actual object
 3. **canvas.toSVG()** - returns an SVG representation of the canvas

For more about FabricJS canvas serialization Refer the link [Canvas Serialization][1]


  [1]: http://fabricjs.com/fabric-intro-part-3#serialization

## Canvas Serilization
    <canvas id = "canvas" height='400' width='500'></canvas>
    
    var canvas = new fabric.Canvas(document.getElementById('canvas'));
    console.log(JSON.stringify(canvas)); // '{"objects":[],"background":""}'
    
    canvas.add(new fabric.Rect({
      left: 10,
      top: 10,
      height: 50,
      width: 50,
      fill: 'green',
         stroke:'black'
    }));
    canvas.renderAll();
    
    console.log(JSON.stringify(canvas));//logs the string representation
    console.log(canvas.toObject());//logs canvas as an object
    console.log(canvas.toSVG());//logs the SVG representation of canvas

[***Fiddle***][1]


  [1]: https://jsfiddle.net/2hcz6L8d/

