---
title: "Getting started with fabricjs"
slug: "getting-started-with-fabricjs"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
Fabric.js is just like any other JS library just specific to canvas. Easy to setup and get started. All you need to do is download the fabric.js from [HERE][1] and include it in your project just like any other JS library for example the way you do it for jQuery. Then create the html file suppose index.html like:

    <head>
        <script src="fabric.js"></script>
    </head>
    
    <body>
        <canvas id="canvas" width="400" height="400" style="border:2px solid #000000</canvas>
        <script>
            var canvas = new fabric.Canvas('canvas');
            canvas.add(new fabric.Circle({ radius: 30, fill: '#f55', top: 100, left: 100 }));
        </script>
    </body>

the src attribute in the script is referring to a file fabric.js kept in the same folder you can keep it in some other folder and give a relative path. And you are good to go.
For more about Fabric visit [Offcial Page][2]. Here is a very basic [Demo][3]


  [1]: https://github.com/kangax/fabric.js/tree/master/dist
  [2]: http://fabricjs.com/
  [3]: http://jsfiddle.net/kLdng24p/13/


## Hello World
    <canvas id="c" width="400" height="400"></canvas>
    var canvas = new fabric.Canvas("c");
    var text = new fabric.Textbox('Hello world From Fabric JS', {
                width:250,
                cursorColor :"blue",
                top:10,
                left:10
            });
    canvas.add(text)

The example creates a text `'Hello world From Fabric JS'` using fabricjs. [Demo][1]


  [1]: http://jsfiddle.net/kLdng24p/17/

