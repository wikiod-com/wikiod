---
title: "Getting started with webgl"
slug: "getting-started-with-webgl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
WebGL is a browser technology so there isn't much to set up other than to have a browser. You can get started with WebGL on [JSFiddle](http://jsfiddle.net) or [Codepen](http://codepen.io) or [JSBIn](http://jsbin.com) or any number of other sites that let you edit HTML, CSS, and JavaScript online though there will be a few limitations [(see below)](#limitations-of-webgl-on-online-services). You can also host open source files on [github pages](https://pages.github.com/) or similar services.

On the other hand at some point you're probably going to work locally. To do that it's recommended you run a simple web server. There are plenty to choose from that are simple to use and require very little setup.

Using node.js as a server

1. install [node.js](http://nodejs.org)
2. Open a terminal or node command prompt and type `npm install -g http-server` (on OSX put `sudo` in front of that.
3. type `http-server` to start serving the files in the current folder OR `http-server path-to-folder` to server a different folder
4. Point your browser to `http://localhost:8080/name-of-file` to view your WebGL webpage

Using devd as a server

1. Download [devd](https://github.com/cortesi/devd)
2. Open a terminal and run devd with either `devd .` to server files from the current folder or `devd path-to-folder` to serve a different folder
3. Point your browser to `http://localhost:8000/name-of-file` to view your WebGL webpage

Using Servez as a server

1. Download [Servez](https://greggman.github.io/servez/)
2. Install It, Run it
3. Choose the folder to serve
4. Pick "Start"
5. Go to `http://localhost:8080` or pick "Launch Browser"

![servez](https://github.com/greggman/servez/raw/master/servez.gif)

Using "*Web Server for Chrome*" Chrome Extension

1. Install the [Web Server From Chrome](https://chrome.google.com/webstore/detail/web-server-for-chrome/ofhbbkphhbklhfoeikjpcbhemlocgigb?hl=en)

2. Launch it from the *Apps* icon on a new tab page.

   [![how to launch app on chrome][1]][1]

3. Set the folder where your files are then click the `http://127.0.0.1:8787` link

   [![settings dialog][2]][2]

### Limitation of WebGL on Online Services

In WebGL it is very common to load images. In WebGL there are restrictions on how images can be used. Specifically WebGL can not use images from other domains without permission from the server hosting the images. Services that currently give permission to use images include imgur and flickr. See [Loading Cross Domain Images](tbd). Otherwise you'll need to have the images on the same server as your webgl page or use other creative solutions like generating images with a canvas tag


  [1]: http://i.stack.imgur.com/3fHLG.gif
  [2]: http://i.stack.imgur.com/GRBJT.png

## Hello World
Like it mentions in the *remarks* section we need to supply two functions. A vertex shader and a fragment shader

Let's start with a vertex shader

<!-- language: lang-glsl -->

    // an attribute will receive data from a buffer
    attribute vec4 position;

    // all shaders have a main function
    void main() {

      // gl_Position is a special variable a vertex shader 
      // is responsible for setting
      gl_Position = position;
    }

If the entire thing was written in JavaScript instead of GLSL you could imagine it would be used like this

<!-- language: lang-js -->

    // *** PSUEDO CODE!! ***

    var positionBuffer = [
      0, 0, 0, 0,
      0, 0.5, 0, 0,
      0.7, 0, 0, 0,
    ];
    var attributes = {};
    var gl_Position;
    
    drawArrays(..., offset, count) {
      for (var i = 0; i < count; ++i) {
         // copy the next 4 values from positionBuffer to the position attribute
         attributes.position = positionBuffer.slice((offset + i) * 4, 4); 
         runVertexShader();
         ...
         doSomethingWith_gl_Position();
    }

Next we need a fragment shader

<!-- language: lang-glsl -->

    // fragment shaders don't have a default precision so we need
    // to pick one. mediump, short for medium precision, is a good default.
    precision mediump float;

    void main() {
      // gl_FragColor is a special variable a fragment shader
      // is responsible for setting
      gl_FragColor = vec4(1, 0, 0.5, 1); // return redish-purple 
    }

Above we're setting `gl_FragColor` to `1, 0, 0.5, 1` which is 1 for red, 0 for green, 0.5 for blue, 1 for alpha. Colors in WebGL go from 0 to 1.

Now that we have written the 2 functions lets get started with WebGL

First we need an HTML canvas element

<!-- language: lang-html -->

     <canvas id="c"></canvas>

Then in JavaScript we can look that up

<!-- language: lang-js -->

     var canvas = document.getElementById("c");

Now we can create a WebGLRenderingContext

<!-- language: lang-js -->

     var gl = canvas.getContext("webgl");
     if (!gl) {
        // no webgl for you!
        ...

Now we need to compile those shaders to put them on the GPU so first we need to get them into strings. You can get your strings any way you normal get strings. By concatenating, by using AJAX, by putting them in non-JavaScript typed script tags, or in this case by using multiline template literals

<!-- language: lang-js -->

    var vertexShaderSource = `
    // an attribute will receive data from a buffer
    attribute vec4 position;

    // all shaders have a main function
    void main() {

      // gl_Position is a special variable a vertex shader 
      // is responsible for setting
      gl_Position = position;
    }
    `;

    var fragmentShaderSource = `
    // fragment shaders don't have a default precision so we need
    // to pick one. mediump is a good default
    precision mediump float;

    void main() {
      // gl_FragColor is a special variable a fragment shader
      // is responsible for setting
      gl_FragColor = vec4(1, 0, 0.5, 1); // return redish-purple 
    }
    `;

Then we need a function that will create a shader, upload the source and compile the shader

<!-- language: lang-js -->

    function createShader(gl, type, source) {
      var shader = gl.createShader(type);
      gl.shaderSource(shader, source);  
      gl.compileShader(shader);
      var success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
      if (success) {
        return shader;
      }

      console.log(gl.getShaderInfoLog(shader));
      gl.deleteShader(shader);
    }

We can now call that function to create the 2 shaders

<!-- language: lang-js -->

    var vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);

We then need to *link* those 2 shaders into a *program*

<!-- language: lang-js -->

    function createProgram(gl, vertexShader, fragmentShader) {
      var program = gl.createProgram();
      gl.attachShader(program, vertexShader);
      gl.attachShader(program, fragmentShader);
      gl.linkProgram(program);
      var sucesss = gl.getProgramParameter(program, gl.LINK_STATUS);
      if (success) {
        return program;
      }

      console.log(gl.getProgramInfoLog(program));
      gl.deleteProgram(program);
    }
    
And call it

<!-- language: lang-js -->

    var program = createProgram(gl, vertexShader, fragmentShader);

Now that we've created a GLSL program on the GPU we need to supply data to it. The majority of the WebGL API is about setting up state to supply data to our GLSL programs. In this case our only input to our GLSL program is `position` which is an attribute. The first thing we should do is look up the location of the attribute for the program we just created

<!-- language: lang-js -->

    var positionAttributeLocation = gl.getAttribLocation(program, "position");

Attributes get their data from buffers so we need to create a buffer

<!-- language: lang-js -->

    var positionBuffer = gl.createBuffer();

WebGL lets us manipulate many WebGL resources on global bind points. You can think of bind points as internal global variables inside WebGL. First you set the bind point to your resource. Then, all other functions refer to the resource through the bind point. So, let's bind the position buffer.

<!-- language: lang-js -->

    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);

Now we can put data in that buffer by referencing it through the bind point

<!-- language: lang-js -->

    // three 2d points
    var positions = [
      0, 0, 
      0, 0.5,
      0.7, 0,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

There's a lot going on here. The first thing is we have `positions` which is a JavaScript array. WebGL on other hand needs strongly typed data so the line `new Float32Array(positions)` creates a new array of 32bit floating point numbers and copies the values from `positions`. `gl.bufferData` then copies that data to the `positionBuffer` on the GPU. It's using the position buffer because we bound it to the `ARRAY_BUFFER` bind point above.

The last argument, `gl.STATIC_DRAW` is a hint to WebGL about how we'll use the data. It can try to use that info to optimize certain things. `gl.STATIC_DRAW` tells WebGL we're not likely to change this data much.

Now that we've put data in the a buffer we need to tell the attribute how to get data out of it. First off we need to turn the attribute on

<!-- language: lang-js -->

    gl.enableVertexAttribArray(positionAttributeLocation);

Then we need to specify how to pull the data out

<!-- language: lang-js -->

    var size = 2;          // 2 components per iteration
    var type = gl.FLOAT;   // the data is 32bit floats
    var normalize = false; // use the data as is
    var stride = 0;        // 0 = move size * sizeof(type) each iteration
    var offset = 0;        // start at the beginning of the buffer
    gl.vertexAttribPointer(
       positionAttributeLocation, size, type, normalize, stride, offset)

A hidden part of `gl.vertexAttribPointer` is that it binds the current `ARRAY_BUFFER` to the attribute. In other words now that this attribute is bound to `positionBuffer` we're free to bind something else to the `ARRAY_BUFFER` bind point.

note that from the point of view of our GLSL vertex shader the position attribute was a `vec4`

    attribute vec4 position;

`vec4` is a 4 float value. In JavaScript you could think of it something like `position = {x: 0, y: 0, z: 0, w: 0}`. Above we set `size = 2`. Attributes default to `0, 0, 0, 1` so this attribute will get its first 2 values (x and y) from our buffer. The z, and w will be the default 0 and 1 respectively.

After all that we can finally ask WebGL to execute are GLSL program. 

<!-- language: lang-js -->

    var primitiveType = gl.TRIANGLES;
    var offset = 0;
    var count = 3;
    gl.drawArrays(primitiveType, offset, count);

This will execute our vertex shader 3 times. The first time `position.x` and `position.y` in our vertex shader will be set to the first 2 values from the positionBuffer. The 2nd time `position.xy` will be set to the 2nd 2 values. The last time it will be set to the last 2 values.

Because we set `primitiveType` to `gl.TRIANGLES`, each time our vertex shader is run 3 times WebGL will draw a triangle based on the 3 values we set `gl_Position` to. No matter what size our canvas is those values are in clip space coordinates that go from -1 to 1 in each direction.

Because our vertex shader is simply copying our positionBuffer values to `gl_Position` the triangle will be drawn at clip space coordinates

      0, 0, 
      0, 0.5,
      0.7, 0,

How those values translate to pixels depends on the `gl.viewport` setting. `gl.viewport` defaults to the initial size of the canvas. Since we didn't set a size for our canvas it's the default size of 300x150. Converting from clip space to pixels (often called screen space in WebGL and OpenGL literature) WebGL is going to draw a triangle at

     clip space      screen space
       0, 0       ->   150, 75
       0, 0.5     ->   150, 112.5
     0.7, 0       ->   255, 75

WebGL will now render that triangle. For every pixel it is about to draw it will call our fragment shader. Our fragment shader just sets `gl_FragColor` to `1, 0, 0.5, 1`. Since the Canvas is an 8bit per channel canvas that means WebGL is going to write the values `[255, 0, 127, 255]` into the canvas. 

There are 3 major things we still haven't covered from the *remarks*. Textures, varyings, and uniforms. Each of those requires it's own topic.





