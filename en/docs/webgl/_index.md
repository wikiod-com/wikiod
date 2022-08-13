---
title : webgl Tutorial
slug : webgl-tutorial
weight : 9967
draft : false
images : []
type : docs
---

WebGL is a rasterization API that generally runs on your GPU giving you the ability to quickly draw 2D and 3D graphics. WebGL can also be used to do computations on arrays of data.

WebGL is a very low-level API. At a base level WebGL is an engine that runs 2 user supplied functions on the GPU. One function is called a *vertex shader*. A vertex shader's job is to compute vertex positions. Based on the positions the function outputs WebGL can then rasterize various kinds of primitives including points, lines, or triangles. When rasterizing these primitives it calls a second user supplied function called a *fragment shader*. A fragment shader's job is to compute a color for each pixel of the primitive currently being drawn.

These functions are written in a language called GLSL that is somewhat C/C++ like and strictly typed.

It's up to the programmer to supply those functions to make WebGL draw 2d, 3d or compute something. Nearly every piece of WebGL is about setting up those 2 functions and then supplying data to them.

Data can be supplied from 4 sources.

*  Uniforms

Uniforms are inputs to shader functions very much like function parameters or global variables. They are set once before a shader is executed and remain constant
during executing

*  Attributes

Attributes supply data to vertex shaders only. Attributes define how to pull data out of buffers. For example you might put positions, normal, and texture coordinates into a buffer. Attributes let you tell WebGL how to pull that data out of your buffers and supply them to a vertex shader. A vertex shader is called a user specified number of times by calling `gl.drawArrays` or `gl.drawElements` and specifying a count. Each time the current vertex shader is called the next set of data will be pulled from the user specified buffers and put in the attributes

* Textures

Textures are 2D arrays of data up 4 channels. Most commonly those 4 channels are red, green, blue, and alpha from an image. WebGL doesn't care what the data is though. Unlike attributes and buffers, shaders can read values from textures with random access.

* Varyings

Varyings are a way for a vertex shader to pass data to a fragment shader. Varyings are interpolated between the values output by the vertex shader as a primitive is rasterized using a fragment shader



