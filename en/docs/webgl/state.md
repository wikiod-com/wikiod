---
title: "State"
slug: "state"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

## Textures
Texture units are global state. If they were implemented in JavaScript they would look something like this

    // pseudo code
    gl = {
      activeTextureUnit: 0,
      textureUnits: [
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
        { TEXTURE_2D: ?, TEXTURE_CUBE_MAP: ? },
      ],
    };

You can choose which unit to index with `gl.activeTexture`. 

    // pseudo code
    gl.activeTexture = function(textureUnit) {
      gl.activeTextureUnit = textureUnit - gl.TEXTURE0;
    };

Calling `gl.bindTexture` binds a texture to the active texture unit like this

    // pseudo code
    gl.bindTexture = function(target, texture) {
      var textureUnit = gl.textureUnits[gl.activeTextureUnit];
      textureUnit[target] = texture;
    }

When you have a shader program that uses textures you have to tell that shader program which texture units you bound the textures to. For example if you have a shader like this

    uniform sampler2D diffuse;
    uniform sampler2D normalMap;
    uniform samplerCube environmentMap;

    ...

For you need to query the uniform locations 

    var diffuseUniformLocation = gl.getUniformLocation(someProgram, "diffuse");
    var normalMapUniformLocation = gl.getUniformLocation(someProgram, "normalMap");
    var environmmentMapUniformLocation = gl.getUniformLocation(someProgram,
                                                               "environmentMap");

Then, after you've made your shader program the *current program* 

    gl.useProgram(someProgram);

You then need to tell the shader which texture units you did/will put the textures on. For example

    var diffuseTextureUnit = 3;
    var normalMapTextureUnit = 5;
    var environmentMapTextureUnit = 2;

    gl.uniform1i(diffuseUniformLocation, diffuseTextureUnit);
    gl.uniform1i(normalMapUniformLocation, normalMapTextureUnit);
    gl.uniform1i(environmentMapUniformLocation, environmentMapTextureUnit);

Now you told the shader which units you did/will use. Which texture units you decide to use is entirely up to you.

To actually bind textures to texture units you'd do something like this

    gl.activeTexture(gl.TEXTURE0 + diffuseTextureUnit);
    gl.bindTexture(gl.TEXTURE_2D, diffuseTexture);
    gl.activeTexture(gl.TEXTURE0 + normalMapTextureUnit);
    gl.bindTexture(gl.TEXTURE_2D, normalMapTexture);
    gl.activeTexture(gl.TEXTURE0 + environmentMapTextureUnit);
    gl.bindTexture(gl.TEXTURE_CUBE_MAP, environmentMapTexture);

For very simple WebGL examples that only use 1 texture it's common to never call `gl.activeTexture` since it defaults to texture unit #0. It's also common not to call `gl.uniform1i` because uniforms default to 0 so the shader program, will by default, use texture unit #0 for all textures.

All other texture functions also work off the active texture and texture unit targets. For example `gl.texImage2D` might look something like this

    gl.texImage2D = function(target, level, internalFormat, width, height, 
                             border, format, type, data) {
       var textureUnit = gl.textureUnits[gl.activeTextureUnit];
       var texture = textureUnit[target];

       // Now that we've looked up the texture form the activeTextureUnit and
       // the target we can effect a specific texture
       ...
    };



## Attributes
Attributes are global state (*). If they were implemented in JavaScript they would look something like this

     // pseudo code
     gl = {
       ARRAY_BUFFER: null,
       vertexArray: {
         attributes: [
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
           { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ? },
         ],
         ELEMENT_ARRAY_BUFFER: null,
       },
     }

As you can see above there are `8` attributes and they are global state.

When you call `gl.enableVertexAttribArray(location)` or `gl.disableVertexAttribArray` you can think of it like this

    // pseudo code
    gl.enableVertexAttribArray = function(location) {
      gl.vertexArray.attributes[location].enable = true;
    };

    gl.disableVertexAttribArray = function(location) {
      gl.vertexArray.attributes[location].enable = false;
    };

In other words `location` directly refers to the index of an attribute. 

Similarly `gl.vertexAttribPointer` would be implemented something like this

    // pseudo code
    gl.vertexAttribPointer = function(location, size, type, normalize, stride, offset) {
      var attrib = gl.vertexArray.attributes[location];
      attrib.size = size;
      attrib.type = type;
      attrib.normalize = normalize;
      attrib.stride = stride ? stride : sizeof(type) * size;
      attrib.offset = offset;
      attrib.buffer = gl.ARRAY_BUFFER;  // !!!! <-----
    };

Notice that `attrib.buffer` is set to whatever the current `gl.ARRAY_BUFFER` is set to. `gl.ARRAY_BUFFER` is set by calling `gl.bindBuffer(gl.ARRAY_BUFFER, someBuffer)`.

So, next up we have vertex shaders. In vertex shader you declare attributes. Example

    attribute vec4 position;
    attribute vec2 texcoord;
    attribute vec3 normal;

    ...

    void main() {
      ...
    }

When you link a vertex shader with a fragment shader by calling `gl.linkProgram(someProgram)` WebGL (the driver/GPU/browser) decide on their own which index/location to use for each attribute. You have no idea which ones they're going to pick. It's up the the browser/driver/GPU. So, you have to ask it *which attribute did you use for `position`, `texcoord` and `normal`?*. You do this by calling `gl.getAttribLocation`

    var positionLoc = gl.getAttribLocation(program, "position");
    var texcoordLoc = gl.getAttribLocation(program, "texcoord");
    var normalLoc = gl.getAttribLocation(program, "normal");

Let's say `positionLoc = 5`. That means when the vertex shader executes (when you call `gl.drawArrays` or `gl.drawElements`) the vertex shader expects you to have setup attribute 5 with the correct `type`, `size`, `offset`, `stride`, `buffer` etc.

Note that **BEFORE** you link the program you can choose the locations by calling `gl.bindAttribLoction(program, location, nameOfAttribute)`. Example:

    // Tell `gl.linkProgram` to assign `position` to use attribute #7
    gl.bindAttribLocation(program, 7, "position");

### full attribute state

Missing from the description above is that each attribute also has a default value. It is left out above because it is uncommon to use it.

    attributes: [
       { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ?
         value: [0, 0, 0, 1], },
       { enable: ?, type: ?, size: ?, normalize: ?, stride: ?, offset: ?, buffer: ?
         value: [0, 0, 0, 1], },
       ..

You can set the value with the various `gl.vertexAttribXXX` functions. The `value` is used when `enable` is `false`. When `enable` is true data for the attribute is pulled from the assigned `buffer`. 

# Vertex Array Objects

WebGL has an extension, [`OES_vertex_array_object`](https://www.khronos.org/registry/webgl/extensions/OES_vertex_array_object/)

In the diagram above `OES_vertex_array_object` lets you create and replace the `vertexArray`. In other words

    var vao = ext.createVertexArrayOES();

creates the object you see attached to `gl.vertexArray` in the pseudo code above. Calling `ext.bindVertexArrayOES(vao)` assign your created vertex array object as the current vertex array.

    // pseudo code
    ext.bindVertexArrayOES = function(vao) {
      gl.vertexArray = vao;
    }

This lets you set all of the attributes and `ELEMENT_ARRAY_BUFFER` in the current VAO so that when you want to draw it's one call to `ext.bindVertexArrayOES` where as without the extension it would be up to one call to both `gl.bindBuffer`  `gl.vertexAttribPointer` (and possibly `gl.enableVertexAttribArray`) per attribute.



## Uniforms
Uniforms are *per program* state. Every shader program has its own uniform state and its own locations. 



