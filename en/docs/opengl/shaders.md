---
title: "Shaders"
slug: "shaders"
draft: false
images: []
weight: 9964
type: docs
toc: true
---

## Syntax
- #version version_number // Which GLSL version we are using
- void main() { /* Code */ } // Shader's main function
- in type name; // Specifies an input parameter - GLSL 1.30
- out type name; // Specifies an output parameter - GLSL 1.30
- inout type name; // Parameter for both input and output - GLSL 1.30

## Parameters
| Parameter | Details |
| ------ | ------ |
| type   | The parameter's type, it has to be a GLSL built-in type. |

To specify which version of GLSL should be used to compile a shader, use the version **preprocessor** e.g. `#version 330`. Each version of OpenGL is required to support specific [versions of GLSL][1]. If a `#version`​ preprocessor is not defined at the top of a shader, the default version 1.10 is used.

  [1]: https://www.wikiod.com/glsl/getting-started-with-glsl

## Shader for rendering a coloured rectangle
A shader program, in the **OpenGL** sense, contains a number of different shaders.  Any shader program must have at least a **vertex** shader, that calculates the position of the points on the screen, and a **fragment** shader, that calculates the colour of each pixel.  (Actually the story is longer and more complex, but anyway...) 

The following shaders are for `#version 110`, but should illustrate some points:

**Vertex shader:**

    #version 110
    
    // x and y coordinates of one of the corners
    attribute vec2 input_Position;
    
    // rgba colour of the corner. If all corners are blue, 
    // the rectangle is blue. If not, the colours are 
    // interpolated (combined) towards the center of the rectangle    
    attribute vec4 input_Colour; 

    // The vertex shader gets the colour, and passes it forward     
    // towards the fragment shader which is responsible with colours
    // Must match corresponding declaration in the fragment shader.  
    varying vec4 Colour;    
    
    void main()
    {
        // Set the final position of the corner
        gl_Position = vec4(input_Position, 0.0f, 1.0f);
        
        // Pass the colour to the fragment shader
        UV = input_UV;
    }

**Fragment shader:**

    #version 110
    
    // Must match declaration in the vertex shader.  
    varying vec4 Colour;
    
    void main()
    {
        // Set the fragment colour
        gl_FragColor = vec4(Colour);
    }

