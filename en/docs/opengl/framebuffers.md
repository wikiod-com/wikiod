---
title: "Framebuffers"
slug: "framebuffers"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Basics of framebuffers
***Framebuffer*** is a type of buffer which stores **color** values, **depth** and **stencil** information of pixels in memory. When you draw something in OpenGL the output is stored in the *default framebuffer* and then you actually see the color values of this buffer on screen. You can also make your own framebuffer which can be used for a lot of cool post-processing effects such as *gray-scale, blur, depth of field, distortions, reflections*... 

To start of you need to create a framebuffer object (**FBO**) and bind it like any other object in OpenGL:

    unsigned int FBO;
    glGenFramebuffers(1, &FBO);
    glBindFramebuffer(GL_FRAMEBUFFER, FBO);  

Now you have to add at least one **attachment** (color, depth or stencil) to the framebuffer. An attachment is a memory location that acts as a buffer for the framebuffer. It can either be a *texture*, or a *renderbuffer object*. The advantage of using a texture is that you can easily use this texture in a post-processing shaders. Creating the texture is similar as a normal texture:

    unsigned int texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
  
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, NULL);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE); 

The `width` and `height` should be the same as your rendering window size. The texture data pointer is `NULL` because you only want to allocate the memory and not fill the texture with any data. The texture is ready so you can actually attach it to the framebuffer:

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, texture, 0);  

Your framebuffer should be ready to use now but you may want to also add depth attachment or both depth and stencil attachments. If you want to add those as texture attachments (and use them for some processing) you can create another textures like above. The only difference would be in these lines:

    glTexImage2D(
        GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, width, height, 0, 
        GL_DEPTH_COMPONENT, GL_FLOAT, NULL
    );

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, texture, 0);

Or these if you want to use depth **and** stencil attachment in a single texture:

    glTexImage2D(
        GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, width, height, 0, 
        GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, NULL
    );

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, texture, 0);

You can also use a **renderbuffer** instead of a texture as an attachment for depth and stencil buffers if don't want to process the values later. (It will be explained in another example...)

You can check if the framebuffer is successfully created and completed without any errors:

    if(glCheckFramebufferStatus(GL_FRAMEBUFFER) == GL_FRAMEBUFFER_COMPLETE)
        // do something...

And finally don't forget to unbind the framebuffer so that you don't accidentally render to it:

    glBindFramebuffer(GL_FRAMEBUFFER, 0);  

### Limits ###

The maximum number of color buffers which can be attached to a single frame buffer can be determined by the OGL function [glGetIntegerv](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml), by using the parameter `GL_MAX_COLOR_ATTACHMENTS`:

    GLint maxColAttchments = 0;
    glGetIntegerv( GL_MAX_COLOR_ATTACHMENTS, &maxColAttchments );

----------
## Using the framebuffer ##

The usage is quite straightforward. Firstly you bind your **framebuffer** and render your scene into it. But you won't actually see anything yet because your renderbuffer is not visible. So the second part is to render your framebuffer as a texture of a fullscreen quad onto the screen. You can just render it as it is or do some post-processing effects.

Here are the vertices for a fullscreen quad:

    float vertices[] = {
    //   positions     texture coordinates
        -1.0f,  1.0f,  0.0f, 1.0f,
        -1.0f, -1.0f,  0.0f, 0.0f,
         1.0f, -1.0f,  1.0f, 0.0f,

        -1.0f,  1.0f,  0.0f, 1.0f,
         1.0f, -1.0f,  1.0f, 0.0f,
         1.0f,  1.0f,  1.0f, 1.0f
    };

You will need to store them in a VBO or render using attribute pointers. You will also need some basic shader program for rendering the fullscreen quad with texture. 

Vertex shader:

    in vec2 position;
    in vec2 texCoords;

    out vec2 TexCoords;

    void main()
    {
        gl_Position = vec4(position.x, position.y, 0.0, 1.0); 
        TexCoords = texCoords;
    }  

Fragment shader:

    in vec2 TexCoords;
    out vec4 color;

    uniform sampler2D screenTexture;

    void main()
    { 
        color = texture(screenTexture, TexCoords);
    }

**Note:** You may need to adjust the shaders for your version of *GLSL*.

Now you can do the actual rendering. As described above, the first thing is to render the scene into your FBO. To do that you simply bind your FBO, clear it and draw the scene:

    glBindFramebuffer(GL_FRAMEBUFFER, FBO);
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
    // draw your scene here...   

**Note:** In `glClear` function you should specify all framebuffer attachments you are using (In this example color and depth attachment).

Now you can render your FBO as a fullscreen quad on the default framebuffer so that you can see it. To do this you simply unbind your FBO and render the quad:

    glBindFramebuffer(GL_FRAMEBUFFER, 0); // unbind your FBO to set the default framebuffer
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
  
    shader.Use(); // shader program for rendering the quad  

    glBindTexture(GL_TEXTURE_2D, texture); // color attachment texture
    glBindBuffer(GL_ARRAY_BUFFER, VBO); // VBO of the quad
    // You can also use VAO or attribute pointers instead of only VBO...
    glDrawArrays(GL_TRIANGLES, 0, 6); 
    glBindBuffer(GL_ARRAY_BUFFER, 0);

And that's all! If you have done everything correctly you should see the same scene as before but rendered on a fullscreen quad. The visual output is the same as before but now you can easily add post-processing effects just by editing the fragment shader. (I will add effects in another example(s) and link it here)


