---
title: "Texturing"
slug: "texturing"
draft: false
images: []
weight: 9920
type: docs
toc: true
---

## Basics of texturing
A texture is a form of data storage that allows convenient access not just to particular data entries, but also to sample points mixing (interpolating) multiple entries together.

In OpenGL textures can be used for many things, but most commonly it's mapping an image to a polygon (for example a triangle). In order to map the texture to a triangle (or another polygon) we have to tell each vertex which part of the texture it corresponds to. We assign a texture coordinate to each vertex of a polygon and it will be then interpolated between all fragments in that polygon. Texture coordinates typically range from 0 to 1 in the x and y axis as shown in the image below:

[![texture coordinates][1]][1]

The texture coordinates of this triangle would look like this: 

    GLfloat texCoords[] = {
        0.0f, 0.0f,  // Lower-left corner  
        1.0f, 0.0f,  // Lower-right corner
        0.5f, 1.0f   // Top-center corner
    };

Put those coordinates into VBO (vertex buffer object) and create a new attribute for the shader. You should already have at least one attribute for the vertex positions so create another for the texture coordinates.

----------
## Generating texture ##

First thing to do will be to generate a texture object which will be referenced by an ID which will be stored in an unsigned int *texture*:
    
    GLuint texture;
    glGenTextures(1, &texture); 

After that it has to be bound so all subsequent texture commands will configure this texture:

    glBindTexture(GL_TEXTURE_2D, texture); 


----------
    
## Loading image ##

To load an image you can create your own image loader or you can use an image-loading library such as [SOIL][2] (Simple OpenGL Image Library) in c++ or [TWL's PNGDecoder][3] in java.

An example of loading image with SOIL would be:

    int width, height;
    unsigned char* image = SOIL_load_image("image.png", &width, &height, 0, SOIL_LOAD_RGB); 

Now you can assign this image to the texture object:

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, image);

After that you should unbind the texture object:

    glBindTexture(GL_TEXTURE_2D, 0); 


----------

## Wrap parameter for texture coordinates ##

As seen above, the lower left corner of the texture has the UV (st) coordinates (0, 0) and the upper right corner of the texture has the coordinates (1, 1), but the texture coordinates of a mesh can be in any range. 
To handle this, it has to be defined how the texture coordinates are wrapped to the the texture.

The wrap parameter for the texture coordinate can be set with [glTextureParameter](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glTexParameter.xhtml) using `GL_TEXTURE_WRAP_S`, `GL_TEXTURE_WRAP_T` and `GL_TEXTURE_WRAP_R`.

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

The possible parameters are:

- `GL_CLAMP_TO_EDGE` causes the texture coordinates to be clamped to the range *[1/2N, 1 - 1/2N]*, where *N* is the size of the texture in the direction. 

- `GL_CLAMP_TO_BORDER` does the same as `GL_CLAMP_TO_EDGE`, but in cases where clamping, the fetched texel data is substituted with the color specified by `GL_TEXTURE_BORDER_COLOR`.

- `GL_REPEAT` causes the integer part of the texture coordinate to be ignored. The texture is **tiled**.

[![repeat texture][4]][4]

- `GL_MIRRORED_REPEAT`: If the integer part of the texture coordinate is even, then the it is ignored. In contrast to, if the integer part of the texture coordinate is odd, then the texture coordinate is set to *1 - frac(s)*. *fract(s)* is the fractional part of the texture coordinate. That causes the texture to be **mirrored** every 2nd time. 

[![mirror texture][5]][5]

- `GL_MIRROR_CLAMP_TO_EDGE` causes the the textue coordinate to be repeated as for `GL_MIRRORED_REPEAT `for one reptition of the texture, at which point the coordinate to be clamped as in `GL_CLAMP_TO_EDGE`.


Note the default value for `GL_TEXTURE_WRAP_S`, `GL_TEXTURE_WRAP_T` and `GL_TEXTURE_WRAP_R` is `GL_REPEAT`.

----------
## Applying textures ##

The last thing to do is to bind the texture before the draw call:
        
    glBindTexture(GL_TEXTURE_2D, texture);


  [1]: http://i.stack.imgur.com/88Nw6.png
  [2]: http://www.lonesock.net/soil.html
  [3]: http://wiki.lwjgl.org/wiki/Loading_PNG_images_with_TWL's_PNGDecoder
  [4]: https://i.stack.imgur.com/uI0Jt.png
  [5]: https://i.stack.imgur.com/6ZyNw.png

## Texture and Framebuffer
You can attach an image in a texture to a framebuffer, so that you can render directly to that texture.

    glGenFramebuffers (1, &framebuffer);
    glBindFramebuffer (GL_FRAMEBUFFER, framebuffer);
    glFramebufferTexture2D(GL_FRAMEBUFFER,
                           GL_COLOR_ATTACHMENT0,
                           GL_TEXTURE_2D,
                           texture,
                           0);

**Note:** you can't read and write from same texture in same render task, because it call undefined behaviour. But you can use: `glTextureBarrier()` between render calls for this.

## Read texture data
You can read texture data with function `glGetTexImage`:

    char *outBuffer = malloc(buf_size);
    
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);
    
    glGetTexImage(GL_TEXTURE_2D,
                  0,
                  GL_RGBA,
                  GL_UNSIGNED_BYTE,
                  outBuffer);

**Note:** texture type and format are only for example and can be different.

## Using textures in GLSL shaders
The vertex shader only accepts the texture coordinates as a vertex attribute and forwards the coordinates to the fragment shader. By default, it will also guarantee that the fragment will receive the properly interpolated coordinate based on its position in a triangle: 

    layout (location = 0) in vec3 position;
    layout (location = 1) in vec2 texCoordIn;

    out vec2 texCoordOut;

    void main()
    {
        gl_Position = vec4(position, 1.0f);
        texCoordOut = texCoordIn;
    }

The fragment shader then accepts the `texCoord` output variable as an input variable. You can then add a texture to the fragment shader by declaring a `uniform sampler2D`. To sample a fragment of the texture we use a built-in function `texture` which has two parameters. First is the texture we want to sample from and the second is the coordinate of this texture:

    in vec2 texCoordOut;

    out vec4 color;

    uniform sampler2D image;

    void main()
    {
        color = texture(image, texCoordOut);
    }

Note that `image` isn't the direct texture id here. It's the id of the *texture unit* that will be sampled. In turn, textures aren't bound to programs directly; they are bound to texture units. This is achieved by first making the texture unit active with `glActiveTexture`, and then calling `glBindTexture` will affect this particular texture unit. However, since the default texture unit is texture unit `0`, programs using one texture can be made simpler omitting this call.


## Using PBOs
If you bind a buffer to `GL_PIXEL_UNPACK_BUFFER` then the `data` parameter in `glTexImage2D` is a offset into that buffer. 

This means that the glTexImage2D doesn't need to wait for all the data to be copied out of the application's memory before it can return, reducing overhead in the main thread.

    glGenBuffers(1, &pbo);
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);
    glBufferData(GL_PIXEL_UNPACK_BUFFER, width*height*3, NULL, GL_STREAM_DRAW);
    void* mappedBuffer = glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
    
    //write data into the mapped buffer, possibly in another thread.
    int width, height;
    unsigned char* image = SOIL_load_image("image.png", &width, &height, 0, SOIL_LOAD_RGB);
    memcpy(mappedBuffer, image, width*height*3);
    SOIL_free_image(image);

    // after reading is complete back on the main thread
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, pbo);
    glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, width, height, 0, GL_RGB, GL_UNSIGNED_BYTE, 0);

---

But where PBOs really shine is when you need to read the result of a render back into application memory. To read pixel data into a buffer bind it to `GL_PIXEL_PACK_BUFFER` then the data parameter of `glGetTexImage` will be an offset into that buffer:


    glGenBuffers(1, &pbo);
    glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo);
    glBufferData(GL_PIXEL_PACK_BUFFER, buf_size, NULL, GL_STREAM_COPY);
    
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);
    
    glGetTexImage(GL_TEXTURE_2D,
                  0,
                  GL_RGBA,
                  GL_UNSIGNED_BYTE,
                  null);
    //ensure we don't try and read data before the transfer is complete
    GLsync sync = glFenceSync(GL_SYNC_GPU_COMMANDS_COMPLETE, 0);

    // then regularly check for completion
    GLint result;
    glGetSynciv(sync, GL_SYNC_STATUS, sizeof(result), NULL, &result);
    if(result == GL_SIGNALED){
        glBindBuffer(GL_PIXEL_PACK_BUFFER, pbo);
        void* mappedBuffer = glMapBuffer(GL_PIXEL_PACK_BUFFER, GL_READ_ONLY);

        //now mapped buffer contains the pixel data

        glUnmapBuffer(GL_PIXEL_PACK_BUFFER);

    }



