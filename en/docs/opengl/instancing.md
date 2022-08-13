---
title: "Instancing"
slug: "instancing"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

Instancing is a rendering technique that allows us to draw multiple copies of the same object in one draw call. It is usually used to render particles, foliage or large amounts of any other types of objects. 

## Instancing by Vertex Attribute Arrays
<!-- if version [gte 3.3] -->

Instancing can be done via modifications to how vertex attributes are provided to the vertex shader. This introduces a new way of accessing attribute arrays, allowing them to provide per-instance data that looks like a regular attribute.

A single instance represents one object or group of vertices (one grass leaf etc). Attributes associated with instanced arrays only advance between instances; unlike regular vertex attributes, they do not get a new value per-vertex.

To specify that an attribute array is instanced, use this call:
    
    glVertexAttribDivisor(attributeIndex, 1);

This sets vertex array object state. The "1" means that the attribute is advanced for each instance. Passing a 0 turns off instancing for the attribute.

In the shader, the instanced attribute looks like any other vertex attribute:

    in vec3 your_instanced_attribute;

To render multiple instances, you can invoke one of the `Instanced` forms of the value `glDraw*` calls. For example, this will draw 1000 instances, with each instance consisting of 3 vertices:

    glDrawArraysInstanced(GL_TRIANGLES, 0, 3, 1000); 


## Instanced Array Code

**Setting up VAOs, VBOs and the attributes:**

    // List of 10 triangle x-offsets (translations)
    GLfloat translations[10];
    GLint index = 0;
    for (GLint x = 0; x < 10; x++)
    {
        translations[index++] = (GLfloat)x / 10.0f;
    }

    // vertices
    GLfloat vertices[] = {
         0.0f,   0.05f,
         0.05f, -0.05f, 
        -0.05f, -0.05f,
         0.0f,  -0.1f,
    };

    // Setting VAOs and VBOs
    GLuint meshVAO, vertexVBO, instanceVBO;
    glGenVertexArrays(1, &meshVAO);    
    glGenBuffers(1, &instanceVBO);
    glGenBuffers(1, &vertexVBO);

    glBindVertexArray(meshVAO);
    
        glBindBuffer(GL_ARRAY_BUFFER, vertexVBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), (GLvoid*)0);

        glBindBuffer(GL_ARRAY_BUFFER, instanceVBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(translations), translations, GL_STATIC_DRAW);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, sizeof(GLfloat), (GLvoid*)0);
        glVertexAttribDivisor(1, 1); // This sets the vertex attribute to instanced attribute.

        glBindBuffer(GL_ARRAY_BUFFER, 0);

    glBindVertexArray(0);
    
**Draw call:**

    glBindVertexArray(meshVAO);
    glDrawArraysInstanced(GL_TRIANGLE_STRIP, 0, 4, 10); // 10 diamonds, 4 vertices per instance
    glBindVertexArray(0);

**Vertex shader:**

    #version 330 core
    layout(location = 0) in vec2 position;
    layout(location = 1) in float offset;

    void main()
    {
        gl_Position = vec4(position.x + offset, position.y, 0.0, 1.0);
    }

**Fragment shader:**

    #version 330 core
    layout(location = 0) out vec4 color;

    void main()
    {
        color = vec4(1.0, 1.0, 1.0, 1.0f);
    }

<!-- end version if -->

