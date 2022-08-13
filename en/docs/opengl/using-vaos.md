---
title: "Using VAOs"
slug: "using-vaos"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

The Vertex Array Object stores how opengl should interpret a set of VBOs.

In essence it will let you avoid calling glVertexAttribPointer every time you want to render a new mesh.

If you don't want to deal with VAOs you can simply create one and bind it during program initialization and pretend they don't exist.

## Syntax
* void glEnableVertexAttribArray​(GLuint attribIndex);

* void glDisableVertexAttribArray​(GLuint attribIndex);

* void glVertexAttribPointer(GLuint attribIndex, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid * pointer);

* void glVertexAttribFormat(GLuint attribIndex, GLint size, GLenum type, GLboolean normalized, GLuint relativeoffset);

* void glVertexAttribBinding(GLuint attribIndex, GLuint bindingIndex);

* void glBindVertexBuffer(GLuint bindingIndex, GLuint buffer, GLintptr offset, GLintptr stride);


## Parameters
|parameter | Details |
| -----    | ------- |
| attribIndex | the location for the vertex attribute to which the vertex array will feed data |
|size| the number of components to be pulled from the attribute |
|type| The C++ type of the attribute data in the buffer |
|normalized| whether to map integer types to the floating-point range [0, 1] (for unsigned) or [-1, 1] (for signed) |
|pointer| the byte offset into the buffer to the first byte of the attribute's data (cast to `void*` for legacy reasons)|
| offset | the base byte offset from the beginning of the buffer to where the array data starts |
| relativeOffset | the offset to a particular attribute, relative to the base offset for the buffer | 
| stride | the number of bytes from one vertex's data to the next|
| buffer | the buffer object where the vertex arrays are stored|
| bindingIndex | the index to which the source buffer object will be bound |

The separate attribute format VAO setup can interoperate with `glVertexAttribPointer` (the latter is defined in terms of the former). But you must be careful when doing so.

The separate attribute format version have direct state access (DSA) equivalents in 4.5. These will have the same parameters but instead of using the bound VAO, the VAO being modified is passed explicitly. When using DSA de index buffer for `glDrawElements` can be set with `glVertexArrayElementBuffer(vao, ebo);`

## Version 4.3
<!-- if version [gte 4.3] -->

OpenGL 4.3 (or ARB_separate_attrib_format) adds an alternative way of specifying the vertex data, which creates a separation between the format of the data bound for an attribute and the buffer object source that provides the data. So instead of having a VAO per mesh, you may have a VAO per vertex format.

Each attribute is associated with a vertex format and a binding point. The vertex format consists of the type, component count, whether it is normalized, and the relative offset from the start of the data to that particular vertex. The binding point specifies which buffer an attribute takes its data from. By separating the two, you can bind buffers without respecifying any vertex formats. You can also change the buffer that provides data to multiple attributes with a single bind call.

    //accessible constant declarations
    constexpr int vertexBindingPoint = 0;
    constexpr int texBindingPoint = 1;// free to choose, must be less than the GL_MAX_VERTEX_ATTRIB_BINDINGS limit

    //during initialization
    glBindVertexArray(vao);

    glVertexAttribFormat(posAttrLoc, 3, GL_FLOAT, false, offsetof(Vertex, pos));
    // set the details of a single attribute
    glVertexAttribBinding(posAttrLoc, vertexBindingPoint);
    // which buffer binding point it is attached to
    glEnableVertexAttribArray(posAttrLoc);
    
    glVertexAttribFormat(normalAttrLoc, 3, GL_FLOAT, false, offsetof(Vertex, normal));
    glVertexAttribBinding(normalAttrLoc, vertexBindingPoint);
    glEnableVertexAttribArray(normalAttrLoc);
    
    glVertexAttribFormat(texAttrLoc, 2, GL_FLOAT, false, offsetof(Texture, tex));
    glVertexAttribBinding(texAttrLoc, texBindingPoint);
    glEnableVertexAttribArray(texAttrLoc);

Then during draw you keep the vao bound and only change the buffer bindings.

    void drawMesh(Mesh[] mesh){
        glBindVertexArray(vao);

        foreach(mesh in meshes){
            glBindVertexBuffer(vertexBindingPoint, mesh.vbo, mesh.vboOffset, sizeof(Vertex));
            glBindVertexBuffer(texBindingPoint, mesh.texVbo, mesh.texVboOffset, sizeof(Texture));
            // bind the buffers to the binding point

            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh.ebo);

            glDrawElements(GL_TRIANGLES, mesh.vertexCount, GL_UNSIGNED_INT, mesh.indexOffset);
            //draw
        }
    }

<!-- end version if -->

## Version 3.0
Each attribute is associated with a component count, type, normalized, offset, stride and VBO. The VBO is no passed explicitly as a parameter but is instead the buffer bound to GL_ARRAY_BUFFER at the time of the call.

    void prepareMeshForRender(Mesh mesh){
        glBindVertexArray(mesh.vao);
        glBindBuffer(GL_ARRAY_BUFFER, mesh.vbo);
        
        glVertexAttribPointer (posAttrLoc, 3, GL_FLOAT, false, sizeof(Vertex), mesh.vboOffset + offsetof(Vertex, pos));//will associate mesh.vbo with the posAttrLoc
        glEnableVertexAttribArray(posAttrLoc);
    
        glVertexAttribPointer (normalAttrLoc, 3, GL_FLOAT, false, sizeof(Vertex), mesh.vboOffset + offsetof(Vertex, normal));
        glEnableVertexAttribArray(normalAttrLoc);
    
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh.ebo); //this binding is also saved.
        glBindVertexArray(0);
    }

    void drawMesh(Mesh[] meshes){
        foreach(mesh in meshes){        
            glBindVertexArray(mesh.vao);
            glDrawElements(GL_TRIANGLES, mesh.vertexCount, GL_UNSIGNED_INT, mesh.indexOffset);
        }
    }

