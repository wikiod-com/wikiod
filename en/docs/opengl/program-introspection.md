---
title: "Program Introspection"
slug: "program-introspection"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

A number of features of program objects can be fetched through the program API.

## Vertex Attribute Information
Information about vertex attributes can be retrieved with the OGL function [glGetProgram](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml) and the parameters `GL_ACTIVE_ATTRIBUTES` and `GL_ACTIVE_ATTRIBUTE_MAX_LENGTH`.

The location of an active shader attribute can be determined by the OGL function [glGetAttribLocation](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetAttribLocation.xhtml), by the index of the attribute.

    GLuint shaderProg = ...;
    std::map< std::string, GLint > attributeLocation;

    GLint maxAttribLen, nAttribs;
    glGetProgramiv( shaderProg, GL_ACTIVE_ATTRIBUTES, &nAttribs );
    glGetProgramiv( shaderProg, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, &maxAttribLen 
    GLint written, size;
    GLenum type;
    std::vector< GLchar >attrName( maxAttribLen );
    for( int attribInx = 0; attribInx < nAttribs; attribInx++ )
    {
        glGetActiveAttrib( shaderProg, attribInx, maxAttribLen, &written, &size, &type, &attrName[0] );
        attributeLocation[attrName] = glGetAttribLocation( shaderProg, attrName.data() );
    }


## Uniform Information
Information about active uniforms in a program can be retrieved with the OGL function [glGetProgram](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetProgram.xhtml) and the parameters `GL_ACTIVE_UNIFORMS` and `GL_ACTIVE_UNIFORM_MAX_LENGTH`.

The location of an active shader uniform variable can be determined by the OGL function [glGetActiveUniform](https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glGetActiveUniform.xhtml), by the index of the attribute.

    GLuint shaderProg = ...;
    std::map< std::string, GLint > unifomLocation;

    GLint maxUniformLen, nUniforms;
    glGetProgramiv( shaderProg, GL_ACTIVE_UNIFORMS, &nUniforms );
    glGetProgramiv( shaderProg, GL_ACTIVE_UNIFORM_MAX_LENGTH, &maxUniformLen );

    GLint written, size;
    GLenum type;
    std::vector< GLchar >uniformName( maxUniformLen );
    for( int uniformInx = 0; uniformInx < nUniforms; uniformInx++ )
    {
        glGetActiveUniform( shaderProg, uniformInx, maxUniformLen, &written, &size, &type, &uniformName[0] );
        unifomLocation[uniformName] = glGetUniformLocation( shaderProg, uniformName.data() );
    }

