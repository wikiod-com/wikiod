---
title: "Shader Loading and Compilation"
slug: "shader-loading-and-compilation"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

These examples demonstrate various ways to load and compile shaders. All examples ***must include error handling code***.

Shader objects, as created from `glCreateShader` do not do much. They contain the compiled code for a single stage, but they do not even have to contain the *complete* compiled code for that stage. In many ways, they work like C and C++ object files.

Program objects contain the final linked program. But they also hold the state for the program's uniform values, as well as a number of other state data. They have APIs for introspecting the shader's interface data (though it only became comprehensive in GL 4.3). Program objects are what defines the shader code that you use when rendering.

Shader objects, once used to link a program, are no longer needed unless you intend to use them to link other programs.

## Individual Shader Object Compilation in C++
The traditional GLSL compilation model involves compiling code for a shader stage into a shader object, then linking multiple shader objects (covering all of the stages you want to use) into a single program object.

Since 4.2, program objects can be created that have only one shader stage. This method links all shader stages into a single program.

# Shader Object Compilation

    #include <string>
    #include <fstream>
    
    //In C++17, we could take a `std::filesystem::path` instead of a std::string
    //for the filename.
    GLuint CreateShaderObject(GLenum stage, const std::string &filename)
    {
        std::ifstream input(filename.c_str(), std::ios::in | std::ios::binary | std::ios::ate);
        
        //Figure out how big the file is.
        auto fileSize = input.tellg();
        input.seekg(0, ios::beg);
        
        //Read the whole file.
        std::string fileData(fileSize);
        input.read(&fileData[0], fileSize);
        input.close();
        
        //Create a shader name
        auto shader = glCreateShader(stage);
    
        //Send the shader source code to GL
        auto fileCstr = (const GLchar *)fileData.c_str();
        glShaderSource(shader, 1, &fileCstr, nullptr);
    
        //Compile the shader
        glCompileShader(shader);
    
        GLint isCompiled = 0;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &isCompiled);
        if(isCompiled == GL_FALSE)
        {
            GLint maxLength = 0;
            glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &maxLength);
    
            //C++11 does not permit you to overwrite the NUL terminator,
            //even if you are overwriting it with the NUL terminator.
            //C++17 does, so you could subtract 1 from the length and skip the `pop_back`.
            std::basic_string<GLchar> infoLog(maxLength);
            glGetShaderInfoLog(shader, maxLength, &maxLength, &infoLog[0]);
            infoLog.pop_back();
    
            //We don't need the shader anymore.
            glDeleteShader(shader);
    
            //Use the infoLog as you see fit.
            
            //Exit with failure.
            return 0;
        }
    
        return shader;
    }

# Program Object Linking

    #include <string>
    
    GLuint LinkProgramObject(vector<GLuint> shaders)
    {
        //Get a program object.
        auto program = glCreateProgram();
    
        //Attach our shaders to our program
        for(auto shader : shaders)
            glAttachShader(program, shader);
    
        //Link our program
        glLinkProgram(program);
    
        //Note the different functions here: glGetProgram* instead of glGetShader*.
        GLint isLinked = 0;
        glGetProgramiv(program, GL_LINK_STATUS, (int *)&isLinked);
        if(isLinked == GL_FALSE)
        {
            GLint maxLength = 0;
            glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);
    
            //C++11 does not permit you to overwrite the NUL terminator,
            //even if you are overwriting it with the NUL terminator.
            //C++17 does, so you could subtract 1 from the length and skip the `pop_back`.
            std::basic_string<GLchar> infoLog(maxLength);
            glGetProgramInfoLog(program, maxLength, &maxLength, &infoLog[0]);
            infoLog.pop_back();
            
            //We don't need the program anymore.
            glDeleteProgram(program);
    
            //Use the infoLog as you see fit.
            
            //Exit with failure
            return 0;
        }
    
        //Always detach shaders after a successful link.
        for(auto shader : shaders)
            gldetachShader(program, shader);
            
        return program;
    }

## Load Separable Shader in C++
<!-- if version [gte 4.1] -->

This code loads, compiles, and links a single file that creates a [separate shader program for a single stage][1]. If there are errors, it will get the info-log for those errors.

The code uses some commonly-available C++11 functionality.

    #include <string>
    #include <fstream>
    
    //In C++17, we could take a `std::filesystem::path` instead of a std::string
    //for the filename.
    GLuint CreateSeparateProgram(GLenum stage, const std::string &filename)
    {
        std::ifstream input(filename.c_str(), std::ios::in | std::ios::binary | std::ios::ate);
        
        //Figure out how big the file is.
        auto fileSize = input.tellg();
        input.seekg(0, ios::beg);
        
        //Read the whole file.
        std::string fileData(fileSize);
        input.read(&fileData[0], fileSize);
        input.close();
        
        //Compile&link the file
        auto fileCstr = (const GLchar *)fileData.c_str();
        auto program = glCreateShaderProgramv(stage, 1, &fileCstr);
    
        //Check for errors
        GLint isLinked = 0;
        glGetProgramiv(program, GL_LINK_STATUS, &isLinked);
        if(isLinked == GL_FALSE)
        {
            //Note: maxLength includes the NUL terminator.
            GLint maxLength = 0;
            glGetProgramiv(program, GL_INFO_LOG_LENGTH, &maxLength);        
    
            //C++11 does not permit you to overwrite the NUL terminator,
            //even if you are overwriting it with the NUL terminator.
            //C++17 does, so you could subtract 1 from the length and skip the `pop_back`.
            std::basic_string<GLchar> infoLog(maxLength);
            glGetProgramInfoLog(program, maxLength, &maxLength, &infoLog[0]);
            infoLog.pop_back();
    
            //The program is useless now. So delete it.
            glDeleteProgram(program);
    
            //Use the infoLog in whatever manner you deem best.

            //Exit with failure.
            return 0;
        }
        
        return program;
    }

<!-- end version if -->


  [1]: https://www.khronos.org/opengl/wiki/Shader_Compilation#Separate_programs

