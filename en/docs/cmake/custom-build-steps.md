---
title: "Custom Build-Steps"
slug: "custom-build-steps"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Custom build steps are useful to run custom targets in your project build or for easily copying files so you don't  have to do it manually (maybe dlls?). Here I'll show you two examples, the first is for copying dlls (in particular Qt5 dlls) to your projects binary directory (either Debug or Release) and the second is for running a custom target (Doxygen in this case) in your solution (if you're using Visual Studio). 



As you can see, you can do a lot with custom build targets and steps in cmake, but you should be careful in using them, especially when copying dlls. While it is convinient to do so, it can at times result in what is affectionately called "dll hell". 

Basically this means you can get lost in which dlls your executable actually depends on, which ones its loading, and which ones it needs to run (maybe because of your computer's path variable).

Other than the above caveat, feel free to make custom targets do whatever you want! They're powerful and flexible and are an invaluable tool to any cmake project.

## Qt5 dll copy example
So let's say you have a project that depends on Qt5 and you need to copy the relevant dlls to your build directory and you don't want to do it manually; you can do the following:

    cmake_minimum_required(VERSION 3.0)
    project(MyQtProj LANGUAGES C CXX)
    find_package(Qt5 COMPONENTS Core Gui Widgets)
    #...set up your project
    
    # add the executable
    add_executable(MyQtProj ${PROJ_SOURCES} ${PROJ_HEADERS})
        
    add_custom_command(TARGET MyQtProj POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Core> $<TARGET_FILE_DIR:MyQtProj>
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Gui> $<TARGET_FILE_DIR:MyQtProj>
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Widgets> $<TARGET_FILE_DIR:MyQtProj>
        )

So now everytime you build your project, if the target dlls have changed that you want to copy, then they will be copied after your target (in this case the main executable) is built (notice the `copy_if_different` command); otherwise, they will not be copied. 

Additionally, note the use of [generator expressions][1] here. The advantage with using these is that you don't have to explicitly say where to copy dlls or which variants to use. To be able to use these though, the project you're using (Qt5 in this case) must have imported targets. 

If you're building in debug, then CMake knows (based on the imported target) to copy the Qt5Cored.dll, Qt5Guid.dll, and Qt5Widgetsd.dll to the Debug folder of you build folder. If you're building in release, then the release versions of the .dlls will be copied to the release folder. 


  [1]: https://cmake.org/cmake/help/v3.3/manual/cmake-generator-expressions.7.html "generator expressions"

## Running a Custom Target
You can also create a custom target to run when you want to perform a particular task. These are typically executables that you run to do different things. Something that may be of particular use is to run [Doxygen][1] to generate documentation for your project. To do this you can do the following in your `CMakeLists.txt` (for the sake of simplicity we'll contiue our Qt5 project example): 

    cmake_minimum_required(VERSION 3.0)
    project(MyQtProj LANGUAGES C CXX)
    find_package(Qt5 COMPONENTS Core Gui Widgets)
    #...set up your project
    
    add_executable(MyQtProj ${PROJ_SOURCES} ${PROJ_HEADERS})
        
    add_custom_command(TARGET MyQtProj POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Core> $<TARGET_FILE_DIR:MyQtProj>
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Gui> $<TARGET_FILE_DIR:MyQtProj>
        COMMAND ${CMAKE_COMMAND} -E copy_if_different $<TARGET_FILE:Qt5::Widgets> $<TARGET_FILE_DIR:MyQtProj>
        )
    
    #Add target to build documents from visual studio.
    set(DOXYGEN_INPUT ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile)
    #set the output directory of the documentation
    set(DOXYGEN_OUTPUT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/docs)
    # sanity check...
    message("Doxygen Output ${DOXYGEN_OUTPUT_DIR}")
    find_package(Doxygen)
    
    if(DOXYGEN_FOUND)
        # create the output directory where the documentation will live
        file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})
        # configure our Doxygen configuration file. This will be the input to the doxygen
        # executable
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
        ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    
    # now add the custom target. This will create a build target called 'DOCUMENTATION' 
    # in your project
    ADD_CUSTOM_TARGET(DOCUMENTATION
      COMMAND ${CMAKE_COMMAND} -E echo_append "Building API Documentation..."
      COMMAND ${CMAKE_COMMAND} -E make_directory ${DOXYGEN_OUTPUT_DIR}
      COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMAND ${CMAKE_COMMAND} -E echo "Done."
      WORKING_DIRECTORY ${DOXYGEN_OUTPUT_DIR})
    
    endif(DOXYGEN_FOUND)

Now when we create our solution (again assuming you're using Visual Studio), you'll have a build target called `DOCUMENTATION` that you can build to regenerate your project's documentation. 

  [1]: http://www.stack.nl/~dimitri/doxygen/ "Doxygen"

