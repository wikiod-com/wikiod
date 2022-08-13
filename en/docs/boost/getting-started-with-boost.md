---
title: "Getting started with boost"
slug: "getting-started-with-boost"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation or Setup
See [Boost Getting Started][1].

Most of the Boost libraries are header-only, meaning that there's nothing you have to compile or link to.

Make sure you are getting the most recent version of Boost:
1. Visit [www.boost.org][2]
2. Look for the Current Release download. Currently, this links [here][3].
   [![Shows where on the boost website you can download Boost][4]][4]
3. Select the appropriate archive file for your operating system, and download.

Header-only libraries can then be used by simply including the respective header files.

A few Boost libraries require compilation:

* Boost.Chrono
* Boost.Context
* Boost.Filesystem
* Boost.GraphParallel
* Boost.IOStreams
* Boost.Locale
* Boost.MPI
* Boost.ProgramOptions
* Boost.Python
* Boost.Regex
* Boost.Serialization
* Boost.Signals
* Boost.System
* Boost.Thread
* Boost.Timer
* Boost.Wave

Also, the following libraries have components which must be compiled:

* Boost.DateTime
* Boost.Graph
* Boost.Math
* Boost.Random
* Boost.Test
* Boost.Exception

  [1]: http://www.boost.org/doc/libs/1_61_0/more/getting_started/index.html
  [2]: http://www.boost.org
  [3]: https://sourceforge.net/projects/boost/files/boost/1.61.0/
  [4]: http://i.stack.imgur.com/90rM7.png

The source for Boost can be obtained through the [download link on the site](http://www.boost.org/users/download/), which will re-direct to its [SourceForge page](https://sourceforge.net/projects/boost/files/boost/) for the latest version ([1.61.0](https://sourceforge.net/projects/boost/files/boost/) as of July 2016). This can be unzipped (or un-tared, etc) to a directory (such as C:\local\boost_1_61_0). This directory can then be added to the include path for the software you are building. After this, you can include Boost headers in C++ files with `#include <boost/header/path.hpp>`. 

The majority of the libraries in Boost are header-only. If you only need these then the above source distribution is all that is needed. However, if you need to use one of the libraries that requires a compiled binary to be built, you will need that as well.

On any system, the most reliable way to get the correct binaries is to build them yourself. These directions are somewhat different for [Windows](http://www.boost.org/doc/libs/1_60_0/more/getting_started/windows.html#prepare-to-use-a-boost-library-binary) or [Linux/Unix/POSIX](http://www.boost.org/doc/libs/1_61_0/more/getting_started/unix-variants.html#or-build-custom-binaries).

On Windows with Visual Studio, an alternative to building the libraries yourself is to download pre-built libraries from [Boost's SourceForge page](https://sourceforge.net/projects/boost/files/boost-binaries/) ([1.61.0](https://sourceforge.net/projects/boost/files/boost-binaries/1.61.0/) as of July 2016). On that page you can select an installer that will install a version for a specific Visual Studio build or the 7-zip file (boost_X_XX_X-bin-all-32-64.7z) that contains the binaries for all the supported Visual Studio versions. Either of these options includes the source/headers as well as the binaries, so there is no need to have downloaded the source distribution above. Once you have it, extract/install to a directory (such as C:\local\boost_1_61_0) and add that directory to your include path, then add the directory containing the binaries that correspond to your version of Visual Studio (e.g. C:\local\boost_1_61_0\lib32-msvc-12.0 for Visual Studio 2013 32-bit projects) to the library path.

## Installing and Running Boost (Cygwin)
(Beginner level; IDE: CLion) 

First, install boost from the Cygwin mirror: open the install exe, search for boost, install the packages. 

___

After boost is installed: it will be located in `/usr/include/boost`.  This is where everything is.  All `#include` statements will be a path from the boost folder, as in: `#include <boost/archive/text_oarchive.hpp>`. 

___

Once you include the boost files of your choice in your `.cpp` files, your code will still not compile in your IDE of choice until you **link** [the libraries][1] and tell [cmake to search for your downloaded boost code][2]. 

___

In order to get cmake to search for your boost code, 

    find_package(Boost 1.60.0 COMPONENTS components_you_want)

    # for example: 
    find_package(Boost 1.60.0 COMPONENTS serialization)

Then, include the directories:   `include_directories(${Boost_INCLUDE_DIRS})`

Finally, add your executable and link the libraries: 

    add_executable(your_target ${SOURCE_FILES})
    target_link_libraries(your_target ${Boost_LIBRARIES} -any_missing_boostlibs)


____
Before starting your program, avoid an error dump by testing to see if boost has been found before including anything or running your code: 

    if (Boost_FOUND)
        include_directories(${Boost_INCLUDE_DIRS})
        add_executable(YourTarget ${SOURCE_FILES})
        target_link_libraries(your_target ${Boost_LIBRARIES} -missing_libs)        
    endif()

I included `-missing_libs` because an error you may run into is that some boost library or another might not have been linked, and you must manually add it--for instance, the [link][1] I referenced earlier. 
___


All together, a final CMakeLists.txt file might look something like: 

    cmake_minimum_required(VERSION 3.7)
    project(your_project)
    
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    set(SOURCE_FILES main.cpp tmap.cpp tmap.h)
    find_package(Boost 1.60.0 COMPONENTS serialization)
    
    if(Boost_FOUND)
        include_directories(${Boost_INCLUDE_DIRS})
        add_executable(your_project ${SOURCE_FILES})
        target_link_libraries(your_project ${Boost_LIBRARIES})
    endif()


  [1]: http://stackoverflow.com/questions/13467072/c-boost-undefined-reference-to-boostsystemgeneric-category#13468280
  [2]: http://stackoverflow.com/questions/3808775/cmake-doesnt-find-boost

