---
title: "Test and Debug"
slug: "test-and-debug"
draft: false
images: []
weight: 9941
type: docs
toc: true
---

## General approach to debug when building with Make
Suppose the `make` fails:

    $ make

Launch it instead with `make VERBOSE=1` to see the commands executed. Then directly run the linker or compiler command that you'll see. Try to make it work by adding necessary flags or libraries.

Then figure out what to change, so CMake itself can pass correct arguments to the compiler/linker command:
* what to change in the system (what libraries to install, which versions, versions of CMake itself)
* if previous fails, what environment variables to set or parameters to pass to CMake
* otherwise, what to change in the `CMakeLists.txt` of the project or the library detection scripts like `FindSomeLib.cmake`

To help in that, add `message(${MY_VARIABLE})` calls into `CMakeLists.txt` or `*.cmake` to debug variables that you want to inspect.

## Debug find_package() errors
***Note:*** The shown CMake error messages already include the fix for "non-standard" library/tool installation paths. The following examples just demonstrate more verbose CMake `find_package()` outputs.

# CMake internally supported Package/Module #

If the following code (replace the [`FindBoost`](https://cmake.org/cmake/help/v3.0/module/FindBoost.html) module with [your module in question](https://cmake.org/cmake/help/latest/manual/cmake-modules.7.html))

    cmake_minimum_required(VERSION 2.8)
    project(FindPackageTest)

    find_package(Boost REQUIRED)

gives some error like

    CMake Error at [...]/Modules/FindBoost.cmake:1753 (message):
      Unable to find the requested Boost libraries.

      Unable to find the Boost header files.  Please set BOOST_ROOT to the root
      directory containing Boost or BOOST_INCLUDEDIR to the directory containing
      Boost's headers.

And you're wondering where it tried to find the library, you can check if your package has an `_DEBUG` option like the `Boost` module has for getting more verbose output

    $ cmake -D Boost_DEBUG=ON .. 

# CMake enabled Package/Library #

If the following code (replace the `Xyz` with [your library in question](https://cmake.org/Wiki/CMake:How_To_Find_Libraries))

    cmake_minimum_required(VERSION 2.8)
    project(FindPackageTest)

    find_package(Xyz REQUIRED)

gives the some error like

    CMake Error at CMakeLists.txt:4 (find_package):
      By not providing "FindXyz.cmake" in CMAKE_MODULE_PATH this project has
      asked CMake to find a package configuration file provided by "Xyz", but
      CMake did not find one.

      Could not find a package configuration file provided by "Xyz" with any of
      the following names:

        XyzConfig.cmake
        xyz-config.cmake

      Add the installation prefix of "Xyz" to CMAKE_PREFIX_PATH or set "Xyz_DIR"
      to a directory containing one of the above files.  If "Xyz" provides a
      separate development package or SDK, be sure it has been installed.

And you're wondering where it tried to find the library, you can use the undocumented `CMAKE_FIND_DEBUG_MODE` global variable for getting a more verbose output

    $ cmake -D CMAKE_FIND_DEBUG_MODE=ON ..

## Let CMake create verbose Makefiles
Once a CMake project is initialized via `project()`, the output verbosity of the resulting build script can be adjusted via:

    CMAKE_VERBOSE_MAKEFILE

This variable can be set via CMake's command line when configuring a project:

    cmake -DCMAKE_VERBOSE_MAKEFILE=ON <PATH_TO_PROJECT_ROOT>

For GNU make this variable has the same effect as running `make VERBOSE=1`.

