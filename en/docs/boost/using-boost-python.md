---
title: "Using boost.python"
slug: "using-boostpython"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Introductory Example on Boost.Python
Things are easy when you have to use a C++ library in a Python project. Just you can use Boost.

First of all here is a list of components you need:

 - A CMakeList.txt file, because you're going to use CMake.
 - The C++ files of the C++ project.
 - The python file - this is your python project.

Let's start with a small C++ file. Our C++ project has only one method which returns some string "This is the first try". Call it *CppProject.cpp*

    char const *firstMethod() {
        return "This is the first try.";
    }

    BOOST_PYTHON_MODULE(CppProject) {
        boost::python::def("getTryString", firstMethod); // boost::python is the namespace
    }

Have a CMakeLists.txt file a below:

    cmake_minimum_required(VERSION 2.8.3)
    FIND_PACKAGE(PythonInterp)
    FIND_PACKAGE(PythonLibs)
    FIND_PACKAGE(Boost COMPONENTS python)

    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIRS} ${PYTHON_INCLUDE_DIRS})

    PYTHON_ADD_MODULE(NativeLib CppProject)
    FILE(COPY MyProject.py DESTINATION .) # See the whole tutorial to understand this line

By this part of the tutorial everything is so easy. you can import the library and call method in your python project. Call your python project *MyProject.py*.

    import NativeLib
    print (NativeLib.getTryString)


----------

In order to run your project follow the instructions below:

 - Create a directory with the name *build*.
 - Enter into that directory.
 - Give the command `cmake -DCMAKE_BUILD_TYPE=Release ..`
 - `make`
 - `python MyProject.py`. Now, you have to see the string which the method in your C++ project returns.

## Wrapping std::vector in boost.python
If a function returns a `std::vector` type, and it is exposed to Python directly like

    std::vector<float> secondMethod() {
        return std::vector<float>();
    }
    
    BOOST_PYTHON_MODULE(CppProject) {
        boost::python::def("getEmptyVec", secondMethod);
    }

then when the functions gets called Python will tell you `No to_python (by-value) converter found for C++ type: std::vector<float, std::allocator<float> >`, because Python needs to know how to deal with `std::vector`. 

Fortunately boost.python has provided a wrapper funciton for us in [`vector_indexing_suite.hpp`][1]. The returning value can be handled as a `FloatVec` object whose element can be accessed by the `[]` operator, by exposing the corresponding wrapper function as following.

    std::vector<float> secondMethod() {
        return std::vector<float>();
    }
    
    BOOST_PYTHON_MODULE(CppProject) {
        // wrapper function
        class_<std::vector<float> >("FloatVec")
            .def(vector_indexing_suite<std::vector<float> >());
        boost::python::def("getEmptyVec", secondMethod);
    }

The result can be further converted into a Python list or Numpy array simply by calling `list()` and `numpy.asarray()`.

  [1]: http://www.boost.org/doc/libs/1_39_0/libs/python/doc/v2/indexing.html

