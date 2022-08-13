---
title: "Create test suites with CTest"
slug: "create-test-suites-with-ctest"
draft: false
images: []
weight: 9969
type: docs
toc: true
---

## Basic Test Suite
    # the usual boilerplate setup
    cmake_minimum_required(2.8)
    project(my_test_project
            LANGUAGES CXX)
    
    # tell CMake to use CTest extension
    enable_testing()

    # create an executable, which instantiates a runner from
    # GoogleTest, Boost.Test, QtTest or whatever framework you use
    add_executable(my_test
                   test_main.cpp)

    # depending on the framework, you need to link to it
    target_link_libraries(my_test
                          gtest_main)

    # now register the executable with CTest
    add_test(NAME my_test COMMAND my_test)

The macro `enable_testing()` does a lot of magic. First and foremost, it creates a builtin target `test` (for GNU make; `RUN_TESTS` for VS), which, when run, executes _CTest_.

The call to `add_test()` finally registers an arbitrary executable with _CTest_, thus the executable gets run whenever we call the `test` target.

Now, build the project as usual and finally run the test target

| GNU Make | Visual Studio |
|---|---|
| `make test` | `cmake --build . --target RUN_TESTS` |

