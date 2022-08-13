---
title: "Compile features and CC++ standard selection"
slug: "compile-features-and-cc++-standard-selection"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Syntax
* target_compile_features(*target* **PRIVATE|PUBLIC|INTERFACE** *feature1* [*feature2* ...])


## Compile Feature Requirements
Required compiler features can be specified on a target using the command [target_compile_features][1]:

    add_library(foo
        foo.cpp
    )
    target_compile_features(foo
        PRIVATE          # scope of the feature
        cxx_constexpr    # list of features
    )

The features must be part of [CMAKE_C_COMPILE_FEATURES][2] or [CMAKE_CXX_COMPILE_FEATURES][3]; cmake reports an error otherwise. Cmake will add any necessary flags such as `-std=gnu++11` to the compile options of the target.

In the example, the features are declared `PRIVATE`: the requirements will be added to the target, but not to its consumers. To automatically add the requirements to a target building against foo, `PUBLIC` or `INTERFACE` should be used instead of `PRIVATE`:

    target_compile_features(foo
        PUBLIC    # this time, required as public
        cxx_constexpr
    )

    add_executable(bar
        main.cpp
    )
    target_link_libraries(bar
        foo       # foo's public requirements and compile flags are added to bar
    )


  [1]: https://cmake.org/cmake/help/v3.1/command/target_compile_features.html
  [2]: https://cmake.org/cmake/help/v3.1/variable/CMAKE_C_COMPILE_FEATURES.html#variable:CMAKE_C_COMPILE_FEATURES
  [3]: https://cmake.org/cmake/help/v3.1/variable/CMAKE_CXX_COMPILE_FEATURES.html#variable:CMAKE_CXX_COMPILE_FEATURES

## C/C++ version selection
Wanted version for C and C++ can be specified globally using respectively variables [`CMAKE_C_STANDARD`][2] (accepted values are 98, 99 and 11) and [`CMAKE_CXX_STANDARD`][1] (accepted values are 98, 11 and 14):

    set(CMAKE_C_STANDARD 99)
    set(CMAKE_CXX_STANDARD 11)

These will add the needed compile options on targets (e.g. `-std=c++11` for gcc).

The version can be made a requirement by setting to `ON` the variables `CMAKE_C_STANDARD_REQUIRED` and `CMAKE_CXX_STANDARD_REQUIRED` respectively.

The variables must be set before target creation. The version can also be specified per-target:

    set_target_properties(foo PROPERTIES
        CXX_STANDARD 11
        CXX_STANDARD_REQUIRED ON
    )

[1]:https://cmake.org/cmake/help/latest/prop_tgt/CXX_STANDARD.html
[2]:https://cmake.org/cmake/help/latest/prop_tgt/C_STANDARD.html

