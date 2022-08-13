---
title: "Build Targets"
slug: "build-targets"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

## Syntax
- add_executable(target_name [EXCLUDE_FROM_ALL] source1 [source2...])
- add_library(lib_name [STATIC|SHARED|MODULE] [EXCLUDE_FROM_ALL] source1 [source2 ...])

## Executables
To create a build target producing an executable, one should use the `add_executable` command:
```
add_executable(my_exe
               main.cpp
               utilities.cpp)
```
This creates a build target, e.g. `make my_exe` for GNU make, with the appropriate invocations of the configured compiler to produce an executable `my_exe` from the two source files `main.cpp` and `utilities.cpp`.

By default, all executable targets are added to the builtin `all` target (`all` for GNU make, `BUILD_ALL` for MSVC).  
To exclude an executable from being built with the default `all` target, one can add the optional parameter `EXCLUDE_FROM_ALL` right after the target name:
```
add_executable(my_optional_exe EXCLUDE_FROM_ALL main.cpp)
```

## Libraries
To create an build target that creates an library, use the `add_library` command:

```
add_library(my_lib lib.cpp)
```

The CMake variable `BUILD_SHARED_LIBS` controls whenever to build an static (`OFF`) or an shared (`ON`) library, using for example `cmake .. -DBUILD_SHARED_LIBS=ON`. However, you can explicitly set to build an shared or an static library by adding `STATIC` or `SHARED` after the target name:

```
add_library(my_shared_lib SHARED lib.cpp) # Builds an shared library
add_library(my_static_lib STATIC lib.cpp) # Builds an static library
```

The actual output file differs between systems. For example, an shared library on Unix systems is usually called `libmy_shared_library.so`, but on Windows it would be `my_shared_library.dll` and `my_shared_library.lib`.

Like `add_executable`, add `EXCLUDE_FROM_ALL` before the list of source files to exclude it from the `all` target:

```
add_library(my_lib EXCLUDE_FROM_ALL lib.cpp)
```

Libraries, that are designed to be loaded at runtime (for example plugins or applications using something like `dlopen`), should use `MODULE` instead of `SHARED`/`STATIC`:

```
add_library(my_module_lib MODULE lib.cpp)
```

For example, on Windows, there won't be a import (`.lib`) file, because the symbols are directly exported in the `.dll`.

