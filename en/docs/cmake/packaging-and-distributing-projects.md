---
title: "Packaging and Distributing Projects"
slug: "packaging-and-distributing-projects"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

## Syntax
* #&nbsp;Package a build directory  
pack [PATH]
* #&nbsp;Use a specific generator  
cpack -G [GENERATOR] [PATH]
* #&nbsp;Provide optional overrides  
* cpack -G [GENERATOR] -C [CONFIGURATION] -P [PACKAGE NAME] -R [PACKAGE VERSION] -B [PACKAGE DIRECTORY] --vendor [PACKAGE VENDOR]

CPack is an external tool allowing the fast packaging of built CMake projects by gathering all the required data straight from the `CMakeLists.txt` files and the utilized installation commands like `install_targets()`.

For CPack to properly work, the `CMakeLists.txt` must include files or targets to be installed using the `install` build target.

A minimal script could look like this:

<!-- language: lang-cmake -->
    # Required headers
    cmake(3.0)

    # Basic project setup
    project(my-tool)

    # Define a buildable target
    add_executable(tool main.cpp)

    # Provide installation instructions
    install_targets(tool DESTINATION bin)

## Selecting a CPack Generator to be used
To create a package using a specific format, it is possible to pick the **Generator** to be used.

Similar to CMake this may be done using the **-G** argument:

<!-- language: none -->
    cpack -G 7Z .

Using this command line would package the built project in the current directory using the 7-Zip archive format.

At the time of writing, CPack version 3.5 supports the following generators by default:

* `7Z` 7-Zip file format (archive)
* `IFW` Qt Installer Framework (executable)
* `NSIS` Null Soft Installer (executable)
* `NSIS64` Null Soft Installer (64-bit, executable)
* `STGZ` Self extracting Tar GZip compression (archive)
* `TBZ2` Tar BZip2 compression (archive)
* `TGZ` Tar GZip compression (archive)
* `TXZ` Tar XZ compression (archive)
* `TZ` Tar Compress compression (archive)
* `WIX` MSI file format via WiX tools (executable archive)
* `ZIP` ZIP file format (archive)

If no explicit generator is provided, CPack will try to determine the best available depending on the actual environment. For example, it will prefer creating a self-extracting executable on Windows and only create a ZIP archive if no suitable toolset is found.

## Creating a package for a built CMake project
To create a redistributable package (e.g. a ZIP archive or setup program), it's usually enough to simply invoke CPack using a syntax very similar to calling CMake:

<!-- language: none -->
    cpack path/to/build/directory

Depending on the environment this will gather all required/installed files for the project and put them into a compressed archive or self-extracting installer.

