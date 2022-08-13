---
title: "Configure file"
slug: "configure-file"
draft: false
images: []
weight: 9767
type: docs
toc: true
---

configure_file is a CMake function for copying a file to another location and modify its contents.

This function is very useful for generating configuration files with paths, custom variables, using a generic template.

 

Copy a file to another location and modify its contents.

    configure_file(<input> <output>
               [COPYONLY] [ESCAPE_QUOTES] [@ONLY]
               [NEWLINE_STYLE [UNIX|DOS|WIN32|LF|CRLF] ])

Copies a file <input> to file <output> and substitutes variable values referenced in the file content. If <input> is a relative path it is evaluated with respect to the current source directory. The <input> must be a file, not a directory. If <output> is a relative path it is evaluated with respect to the current binary directory. If <output> names an existing directory the input file is placed in that directory with its original name.

If the <input> file is modified the build system will re-run CMake to re-configure the file and generate the build system again.

This command replaces any variables in the input file referenced as ${VAR} or @VAR@ with their values as determined by CMake. If a variable is not defined, it will be replaced with nothing. If COPYONLY is specified, then no variable expansion will take place. If ESCAPE_QUOTES is specified then any substituted quotes will be C-style escaped. The file will be configured with the current values of CMake variables. If @ONLY is specified, only variables of the form @VAR@ will be replaced and ${VAR} will be ignored. This is useful for configuring scripts that use ${VAR}.

Input file lines of the form “#cmakedefine VAR ...” will be replaced with either “#define VAR ...” or /* #undef VAR */ depending on whether VAR is set in CMake to any value not considered a false constant by the if() command. (Content of ”...”, if any, is processed as above.) Input file lines of the form “#cmakedefine01 VAR” will be replaced with either “#define VAR 1” or “#define VAR 0” similarly.

With NEWLINE_STYLE the line ending could be adjusted:

    'UNIX' or 'LF' for \n, 'DOS', 'WIN32' or 'CRLF' for \r\n.

COPYONLY must not be used with NEWLINE_STYLE.



## Generate a c++ configure file with CMake
If we have a c++ project that uses a config.h configuration file with some custom paths or variables, we can generate it using CMake and a generic file config.h.in.

The config.h.in can be part of a git repository, while the generated file config.h will never be added, as it is generated from the current environment.

    #CMakeLists.txt
    CMAKE_MINIMUM_REQUIRED(VERSION 2.8.11)

    SET(PROJ_NAME "myproject")
    PROJECT(${PROJ_NAME})

    SET(${PROJ_NAME}_DATA     ""     CACHE PATH "This directory contains all DATA and RESOURCES")
    SET(THIRDPARTIES_PATH    ${CMAKE_CURRENT_SOURCE_DIR}/../thirdparties      CACHE PATH "This directory contains thirdparties")

    configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/common/config.h.in"
                "${CMAKE_CURRENT_SOURCE_DIR}/include/config.h" )

If we have a config.h.in like this:

    cmakedefine PATH_DATA "@myproject_DATA@"
    cmakedefine THIRDPARTIES_PATH "@THIRDPARTIES_PATH@"

The previous CMakeLists will generate a c++ header like this:

    #define PATH_DATA "/home/user/projects/myproject/data"
    #define THIRDPARTIES_PATH "/home/user/projects/myproject/thirdparties"

## Examble based on SDL2 control version
If you have a `cmake` module . You can create a folder called `in` to store all config files. 

For example,you have a project called `FOO`, you can create a `FOO_config.h.in` file like:

    //===================================================================================
    //  CMake configuration file, based on SDL 2 version header
    // ===================================================================================
    
    #pragma once
    
    #include <string>
    #include <sstream>
    
    namespace yournamespace
    {
      /**
     *  \brief Information the version of FOO_PROJECT in use.
     *
     *  Represents the library's version as three levels: major revision
     *  (increments with massive changes, additions, and enhancements),
     *  minor revision (increments with backwards-compatible changes to the
     *  major revision), and patchlevel (increments with fixes to the minor
     *  revision).
     *
     *  \sa FOO_VERSION
     *  \sa FOO_GetVersion
     */
    typedef struct FOO_version
    {
        int major;        /**< major version */
        int minor;        /**< minor version */
        int patch;        /**< update version */
    } FOO_version;
    
    /* Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL
    */
    #define FOO_MAJOR_VERSION   0
    #define FOO_MINOR_VERSION   1
    #define FOO_PATCHLEVEL      0
    
    /**
     *  \brief Macro to determine FOO version program was compiled against.
     *
     *  This macro fills in a FOO_version structure with the version of the
     *  library you compiled against. This is determined by what header the
     *  compiler uses. Note that if you dynamically linked the library, you might
     *  have a slightly newer or older version at runtime. That version can be
     *  determined with GUCpp_GetVersion(), which, unlike GUCpp_VERSION(),
     *  is not a macro.
     *
     *  \param x A pointer to a FOO_version struct to initialize.
     *
     *  \sa FOO_version
     *  \sa FOO_GetVersion
     */
    #define FOO_VERSION(x)                          \
    {                                   \
        (x)->major = FOO_MAJOR_VERSION;                 \
        (x)->minor = FOO_MINOR_VERSION;                 \
        (x)->patch = FOO_PATCHLEVEL;                    \
    }
    
    /**
     *  This macro turns the version numbers into a numeric value:
     *  \verbatim
        (1,2,3) -> (1203)
        \endverbatim
     *
     *  This assumes that there will never be more than 100 patchlevels.
     */
    #define FOO_VERSIONNUM(X, Y, Z)                     \
        ((X)*1000 + (Y)*100 + (Z))
    
    /**
     *  This is the version number macro for the current GUCpp version.
     */
    #define FOO_COMPILEDVERSION \
        FOO_VERSIONNUM(FOO_MAJOR_VERSION, FOO_MINOR_VERSION, FOO_PATCHLEVEL)
    
    /**
     *  This macro will evaluate to true if compiled with FOO at least X.Y.Z.
     */
    #define FOO_VERSION_ATLEAST(X, Y, Z) \
        (FOO_COMPILEDVERSION >= FOO_VERSIONNUM(X, Y, Z))
    
    }
    
    // Paths
    #cmakedefine FOO_PATH_MAIN "@FOO_PATH_MAIN@"

This file will create a `FOO_config.h` in the install path, with a variable defined in c `FOO_PATH_MAIN` from cmake variable. To generate it you need to include `in` file in your CMakeLists.txt,like this (set paths and variables):

    MESSAGE("Configuring FOO_config.h ...")
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/common/in/FOO_config.h.in"
    "${FOO_PATH_INSTALL}/common/include/FOO_config.h" )

That file will contain the data from template, and variable with your real path, for example:

    // Paths
    #define FOO_PATH_MAIN "/home/YOUR_USER/Respositories/git/foo_project"



