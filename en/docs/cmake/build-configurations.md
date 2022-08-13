---
title: "Build Configurations"
slug: "build-configurations"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

This topic shows the uses of different CMake configurations like Debug or Release, in different environments.

## Setting a Release/Debug configuration
    CMAKE_MINIMUM_REQUIRED(VERSION 2.8.11)
    SET(PROJ_NAME "myproject")
    PROJECT(${PROJ_NAME})
    
    # Configuration types
    SET(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "Configs" FORCE)
    IF(DEFINED CMAKE_BUILD_TYPE AND CMAKE_VERSION VERSION_GREATER "2.8")
      SET_PROPERTY(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS  ${CMAKE_CONFIGURATION_TYPES})
    ENDIF()
    
    SET(${PROJ_NAME}_PATH_INSTALL     "/opt/project"                     CACHE PATH "This directory contains installation Path")
    SET(CMAKE_DEBUG_POSTFIX "d")

    # Install
    #---------------------------------------------------#
    INSTALL(TARGETS ${PROJ_NAME}
        DESTINATION  "${${PROJ_NAME}_PATH_INSTALL}/lib/${CMAKE_BUILD_TYPE}/"
        )

Performin the following builds will generate two different ('/opt/myproject/lib/Debug' '/opt/myproject/lib/Release') folders with the libraries:

    $ cd /myproject/build
    $ cmake -DCMAKE_BUILD_TYPE=Debug ..
    $ make
    $ sudo make install
    $ cmake _DCMAKE_BUILD_TYPE=Release ..
    $ make
    $ sudo make install

