---
title: "CMakeLists.txt for your Qt project"
slug: "cmakeliststxt-for-your-qt-project"
draft: false
images: []
weight: 9880
type: docs
toc: true
---

## CMakeLists.txt for Qt 5
A minimal CMake project file that uses Qt5 can be:

    cmake_minimum_required(VERSION 2.8.11)

    project(myproject)

    find_package(Qt5 5.7.0 REQUIRED COMPONENTS
        Core
    )

    set(CMAKE_AUTOMOC ON)

    add_executable(${PROJECT_NAME}
        main.cpp
    )

    target_link_libraries(${PROJECT_NAME}
        Qt5::Core
    )

`cmake_minimum_required` is called to set minimum required version for CMake. The minimum required version for this example to work is `2.8.11` -- previous versions of CMake need additional code for a target to use Qt.

`find_package` is called to search an installation of Qt5 with a given version -- 5.7.0 in the example -- and wanted components -- Core module in the example. For a list of available modules, see [Qt Documentation](http://doc.qt.io/qt-5/qtmodules.html). Qt5 is marked as `REQUIRED` in this project. The path to the installation can be hinted by setting the variable `Qt5_DIR`.

`AUTOMOC` is a boolean specifying whether CMake will handle the Qt `moc` preprocessor automatically, i.e. without having to use the `QT5_WRAP_CPP()` macro.

Other "AUTOMOC-like" variables are:

* `AUTOUIC`: a boolean specifying whether CMake will handle the Qt `uic` code generator automatically, i.e. without having to use the `QT5_WRAP_UI()` macro.

* `AUTORCC`: a boolean specifying whether CMake will handle the Qt `rcc` code generator automatically, i.e. without having to use the `QT5_ADD_RESOURCES()` macro.

`add_executable` is called to create an executable target from the given source files. The target is then linked to the listed Qt's modules with the command `target_link_libraries`. From CMake 2.8.11, `target_link_libraries` with Qt's imported targets handles linker parameters, as well as include directories and compiler options.

