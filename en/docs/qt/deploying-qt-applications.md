---
title: "Deploying Qt applications"
slug: "deploying-qt-applications"
draft: false
images: []
weight: 9962
type: docs
toc: true
---

## Deploying on windows
Qt provides a deployment tool for Windows: `windeployqt`. The tool inspects a Qt application executable for its dependecies to Qt modules and creates a deployment directory with the necessary Qt files to run the inspected executable. A possible script may look like:

    set PATH=%PATH%;<qt_install_prefix>/bin
    windeployqt --dir /path/to/deployment/dir /path/to/qt/application.exe

The `set` command is called to add Qt's `bin` directory to the `PATH` environment variable. `windeployqt` is then called:

* The path to the deployment directory is given an optional argument given with the parameter `--dir` (default is the path where `windeployqt` is called).
* The path to the executable to be inspected is given as last argument.

The deployment directory can then be bundled with the executable.

**NOTE:**

If you are using pre-compiled Qt5.7.0 with vs2013 on Windows *(not sure if all versions has this issue)*, there is a chance, that you need to manually copy `<QTDIR>\5.7\msvc2015\qml` to your bin directory of your program. Otherwise the program will auto quit after start.

See also [Qt documentation](http://doc.qt.io/qt-5/windows-deployment.html).

## Deploying on Mac
Qt offers a deployment tool for Mac: The Mac Deployment Tool.

The Mac deployment tool can be found in `QTDIR/bin/macdeployqt`. It is designed to automate the process of creating a deployable application bundle that contains the Qt libraries as private frameworks.

The mac deployment tool also deploys the Qt plugins, according to the following rules (unless <b>-no-plugins option is used</b>):

 - The platform plugin is always deployed.
 - Debug versions of the plugins are not deployed.
 - The designer plugins are not deployed.
 - The image format plugins are always deployed.
 - The print support plugin is always deployed.
 - SQL driver plugins are deployed if the application uses the Qt SQL module.
 - Script plugins are deployed if the application uses the Qt Script module.
 - The SVG icon plugin is deployed if the application uses the Qt SVG module.
 - The accessibility plugin is always deployed.

To include a 3rd party library in the application bundle, copy the library into the bundle manually, after the bundle is created.

To use `macdeployqt` tool you can open terminal and type:

    $ QTDIR/bin/macdeployqt <path to app file generated by build>/appFile.app

The app file will now contain all the Qt Libraries used as private frameworks.

`macdeployqt` also supports the following options

| Option | Description |
| ------ | ------ |
| -verbose=<0-3>   | 0 = no output, 1 = error/warning (default), 2 = normal, 3 = debug  |
|-no-plugins|Skip plugin deployment|
|-dmg|Create a .dmg disk image|
|-no-strip|Don't run 'strip' on the binaries|
|-use-debug-libs|Deploy with debug versions of frameworks and plugins (implies -no-strip)|
|-executable=<path>|Let the given executable also use the deployed frameworks|
|-qmldir=<path>|Deploy imports used by .qml files in the given path|

Detailed informations can be fount on [Qt Documentation][1]


  [1]: http://doc.qt.io/qt-5/osx-deployment.html

## Integrating with CMake
It is possible to run `windeployqt` and `macdeployqt` from CMake, but first the path to the executables must be found:

    # Retrieve the absolute path to qmake and then use that path to find
    # the binaries
    get_target_property(_qmake_executable Qt5::qmake IMPORTED_LOCATION)
    get_filename_component(_qt_bin_dir "${_qmake_executable}" DIRECTORY)
    find_program(WINDEPLOYQT_EXECUTABLE windeployqt HINTS "${_qt_bin_dir}")
    find_program(MACDEPLOYQT_EXECUTABLE macdeployqt HINTS "${_qt_bin_dir}")

In order for `windeployqt` to find the Qt libraries in their installed location, the folder must be added to `%PATH%`. To do this for a target named `myapp` after being built:

    add_custom_command(TARGET myapp POST_BUILD
        COMMAND "${CMAKE_COMMAND}" -E
            env PATH="${_qt_bin_dir}" "${WINDEPLOYQT_EXECUTABLE}"
                "$<TARGET_FILE:myapp>"
        COMMENT "Running windeployqt..."
    )

For running `macdeployqt` on a bundle, it would be done this way:

    add_custom_command(TARGET myapp POST_BUILD
        COMMAND "${MACDEPLOYQT_EXECUTABLE}"
            "$<TARGET_FILE_DIR:myapp>/../.."
            -always-overwrite
        COMMENT "Running macdeployqt..."
    )

## Deploying on linux
There is a a deployment tool for linux on [GitHub][1]. While not perfect, it is linked to from the Qt wiki. It's based conceptually on the Qt Mac Deployment Tool and functions similarly by providing an [AppImage][2].

Given that a desktop file should be provided with an AppImage, `linuxdeployqt` can use that to determine the parameters of the build.

    linuxdeployqt ./path/to/appdir/usr/share/application_name.desktop

Where the [desktop file][3] specifies the executable to be run (with `EXEC=`), the name of the application, and an icon.

  [1]: https://github.com/probonopd/linuxdeployqt
  [2]: http://appimage.org/
  [3]: https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html

