---
title: "Compile SFML for Android on Windows"
slug: "compile-sfml-for-android-on-windows"
draft: false
images: []
weight: 9960
type: docs
toc: true
---

## 4. Build the SFML Android Sample
You can find the Android Sample in `[SFML_ROOT]\examples\android`\
You can copy it to leave the SFML repository in it's original state. Open cmd.exe in the sample location.\
To get a list of all available Android build targets:

    android list target

---

Run Update Project for the Sample:

    android update project --path [Path/to/Android/Sample] --target [targetID]
e.g.

    android update project --path "" --target android-19

For path we can use `""` because we are running cmd in the sample path already.

---

To compile use this command:

    ndk-build

---

Create the debug (or release) apk:

    ant debug
    ant release

---
Or use this command to directly install it on a device:

    ant debug install


## 1. Get the Tools
This are the tools you need to build SFML for Android on a Windows Machine
- CMake
- Git
- Android SDK
- Android NDK
- Apache Ant
- MinGW (msys basic)
- Java jre
- Java jdk
- Android USB Driver (Download: http://adbdriver.com/ )

Make sure you've installed all tools (Tools -> Android SDK Tools / Platform-tools / Build-tools) in the Android SDK Manager.


----------


If you have installed Visual Studio 2015 you might got some tools from above alerady. If so here are the default directories Visual Studio will put them:
- Android NDK: `C:\ProgramData\Microsoft\AndroidNDK  (or AndroidNDK64)`
- Android SDK: `C:\Program Files (x86)\Android\android-sdk`
- Apache Ant:  `C:\Program Files (x86)\Microsoft Visual Studio 14.0\Apps`
- Java SE jdk: `C:\Program Files (x86)\Java`
- Git:         `C:\Program Files\Git`

## 2. Adjust your enviroment variables
<h2>Add following Paths to the PATH-Enviromentvariable</h2>

- `[Path to CMake]\bin`
- `[Path to Git]\bin`
- `[Path to SDK]\tools`
- `[Path to SDK]\platform-tools`
- `[Path to NDK]`
- `[Path to ANT]\bin`
- `[Path to MinGW]\bin`
- `[Path to MinGW]\msys\1.0\bin`
- `[Path to Java jre]\bin`
- `[Path to Java jdk]\bin`

Make sure you use backslashes(`\`) and seperate the paths with semicolons (`;`)!


----------


<h2>Add two new enviroment variables</h2>
Name: `ANDROID_NDK` \
Value: `[Path/to/NDK]` \
(e.g. `C:/Android/NDK` ) \
Make sure you use forwardslashes(`/`)!

Name: `JAVA_HOME`\
Value: `[PATH\to\jdk]` \
(e.g. `C:\Program Files (x86)\Java\jdk1.7.0_55` ) \
Make sure you use backslashes(`\`)!

## 3. Compiling SFML
Clone the SFML Repository from Github.
--------------------------------------------------------------------
Enter following comands in a cmd window:

    git clone https://github.com/SFML/SFML.git SFML

If you already downloaded SFML before you can just use the existing one.

---

Create some folders for the build-files
--------------------------------------------

    cd SFML
    mkdir build && cd build
    mkdir armeabi-v7a && cd armeabi-v7a

---

Generate MSYS Makefiles for armeabi-v7a with cmake
--------------------------------------------------

    cmake -DANDROID_ABI=armeabi-v7a -DCMAKE_TOOLCHAIN_FILE=../../cmake/toolchains/android.toolchain.cmake ../.. -G "MSYS Makefiles"

You can exchange `armeabi-v7a` with other architectures as you like.

---

Compile SFML from the generated makefiles and install it to `$(NDK)/sources` folder.
------------------------------------------------------------------------
For this action you probably need administrator privileges. (Run cmd.exe as admin)

    make && make install

You can use `make install` for multiple architectures. It all uses one sfml tag in the `$(NDK)/sources` folder.


