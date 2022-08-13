---
title: "Setting up Eclipse for C++"
slug: "setting-up-eclipse-for-c++"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Linux + CMake ("Unix Makefiles" generator) + Qt (optional)
You should have a plain CMake project **myproject**, and we are going to make an Eclipse workspace outside of it:
<pre>
    <b>myproject/
      .git/
      CMakeLists.txt
      src/
        main.cpp</b>
    workspace/
      myproject/
        Release/
        Debug/
</pre>

Qt (optional)
---

* Get latest Eclipse CDT and then install the Qt package in it through "Help -> Install New Software".

Workspace
---

* Create an empty "workspace" directory alongside your CMake project source directory.
* Launch Eclipse and switch to that "workspace" directory.
* Create a C++ project (for Qt with Eclipse older than Neon: create "Qt Makefile Project" and then delete *.pro file, makefile and main.cpp from it)

Attaching Sources to the Project
---

* Go to Project Properties -> Paths and Symbols -> Source Location -> Link Folder.
* Check "Advanced" and link the source folder of CMake project like that: `../../myproject/src/`. It works because the workspace is just outside the CMake project directory.

CMake generator
---

* Create `Release` folder in the project.
* Go to "Make Target" view (<kbd>Ctrl</kbd>+<kbd>3</kbd> and then type "Make Target" if it's hard to find). "Make Target" view looks similarly to project view.
* Right click on the "Release" folder and then click "New...".
  * Uncheck "Same as target name".
  * Uncheck "Use builder settings".
  * Type in "Release" into "Target name" field.
  * Leave "Make target" empty.
  * Set "Build command" to something like `cmake ../../../myproject/`.
  * Click ok.
* Double click on this "Release" make target that was just created in the Release folder. That will run cmake generation.

Build
---

* Go to Project Properties and create a "Release" configuration.
* Make "Release" configuration active.
* For "Release" configuration uncheck "Generate Makefiles automatically".
* Set Build directory to "Release".
* Enable parallel build.

Now, you can build the project from Eclipse with a usual <kbd>Ctrl</kbd>+<kbd>b</kbd> "Build".

Re-running CMake (to re-generate the makefiles)
---
* Remove everything from the "Release" directory.
* Go to "Make Target" view.
* Double-click on the "Release" target.


