---
title: "Getting started with maya"
slug: "getting-started-with-maya"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Maya supports 3 main programming environments.  Each has different setup requirements.

MEL
----

**MEL** scripting language is included with the Maya application. Enabled by default, users can test MEL in the script listener window in a running copy of Maya.  

MEL files are text files with the extension `.mel`. They can be loaded into a running Maya session using the `source` command in the listener or in another MEL script. Maya maintains a list of source directories and will search for a requested MEL script in all directories until it finds an appropriately named file.  

There are number of methods for setting up the script path; see the [Autodesk documentation](https://knowledge.autodesk.com/support/maya/learn-explore/caas/CloudHelp/cloudhelp/2017/ENU/Maya/files/GUID-228CCA33-4AFE-4380-8C3D-18D23F7EAC72-htm.html) for more details.

Python
----
Maya includes an embedded **Python** intepreter. MEL commands are available from Python in the `maya.cmds` Python module, so a command like `polyCube -n "new_cube"` is available in Python as `maya.cmds.polyCube(n='new_cube')`. The listener window includes a Python tab which allows users to enter Python commands interactively.

Maya python can import modules using the python `import` directive. Maya will look for Python files in a number of locations, configured in the Maya application, using environment variable or a`maya.env` file. The  [Autodesk documentation](https://knowledge.autodesk.com/support/maya/learn-explore/caas/CloudHelp/cloudhelp/2017/ENU/Maya/files/GUID-228CCA33-4AFE-4380-8C3D-18D23F7EAC72-htm.html) covers the basics of placing python files where Maya can see and import them.

C++
----
Maya exposes its API to **C++**. Developers can compile plugins which Maya will recognize at startup.  

Developing C++ plugins for Maya requires the [Maya Devkit](https://apps.autodesk.com/MAYA/en/Detail/Index?id=6303159649350432165). Download the version appropriate to your platform and follow the included instructions to set up the build environment.  


## Simple Python example
Open the Maya listener with the button at the lower right corner of the help line.  This opens the script listener.

Create a `Python` tab from the tab bar.

Here's a very basic script that will print out the positions of the cameras in a default scene.  Enter this into the listener:

    import maya.cmds as cmds          
    cameras = cmds.ls(type ='camera')  
    for each_camera in cameras:
        parent = cmds.listRelatives(each_camera, parent=True)
        position = cmds.xform(parent, q=True, translation=True)
        print each_camera, "is at", position

Select the script an execute it with `CTRL+enter`;

Here's another simple example that generates a random collection of cubes. It uses the python `random` module to generate random values.

    import maya.cmds as cmds
    import random
             
    for n in range(25):
        cube, cubeShape = cmds.polyCube()
        x = random.randrange(-50, 50)
        y = random.randrange(-50, 50)
        z = random.randrange(-50, 50)
        cmds.xform(cube, t = (x,y,z))

## Hello world
Printing "hello world" on several languages on Maya on the console (Script Editor).

**MEL**

On a MEL tab on the Script Editor, or the command line bar, selecting MEL:

    print ("hello world");

And hit play on the script editor or enter key on the command line.

**PYTHON**

On a Python tab on the Script Editor, or the command line bar, selecting Python:

    print "hello world"

And hit play on the script editor or enter key on the command line.

