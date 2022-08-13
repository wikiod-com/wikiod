---
title: "Multiple projects set up"
slug: "multiple-projects-set-up"
draft: false
images: []
weight: 9974
type: docs
toc: true
---

Unit tests project set up currently can be found [here][1]


  [1]: https://docs.microsoft.com/en-us/dotnet/articles/core/tutorials/using-on-macos

## Referencing local projects
There is no such things as `.sln` and `.proj` files.  
Instead of them **folders** are being used in Visual Studio Code.  
Each project folder should have a seperate `project.json` file.

    /MyProject.Core
       SourceFile.cs
       project.json

    /MyProject.Web
       /Controllers
       /Views
       project.json

To reference `MyProject.Core` from `MyProject.Web` project edit `MyProject.Web\project.json` file and add the dependency:

    // MyProject.Web/project.json
    {
       "dependencies": {
          "MyProject.Core": {"target": "project"},
        ...
       }
       "buildOptions": {
          "emitEntryPoint": true 
       }
    }

The line `"emitEntryPoint": true` says that `MyProject.Web` is a start project for the solution. 
`MyProject.Core` should have this flag disabled in its `project.json` file:
     
     // MyProject.Core/project.json
     {
       ...
       "buildOptions": {
          "emitEntryPoint": false 
       }
    }

Build the project (Mac: <kbd>⌘</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd>, Windows: <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd>) and each project should have own `\bin` and `\obj` folders with new `.dll` files.

## Solution structure
It is very common to group projects, for example, place test projects under the `/test` folder and source projects under the `/src` folder. Add `global.json` file and make similar structure:

    global.json
    /src/
        /MyProject.Core/
           SourceFile.cs
           project.json

        /MyProject.Web/
           /Controllers
           /Views
           project.json

    /test/
        /MyProject.Core.UnitTests/
           SourceFileTest.cs
           project.json

        /MyProject.Web.UnitTests/
           /Controllers
           /Views
           project.json

Edit empty `global.json` file and specify project groups:

    {
        "projects":["src", "test"]
    }


VS Code uses `tasks.json` to run tasks (e.g. building a solution) and `launch.json` for starting a project (e.g. debugging).
If you cannot find these files try to start debugging by pressing <kbd>F5</kbd> and ignore errors, VS Code will generate under the root folder `.vscode` folder with the files.

[![enter image description here][1]][1]

Edit `launch.json` file and specify the path to your start up library, change `MyProject.Web` with your project name:

    {
        "configurations": [
        {
            ...
            "program": "${workspaceRoot}/src/MyProject.Web/bin/Debug/netcoreapp1.0/MyProject.Web.dll",
            "args": [],
            "cwd": "${workspaceRoot}/src/Washita.Web", 
            ...
       }
    }

Edit `tasks.json` file and specify the path to your start up library, change `MyProject.Web` with your project name:

    {
        "tasks": [
            {
            "taskName": "build",
                "args": [
                    "${workspaceRoot}/src/MyProject.Web"
               ],
               "isBuildCommand": true,
               "problemMatcher": "$msCompile"
           }
       ]
    }

Now you should be able to build and debug .NET source files.  

However Intellisense will disappear due the multiple project configuration. To fix it open any `.cs` file and switch to the appropriate project (project.json) by choosing `Select project` in the bottom right corner:

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/I0dzLm.png
  [2]: https://i.stack.imgur.com/7pLIDm.png

