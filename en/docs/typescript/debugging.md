---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9874
type: docs
toc: true
---

There are two ways of running and debugging TypeScript:

__Transpile to JavaScript__, run in node and use mappings to link back to the TypeScript source files

or

__Run TypeScript directly__ using [ts-node](https://www.npmjs.com/package/ts-node)

This article describes both ways using [Visual Studio Code](https://code.visualstudio.com/) and [WebStorm](https://www.jetbrains.com/webstorm/).
All examples presume that your main file is *index.ts*.

## TypeScript with ts-node in WebStorm
Add this script to your ``package.json``:
    
    "start:idea": "ts-node %NODE_DEBUG_OPTION% --ignore false index.ts",

Right click on the script and select *Create 'test:idea'...* and confirm with 'OK' to create the debug configuration:

[![enter image description here][1]][1]

Start the debugger using this configuration:

[![enter image description here][2]][2]


  [1]: https://i.stack.imgur.com/JoNVM.png
  [2]: https://i.stack.imgur.com/VOstc.png

## TypeScript with ts-node in Visual Studio Code
Add ts-node to your TypeScript project:

    npm i ts-node

Add a script to your ``package.json``:

    "start:debug": "ts-node --inspect=5858 --debug-brk --ignore false index.ts"

The ``launch.json`` needs to be configured to use the *node2* type and start npm running the ``start:debug`` script:

    {
        "version": "0.2.0",
        "configurations": [
            {
                "type": "node2",
                "request": "launch",
                "name": "Launch Program",
                "runtimeExecutable": "npm",
                "windows": {
                    "runtimeExecutable": "npm.cmd"
                },
                "runtimeArgs": [
                    "run-script",
                    "start:debug"
                ],
                "cwd": "${workspaceRoot}/server",
                "outFiles": [],
                "port": 5858,
                "sourceMaps": true
            }
        ]
    }

## JavaScript with SourceMaps in Visual Studio Code
In the ``tsconfig.json`` set

    "sourceMap": true,

to generate mappings alongside with js-files from the TypeScript sources using the ``tsc`` command.<BR>
The [launch.json](https://code.visualstudio.com/Docs/editor/debugging) file:

    {
        "version": "0.2.0",
        "configurations": [
            {
                "type": "node",
                "request": "launch",
                "name": "Launch Program",
                "program": "${workspaceRoot}\\index.js",
                "cwd": "${workspaceRoot}",
                "outFiles": [],
                "sourceMaps": true
            }
        ]
    }

This starts node with the generated index.js (if your main file is index.ts) file and the debugger in Visual Studio Code which halts on breakpoints and resolves variable values within your TypeScript code.

## JavaScript with SourceMaps in WebStorm
Create a *Node.js* [debug configuration](https://www.jetbrains.com/help/webstorm/2016.3/creating-and-editing-run-debug-configurations.html#createExplicitly) and use ``index.js`` as *Node parameters*.

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/A7Mt9.png

