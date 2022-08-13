---
title: "Debugging Angular2 typescript application using Visual Studio Code"
slug: "debugging-angular2-typescript-application-using-visual-studio-code"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

## Launch.json setup for you workspace
1. Turn on Debug from menu - view > debug
2. it return some error during start debug, show pop out notification and open launch.json from this popup notification
It is just because of launch.json not set for your workspace. copy and paste below code in to launch.json //new launch.json
<br/><br/><b>your old launch.json</b>
<pre><code>
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Launch Extension",
            "type": "extensionHost",
            "request": "launch",
            "runtimeExecutable": "${execPath}",
            "args": [
                "--extensionDevelopmentPath=${workspaceRoot}"
            ],
            "stopOnEntry": false,
            "sourceMaps": true,
            "outDir": "${workspaceRoot}/out",
            "preLaunchTask": "npm"
        }
    ]
}
</code></pre>
<br>
Now update your launch.json as below
<br><b>new launch.json</b>  
<br>**// remember please mention your main.js path into it**

<pre>
<code>{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Launch",
            "type": "node",
            "request": "launch",
            "program": "${workspaceRoot}/app/main.js", // put your main.js path
            "stopOnEntry": false,
            "args": [],
            "cwd": "${workspaceRoot}",
            "preLaunchTask": null,
            "runtimeExecutable": null,
            "runtimeArgs": [
                "--nolazy"
            ],
            "env": {
                "NODE_ENV": "development"
            },
            "console": "internalConsole",
            "sourceMaps": false,
            "outDir": null
        },
        {
            "name": "Attach",
            "type": "node",
            "request": "attach",
            "port": 5858,
            "address": "localhost",
            "restart": false,
            "sourceMaps": false,
            "outDir": null,
            "localRoot": "${workspaceRoot}",
            "remoteRoot": null
        },
        {
            "name": "Attach to Process",
            "type": "node",
            "request": "attach",
            "processId": "${command.PickProcess}",
            "port": 5858,
            "sourceMaps": false,
            "outDir": null
        }
    ]
}</code></pre>

3. Now it debug is working, show notification popup for step by step debugging

