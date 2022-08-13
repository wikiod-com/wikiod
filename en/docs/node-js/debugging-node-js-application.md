---
title: "Debugging Node.js application"
slug: "debugging-nodejs-application"
draft: false
images: []
weight: 9910
type: docs
toc: true
---

## Core node.js debugger and node inspector
# Using core debugger

Node.js provides a build in non graphical debugging utility. To start the build in the debugger, start the application with this command:

    node debug filename.js

Consider the following simple Node.js application contained in the `debugDemo.js`

    'use strict';
    
    function addTwoNumber(a, b){
    // function returns the sum of the two numbers
    debugger
      return a + b;
    }
    
    var result = addTwoNumber(5, 9);
    console.log(result);

The keyword `debugger` will stop the debugger at that point in the code.

## Command reference

1. Stepping


    cont, c - Continue execution
    next, n - Step next
    step, s - Step in
    out, o - Step out

2. Breakpoints


    setBreakpoint(), sb() - Set breakpoint on current line
    setBreakpoint(line), sb(line) - Set breakpoint on specific line


To Debug the above code run the following command

    node debug debugDemo.js

Once the above commands runs you will see the following output. To exit from the debugger interface, type `process.exit()`

[![enter image description here][4]][4]

Use `watch(expression)` command to add the variable or expression whose value you want to watch and `restart` to restart the app and debugging.

Use `repl` to enter code interactively. The repl mode has the same context as the line you are debugging. This allows you to examine the contents of variables and test out lines of code. Press `Ctrl+C` to leave the debug repl.


# Using Built-in Node inspector

<!-- if version [gte v6.3.0] -->

You can run node's [built in][1] v8 inspector! The [node-inspector][2] plug-in is not needed anymore.

Simply pass the inspector flag and you'll be provided with a URL to the inspector

    node --inspect server.js


  [1]: https://nodejs.org/api/debugger.html#debugger_v8_inspector_integration_for_node_js
  [2]: https://github.com/node-inspector/node-inspector

<!-- end version if -->

# Using Node inspector

Install the node inspector:

    npm install -g node-inspector

Run your app with the node-debug command:

    node-debug filename.js

After that, hit in Chrome:

    http://localhost:8080/debug?port=5858

Sometimes port 8080 might not be available on your computer. You may get the following error:

 > Cannot start the server at 0.0.0.0:8080. Error: listen EACCES.

In this case, start the node inspector on a different port using the following command.

    $node-inspector --web-port=6500

You will see something like this:

[![enter image description here][3]][3]


  [4]: http://i.stack.imgur.com/XSJMF.png
  [3]: http://i.stack.imgur.com/JpaL6.png

