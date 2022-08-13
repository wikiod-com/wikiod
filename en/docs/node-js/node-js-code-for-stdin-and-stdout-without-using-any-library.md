---
title: "Node.js code for STDIN and STDOUT without using  any library"
slug: "nodejs-code-for-stdin-and-stdout-without-using--any-library"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

This is a simple program in node.js to which takes input from the user and prints it to the console. 

The **process** object is a global that provides information about, and control over, the current Node.js process. As a global, it is always available to Node.js applications without using require().




## Program
The **process.stdin** property returns a Readable stream equivalent to or associated with stdin.

The **process.stdout** property returns a Writable stream equivalent to or associated with stdout.

    process.stdin.resume()
    console.log('Enter the data to be displayed ');
    process.stdin.on('data', function(data) { process.stdout.write(data) })

