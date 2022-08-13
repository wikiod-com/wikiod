---
title: "Getting started with x++"
slug: "getting-started-with-x++"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Hello World
<!-- if version [lte 6] -->

This example will walk the user through creating a job where they can run X++ code within Dynamics AX.

There is a node titled Jobs in the Application Object Tree (AOT). This sample can be added under the Jobs node, and then the job can be run.

1. Within Dynamics Ax, open the AOT (Application Object Tree).

2. Navigate to **Jobs** and right click the node.

3. Click **New Job** which will cause Ax to open a code editor window.

4. Enter the following text inside the code editor.  You can run the code by pressing **F5**.
   

    static void Job1(Args _args)  // Job1 will be the name of the job in the AOT
    {
            
        ;  // This semicolon is necessary to mark the end of variable declarations in versions AX 2009 and lower.  If you are interested in the details see https://stackoverflow.com/questions/1976080/is-semicolon-really-needed-after-declarations-in-x
        
        info('Hello world.');   // Shows the text "Hello world" in a dialog called "infolog"
    
    }
<!-- end version if -->


## Installation or Setup
Detailed instructions on getting x++ set up or installed.

