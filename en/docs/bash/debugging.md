---
title: "Debugging"
slug: "debugging"
draft: false
images: []
weight: 9961
type: docs
toc: true
---

## Checking the syntax of a script with "-n"
The -n flag enables you to check the syntax of a script without having to execute it:

    ~> $ bash -n testscript.sh
    testscript.sh: line 128: unexpected EOF while looking for matching `"'
    testscript.sh: line 130: syntax error: unexpected end of file


## Debugging usigh bashdb
Bashdb is a utility that is similar to gdb, in that you can do things like set breakpoints at a line or at a function, print content of variables, you can restart script execution and more.

You can normally install it via your package manager, for example on Fedora:

    sudo dnf install bashdb 

Or get it from the [homepage][1].
Then you can run it with your script as a paramater:

    bashdb <YOUR SCRIPT>

Here are a few commands to get you started:

    l - show local lines, press l again to scroll down
    s - step to next line 
    print $VAR - echo out content of variable 
    restart - reruns bashscript, it re-loads it prior to execution.
    eval - evaluate some custom command, ex: eval echo hi
    
    b <line num> set breakpoint on some line 
    c - continue till some breakpoint 
    i b - info on break points 
    d <line #> - delete breakpoint at line #
    
    shell - launch a sub-shell in the middle of execution, this is handy for manipulating variables

For more information, I recommend consulting the manual: 
http://www.rodericksmith.plus.com/outlines/manuals/bashdbOutline.html

See also homepage:  
http://bashdb.sourceforge.net/


  [1]: http://bashdb.sourceforge.net/

## Debugging a bash script with "-x"
Use "-x" to enable debug output of executed lines. It can be run on an entire session or script, or enabled programmatically within a script.

Run a script with debug output enabled:

    $ bash -x myscript.sh

Or

    $ bash --debug myscript.sh

Turn on debugging within a bash script. It may optionally be turned back on, though debug output is automatically reset when the script exits.

    #!/bin/bash
    set -x   # Enable debugging
    # some code here
    set +x   # Disable debugging output. 

