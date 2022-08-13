---
title: "Getting started with tcl"
slug: "getting-started-with-tcl"
draft: false
images: []
weight: 1
type: docs
toc: true
---

## Installation
Installing **Tcl 8.6.4** on **Windows** :

 1. The easiest way to get Tcl on a windows machine is to install the
    **ActiveTcl** distribution from ActiveState. 
    
 2. Navigate to [www.activestate.com][1] and follow the links to
        download the Free Community Edition of ActiveTcl for Windows (choose
        32/64 bit version appropriately).

 3. Run the installer which will result in a fresh install of ActiveTcl
        usually in the **C:\Tcl** directory.
    
 4. Open a command prompt to test the install, type in "tclsh" which
        should open an interactive tcl console. Enter "info patchlevel" to
        check the version of tcl that was installed and it should display an
        output of the form "8.6.x" depending on the edition of ActiveTcl
        that has been downloaded.

* You may also want to add "C:\Tcl\bin" or its equivalent to your environment **PATH** variable.


    C:\>tclsh
    % info patchlevel
    8.6.4


  [1]: http://www.activestate.com

## The Hello, world program in Tcl (and Tk)
The following code can be entered in a Tcl shell (`tclsh`),
or into a script file and run through a Tcl shell:

    puts "Hello, world!"

It gives the string argument `Hello, world!` to the command `puts`. The `puts` command writes its argument to standard out (your terminal in interactive mode) and adds a newline afterwards.

---

In a Tk-enabled shell, this variation can be used:

    pack [button .b -text "Hello, world!" -command exit]

It creates a graphic button with the text `Hello, world!`
and adds it to the application window. When pressed, 
the application exits.

A Tk-enabled shell is started as: `wish`
Or using `tclsh` along with the following statement:

    package require Tk 



## Features of Tcl
 - Cross Platform Portability
   - Runs on Windows, Mac OS X, Linux, and virtually every variant of unix.
 - Event driven programming
   - Trigger events based on variable read / write / unset.
   - Trigger events when a command is entered or left.
   - Trigger events when a I/O channel (file or network) becomes readable / writable.
   - Create your own events.
   - Trigger a command based on a timer.
 - Object Oriented Programming
   - Mixins.
   - Superclasses and subclasses.
 - Simple Grammar
 - Full unicode support
   - It just works.  No special commands are needed to handle unicode strings.
   - Convert to and from different encoding systems with ease.
 - Flexible
   - Create new control structures and commands.
   - Access variables in the calling procedure's context.
   - Execute code in the calling procedure's context.
 - Powerful introspection capabilities.
   - Many Tcl debuggers have been written in Tcl.
 - Library interface
   - Integrate existing C libraries and provide a Tcl interface to the library.
   - Library "stubs" are not tied to any particular version of Tcl and will 
     still work after a Tcl upgrade.
 - Complete API
   - Embed a Tcl interpreter into your favorite language.
   - Python, Ruby, R, Java and others include a Tcl API.
 - Embedded bigint library. 
   - No special actions are needed to handle very large numerics.
 - Safe interpreters
   - Create sandboxes in which user code can be run.
   - Enable and disable specific commands for the interpreter.
 - Regular Expressions
   - A powerful and fast regular expression engine written by [Henry Spencer][1] (creator of regex).


  [1]: https://en.wikipedia.org/wiki/Henry_Spencer

## Installing packages through teacup
Now days many languages are supporting **archive** server to install their packages into your local machine. TCL also having same archive server we called it as [Teacup](http://wiki.tcl.tk/17305)

``` Shell
teacup version
teacup search <packageName>
```

**Example**
```Shell
teacup install Expect
```

