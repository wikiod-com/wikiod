---
title: "Pathnames and filenames"
slug: "pathnames-and-filenames"
draft: false
images: []
weight: 9984
type: docs
toc: true
---

## Syntax
- file dirname *filepath*
- file tail *filepath*
- file rootname *filepath*
- file extension *filepath*
- file join *path1* *path2* *...*
- file normalize *path*
- file nativename *path*

## Working with pathnames and filenames

    % set mypath /home/tcluser/sources/tcl/myproject/test.tcl
    /home/tcluser/sources/tcl/myproject/test.tcl
    % set dir [file dirname $mypath]
    /home/tcluser/sources/tcl/myproject
    % set filename [file tail $mypath]
    test.tcl
    % set basefilename [file rootname $filename]
    test
    % set extension [file extension $filename]
    .tcl
    % set newpath [file join $dir .. .. otherproject]
    /home/tcluser/sources/tcl/myproject/../../otherproject
    % set newpath [file normalize $newpath]
    /home/tcluser/source/otherproject
    % set pathdisp [file nativename $newpath] ; # not on windows...
    /home/tcluser/source/otherproject
    % set pathdisp [file nativename C:$newpath] ; # on windows...
    C:\home\tcluser\source\otherproject
    % set normpath [file normalize $pathdisp]
    C:/home/tcluser/source/otherproject
    
Documentation: [file][1]
    
  [1]: http://tcl.tk/man/tcl/TclCmd/file.htm

