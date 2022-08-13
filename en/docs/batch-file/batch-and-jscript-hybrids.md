---
title: "Batch and JSCript hybrids"
slug: "batch-and-jscript-hybrids"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

`JScript` is actually the superset of `Javascript` (it's 1.8.1 version  - so some newer features are not available ), and they can be embedded into a `batch` script for extending `batch` script's functions. Usually, techniques of embedding are using the JScript directives (not part of the official Javascript standard) in order to separate the batch and JScript code. JScript allows you to work with Com/ActiveX objects, as well as with WMI objects in addition to the standard Javascript.

## Embedded JScript In a Batch File
This following example is created by user Michael Dillon from [this answer][1].

---

Consider the following script:

    @set @junk=1 /*
    @echo off
    cscript //nologo //E:jscript %0 %*
    goto :eof
    */

    //JScript aka Javascript here

This script snippet does:

- Execute the `cscript` command which calls itself with all the arguments provided.
- As the part after `@set @junk=1` is commented(`/*` and `*/` Are valid JScript comment),
- JScript will ignore them.

- **Note: We need the `@set @junk=1` part because the batch file does not recognize `/*` as a command, but a `set` statement will be a workaround. JScript will recognize `/*` as a comment so the other  `batch` file will not be executed by JScript engine.**

---

You can add your JScript after `*/` and start extending your batch file scripting!



  [1]: https://stackoverflow.com/questions/2325420/embed-javascript-in-bat-files

## Run JScript With Temporary Files
As mentioned [here][1], the old-school method to run another script is by using temporary files. Simple `echo` it into a file and then run it(and remove it optionally).


---

Here's the basic concept:

    @echo off
    echo //A JS Comment > TempJS.js
    echo //Add your code>>TempJS.js

    cscript //nologo //e:cscript.exe TempJS.js

    del /f /s /q TempJS.js

But this would require lots of `echo` statements to create a relatively large JScript. Here's a better method by Aacini.

    @echo off
    setlocal
    
    rem Get the number of the "<resource>" line
    for /F "delims=:" %%a in ('findstr /N "<resource>" "%~F0"') do set "start=%%a"
    
    rem Skip such number of lines and show the rest of this file
    (for /F "usebackq skip=%start% delims=" %%a in ("%~F0") do echo %%a) > TempJS.js
    
    cscript //nologo //e:cscript.txt TempJS.js
    del /f /s /q TempJS.js

    goto :EOF
    
    <resource>
    JScript
    JScript
    JScript


  [1]: https://www.wikiod.com/batch-file/batch-and-vbs-hybrids

