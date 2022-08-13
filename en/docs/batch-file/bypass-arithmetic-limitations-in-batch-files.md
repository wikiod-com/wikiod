---
title: "Bypass arithmetic limitations in batch files"
slug: "bypass-arithmetic-limitations-in-batch-files"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Batch files allows only 32bit integer calculations , though this can be bypassed with different approaches.

## Using powershell
As the powershell is installed by default on every windows system from 7/2008 and above it can be used for more complex calculations:

    @echo off
    set "expression=(2+3)*10/1000"
    for /f %%# in ('"powershell %expression%"') do set result=%%#
    echo %result%

Mind the additional double quotes in the `for /f` which prevent brackets conflicts with the for command syntax.

Potential issue is that powershell is much slower than using wsh/vbscript/jscript due to the loading of the .net framerwork

## Using jscript
`WSH/JScript` is installed on every windows system since NT so using it for more complex calculations makes it pretty portable. JScript is easier for combining it with batch file :

    @if (@codesection==@batch) @then
    @echo off
    
    set "expression=2*(2+3)/1000"
    for /f %%# in ('cscript //nologo //e:jscript "%~f0" "%expression%"') do set 
    result=%%#
    echo %result%
    :: more batch code
    
    exit /b %errorlevel%
    @end
    WScript.Echo(eval(WScript.Arguments(0)));

With this approach you can put your whole code in a single file.It is faster than using powershell. [Here][1] and [here][2] more advanced scripts can be found (which can be used as external files).


  [1]: http://www.dostips.com/forum/viewtopic.php?t=6212
  [2]: https://stackoverflow.com/questions/21392487/batch-jscript-hybrid-calculator

## Emulating pen and paper calculations, math functions implementations
 1. [***Here***][1] can be found the most comprehensive math library
    that emulates pen and paper calculations and allows working with
    bigger numbers.
 2. Here are another examples of pen and paper emulations: [ADD][2] ,
    [Comparison][3] , [Multiply][4]
 3. Some math functions implementations can be found [here][5].

  [1]: https://github.com/Oatworm/MathLibrary.cmd
  [2]: http://www.robvanderwoude.com/files/add_nt.txt
  [3]: http://www.robvanderwoude.com/files/islarger_3rdparty_bat.txt
  [4]: http://www.robvanderwoude.com/files/multiply_3rdparty_bat.txt
  [5]: http://www.dostips.com/forum/viewtopic.php?t=5819

