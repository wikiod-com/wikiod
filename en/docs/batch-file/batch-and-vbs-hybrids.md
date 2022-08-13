---
title: "Batch and VBS hybrids"
slug: "batch-and-vbs-hybrids"
draft: false
images: []
weight: 9991
type: docs
toc: true
---

`Batch` are capable of running with `VBS` functionality further increasing their reliability. For example, `VBS` can deal with decimals, spaces, and some other advanced operations that cannot be done in `batch`. Also is capable of working with WMI and ActiveX objects.

## Run VBS with temporary file(s)
The old-school method for running another script from `batch` is to `echo` the script into another location, and then run it.

This method can be represented like this:

    @echo off
    rem VBS below
        echo your vbs > TempVBS.vbs
        echo other vbs>>TempVBS.vbs
    rem End of VBS

    cscript //nologo TempVBS.vbs
    del /f /s /q TempVBS.vbs

The method above would require lots of `echo (vbs) >> TempVBS.vbs`, so here's a way to shorten it. *(code by Aacini)*

    @echo off
    setlocal

    rem Get the number of the "<resource>" line
    for /F "delims=:" %%a in ('findstr /N "<resource>" "%~F0"') do set "start=%%a"

    rem Skip such number of lines and show the rest of this file
    (for /F "usebackq skip=%start% delims=" %%a in ("%~F0") do echo %%a) > Program.vbs

    cscript //nologo Program.vbs
    del /f /s /q Program.vbs
    exit /b

    <resource>
    your vbs
    another line of vbs

The last method is by using `streams`. A file can have a few streams. And every stream can contain different information.

    @echo off

        echo vbs >%0:stream1
        rem This command redirect "vbs" into the stream1 of this script, then we can call it later

    cscript %0:stream1 //nologo
       rem if you like, you can clear the stream1 of this file by:
       type nul>%0:stream1
    

    
    

## Embed vbscript code into batch file without using temporary files
Here's an example with the technique(hack) invented by the [dostips][1] forums' user Liviu:

 

    @echo off
    echo Printed by CMD.EXE
    cscript //nologo "%~f0?.wsf" //job:JS //job:VBS
    
    exit /b %errorlevel%
    
    ----END OF BATCH CODE---
    <package>
      <job id="JS"> 
        <script language="VBScript">
        
            WScript.Echo("Printed by VBScript"):
            
        </script>
      </job>
      <job id="VBS"> 
        <script language="JScript">
        
            WScript.Echo("Printed by JScript");
            
        </script>
      </job>
    </package>

As running `wsf` file with [windows script host][2] is extension sensitive you can run a file with any extension by adding `?.wsf` at the end of the file (which is the core of the hack). While the Liviu's example is probably more robust the above code is more simplified version. As wsh does not care much about the things outside the `<package>` node you are not obligated to put everything in xml comments. Though it's to be careful with redirection symbols (`<` and `>`)


  [1]: http://www.dostips.com/forum/viewtopic.php?t=5543
  [2]: https://en.wikipedia.org/wiki/Windows_Script_File

