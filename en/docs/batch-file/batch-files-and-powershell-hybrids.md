---
title: "Batch files and Powershell hybrids"
slug: "batch-files-and-powershell-hybrids"
draft: false
images: []
weight: 9997
type: docs
toc: true
---

## Run Powershell with Temporary Files
This has been mentioned in other [hybrid][1] [topics][2] again and again. The old-school, but easy method to run Powershell is by:

 - `echo`ing the Powershell script into a temporary script
 - Execute the temporary script
 - Optionally remove the temporary script

---

This is a sample script.

    @echo off
    echo powershell-command>Temp.ps1
    echo another line>>Temp.ps1
        rem echo the script into a temporary file

    powershell -File Temp.ps1
        rem execute the temporary script

    del Temp.ps1
        rem Optionally remove the temporary script

The method above requires tons of `echo` statement if a long script is required, here is a better method suggest by @Aacini


    @echo off
    setlocal

        rem Get the number of the "<resource>" line
    for /F "delims=:" %%a in ('findstr /N "<resource>" "%~F0"') do set "start=%%a"

        rem Skip such number of lines and show the rest of this file
    (for /F "usebackq skip=%start% delims=" %%a in ("%~F0") do echo %%a) > Temp.ps1

    powershell -File Temp.ps1
    del /f /s /q Temp.ps1

    goto :EOF

    <resource>
    PS
    Powershell script

  [1]: https://www.wikiod.com/batch-file/batch-and-vbs-hybrids
  [2]: https://www.wikiod.com/batch-file/batch-and-jscript-hybrids

## Use POWERSHELL Command To Execute 1-line Powershell Command
Using the `POWERSHELL` command, we can execute a 1-line command directly from a batch script, without any temporary file.

---

Here's the syntax.

    powershell.exe -Command <yourPowershellCommandHere>

You may also want to include other flags, like `-Nologo` to improve the actual outcome.







## Powershell/batch hybrid without temp files
[This is the approach][1] proposed by the stackoverflow's user [rojo][2] which also can handle the command line arguments :

    <# : batch portion
    @echo off & setlocal
    
    (for %%I in ("%~f0";%*) do @echo(%%~I) | ^
    powershell -noprofile "$argv = $input | ?{$_}; iex (${%~f0} | out-string)"
    
    goto :EOF
    : end batch / begin powershell #>
    
    "Result:"
    $argv | %{ "`$argv[{0}]: $_" -f $i++ }

called like this:

>psbatch.bat arg1 "This is arg2" arg3

will produce:

    Result:
    $argv[0]: C:\Users\rojo\Desktop\test.bat
    $argv[1]: arg1
    $argv[2]: This is arg2
    $argv[3]: arg3


  [1]: http://www.dostips.com/forum/viewtopic.php?f=3&t=5526&start=15#p45095
  [2]: https://stackoverflow.com/users/1683264/rojo

