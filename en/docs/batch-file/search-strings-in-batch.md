---
title: "Search strings in batch"
slug: "search-strings-in-batch"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Basic strings search
FIND command can scan large files line-by-line to find a certain string. It doesn't support wildcards in the search string.

    find /i "Completed" "%userprofile%\Downloads\*.log" >> %targetdir%\tested.log

    TYPE scan2.txt | FIND "Failed" /c && echo Scan failed || echo Scan Succeeded

FINDSTR command is more feature reach, and supports Regular Expressions (REGEX) search with wildcards in the search string.

    FINDSTR /L /C:"Completed" Results.txt

    echo %%G | findstr /r /b /c:"[ ]*staff.*" >nul && echo Found!

See [FIND][1] and [FINDSTR][2] help sources for more information. 


  [1]: http://ss64.com/nt/find.html
  [2]: http://ss64.com/nt/findstr.html

## Using search results
The following script shows more advanced _split file_ technique, where FOR function loops through a list of files in a directory, and each file content is piped to FINDSTR that looks for a string containing substring `var` preceded by undefined number of spaces and superseded by any extra text. Once found, the searched file is replaced with a new one that contains only the text portion above the search string.

    @echo off
    setlocal enabledelayedexpansion
    pushd "%temp%\Test"
    for %%G in ("*.txt") do (set "break="
        (for /f "tokens=*" %%H in (%%~G) do (
            if not defined break (
                echo %%H | findstr /r /b /c:"[ ]*var.*" >nul && set break=TRUE || echo %%H )
        )) >> %%~nG_mod.txt
        del %%~G & ren %%~nG_mod.txt %%G )
    popd
    exit /b

Note, how setting `break=TRUE` allows to exit FOR loop from the file searched, once the 1st occurrence of the search string is found.

