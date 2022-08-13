---
title: "For Loops in Batch Files"
slug: "for-loops-in-batch-files"
draft: false
images: []
weight: 9967
type: docs
toc: true
---

## Syntax
 - for /l %%p in (startNumber, increment, endNumber) do command
 - for /f %%p in (filename) do command
 - for /f %%p in ("textStrings") do command
 - for /f %%p in ('command') do command
 - for /r drive:\path %%p in (set) do command 
 - for /d %%p in (directory) do command

The `for` command accepts options when the `/f` flag is used. Here's a list of options that can be used:

  - `delims=x`     Delimiter character(s) to separate tokens 
                  
  - `skip=n`       Number of lines to skip at the beginning of file and text strings

  - `eol=;`        Character at the start of each line to indicate a comment

  - `tokens=n`     Numbered items to read from each line or string to process

  - `usebackq`     Use another quoting style:                        
                  
    Use double quotes for long file names in "files"
                   
    Use single quotes for 'textStrings'
                   
    Use back quotes for \`command`

## Recursively Visit Directories in a Directory Tree
`for /r` command can be used to recursively visit all the directories in a directory tree and perform a command. 

    @echo off
    rem start at the top of the tree to visit and loop though each directory
    for /r %%a in (.) do (
      rem enter the directory
      pushd %%a
      echo In directory:
      cd
      rem leave the directory
      popd
      )

Notes:

- [for /r](http://ss64.com/nt/for_r.html) - Loop through files (Recurse subfolders).
- [pushd](http://ss64.com/nt/pushd.html) - Change the current directory/folder and store the previous folder/path for use by the POPD command.
- [popd](http://ss64.com/nt/popd.html) - Change directory back to the path/folder most recently stored by the PUSHD command.



## Looping through each line in a files set
The following will echo each line in the file `C:\scripts\testFile.txt`. Blank lines will not be processed.

    for /F "tokens=*" %%A in (C:\scripts\testFile.txt) do (
      echo %%A
      rem do other stuff here
      )

More advanced example shows, how derived in FOR loop from a restricted files set data can be used to redirect batch execution, while saving the searched content to a file:

    @echo off
    setlocal enabledelayedexpansion
  
    for /f %%i in ('dir "%temp%\test*.log" /o:-d /t:w /b') do (
        set "last=%temp%\%%i"
        type !last! | find /n /i "Completed" >nul 2>&1 >> %temp%\Completed.log ^
         && (echo Found in log %%i & goto :end) || (echo Not found in log %%i & set "result=1"))

    :: add user tasks code here
    if defined result echo Performing user tasks...
    
    :end    
    echo All tasks completed
    exit /b

Note, how long command strings are split into several code lines, and command groups are separated by parentheses.

## Renaming all files in the current directory
The following uses a variable with a `for` loop to rename a group of files.

    SetLocal EnableDelayedExpansion

    for %%j in (*.*) do (
      set filename=%%~nj
      set filename=!filename:old=new!
      set filename=Prefix !filename!
      set filename=!filename! Suffix
      ren "%%j" "!filename!%%~xj"
      )

By defining the variable name `%%j` and associating it with all current files `(*.*)`, we can use the variable in a `for` loop to represent each file in the current directory.

Every iteration (or pass) through the loop thereby processes a different file from the defined group (which might equally have been any group, e.g. `*.jpg` or `*.txt`).

In the first example, we substitute text: the text string "old" is replaced by the text string "new" (if, but only if, the text "old" is present in the file's name).

In the second example, we add text: the text "Prefix " is added to the start of the file name. This is not a substitution. This change will be applied to all files in the group.

In the third example, again we add text: the text " Suffix" is added to the end of the file name. Again, this is not a substitution. This change will be applied to all files in the group.

The final line actually handles the renaming.



## Iteration
    for /L %%A in (1,2,40) do echo %%A

This line will iterate from 1 to 39, increasing by 2 each time.

The first parameter, `1`, is the starting number.

The second parameter, `2`, is the increment.

The third parameter, `40`, is the maximum.

