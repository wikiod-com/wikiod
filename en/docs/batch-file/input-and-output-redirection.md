---
title: "Input and output redirection"
slug: "input-and-output-redirection"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
 - [command] [[> | >> | < | 2> | 2>>] file]
 - [[> | >> | < | 2> | 2>>] file] [command]

## Parameters
| Parameter |          Details          |
| :-------- | :------------------------ |
| command   | Any valid command.        |
| `>`       | Write `STDOUT` to file.   |
| `>>`      | Append `STDOUT` to file.  |
| `<`       | Read file to `STDIN`.     |
| `2>`      | Write `STDERR` to file.   |
| `2>>`     | Append `STDERR` to file.  |
| file      | The path to a file.       |

  * You can add as many different redirections as you want, so long as the redirection symbol and file remain together and in the correct order.

## An Example...
    
    @echo off
    setlocal
    set /p "_myvar=what is your name?"
    echo HELLO!>file.txt
    echo %_myvar%!>>file.txt
    echo done!
    pause
    type file.txt
    endlocal
    exit
Now file.txt looks like:
    
    HELLO!
    John Smith!
(assuming you typed `John Smith` as your name.)

Now your batch file's console looks like:
    
    what is your name?John Smith
    done!
    Press any key to continue...
    HELLO!
    John Smith!
( and it should exit so quickly that you may not be able to see anything after the prompt `Press any key to coninue...` ) 

## Redirect special character with delayed expansion enabled
[This example][1] echoes the special character `!` into a file. This would only work when DelayedExpansion is disabled. When delayed expansion in enabled, you will need to use three carets and an exclamation mark like this:

    @echo off
    setlocal enabledelayedexpansion
    

    echo ^^^!>file
    echo ^>>>file
    
    goto :eof    

        ^> is the text
        >> is the redirect operator
   
    pause
    endlocal
    exit /b

This code will echo the following text into the file

    !
    >
as

    ^^^ escapes the ! and echos it into the file
    ^> escapes the > and echos it into the file

  [1]: https://www.wikiod.com/batch-file/input-and-output-redirection#An Example...

## Write to a file
    @echo off
    cls
    echo Please input the file path, surrounded by "double quotation marks" if necessary.
    REM If you don't want to redirect, escape the > by preceding it with ^
    set /p filepath=^> 
    
    echo Writing a random number
    echo %RANDOM% > %filepath%
    echo Reading the random number
    type %filepath%
    
    REM Successive file writes will overwrite the previous file contents
    echo Writing the current directory tree:
    > %filepath% tree /A
    echo Reading the file
    type %filepath%
    
    REM nul is a special file. It is always empty, no matter what you write to it.
    echo Writing to nul
    type %windir%\win.ini > nul
    echo Reading from nul
    type nul
    
    echo Writing nul's contents to the file
    type nul > %filepath%
    echo Reading the file
    type %filepath%

