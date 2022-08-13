---
title: "Batch file command line arguments"
slug: "batch-file-command-line-arguments"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Batch files with more than 9 arguments
When more than 9 arguments are supplied, the `shift [/n]` command can be used, where `/n` means start at the nth argument, n is between zero and eight.

**Looping through arguments:**

    :args
    set /a "i+=1"
    set arg!i!=%~1
    call echo arg!i! = %%arg!i!%%
    shift
    goto :args

Note, in the above example delayed expansion variable `i` is used to assign argument values to variables array. The `call` command allows to display such variable values inside the loop.

**Counting arguments:**

    for %%i in (%*) do (set /a ArgCount+=1)
    echo %ArgCount%

**Set a variable to n'th argument:**

    set i=5
    call set "path%i%=%%~i"




## Command line arguments supplied to batch files
Batch file command line arguments are parameter values submitted when starting the batch. They should be enclosed in quotes if they contain spaces. In a running batch file, the arguments are used for various purposes, i.e. redirection to `:labels`, setting variables, or running commands. 

The arguments are referred to in the batch file using `%1, %2, ..., %9`.

    @echo off
    setlocal EnableDelayedExpansion
    if not "%1"=="" (
        set "dir=%~1" & set "file=%~2"
        type !dir!\!file! | find /n /i "True" >nul^
         && echo Success! || echo Failure
    )
    exit /b
    
    C:\Users\UserName> test.bat "C:\Temp\Test Results" "Latest.log"
    Success!

Notes:

- In the above example, double quotes are removed by using the argument modifier `%~1`. 
- Long strings are split to several lines using `^ `, and there is a space before the character on the next line.



## Shifting arguments inside brackets
Lets have the following `example.bat` and call it with arguments `1` ,`2` and `3`:

    @echo off
    
    (
        shift
        shift
        echo %1
    )

As the variable expansion will change after the the end brackets context is reached the output will be:

>1

As this might be an issue when shifting inside brackets to access the argument you'll need to use call:

    @echo off
    
    (
        shift
        shift
        call echo %%1
    ) 

now the output will be `3`. As `CALL` command is used  (which will lead to additional variable expansion) with this technique the arguments accessing can be also parametrized:

    @echo off
    
    set argument=1
    
        shift
        shift
        call echo %%%argument%
    

with delayed expansion:

    @echo off
    setlocal enableDelayedExpansion
    set argument=1
    
        shift
        shift
        call echo %%!argument!
    
the output will be

>3


