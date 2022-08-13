---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9981
type: docs
toc: true
---

You can add starting variables to the function by adding `<parameter>` to it's label. These starting variables can be accessed with `%n` where n is the starting variable's number (`%1` for the first, `%2` for the second. This `%n` method works for %1 - %9. For parameter 10 - 255, you will need to use the [Shift][1] command).  
For example:

    :function <var1> <var2>

Once you use `call :function param1 param2`, `param1` can be accessed with `%1`, and `param2` with `%2`.  
Note: the `<parameter>` isn't strictly necessary, but it helps with readability.

A neat trick that is useful when many variable are flying about is to use `setlocal` and `endlocal` in tandem with `%n`. `setlocal` and `endlocal` essentially make the function it's own separate instance of the command prompt, variables set in it only stick around while it's in the frame.

If you are using `setlocal` and `endlocal`, and you are returning global values use this.

    endlocal & set var=variable
This sets the global value `var` to `variable`. You can chain these together for multiple variables.

    endlocal & set var=variable & set var2=variable number 2
This sets the global variable `var` to `variable` and the global value `var2` to `variable number 2`.  
Since code in code blocks are also performed simultaneously, you can do this as well.

    if "%var%"=="" (
        endlocal
        set %~2=10
    )
But, you __cannot__ do this.

    if "%var%"=="" (
        set %~2=10
        endlocal
    )


  [1]: https://www.wikiod.com/batch-file/batch-file-command-line-arguments

## Anonymous functions in batch files
[Anonymous][1] functions technique uses the fact that `CALL` command uses internally `GOTO` when subroutine is called and abusing help message printing [with variable double expansion][2]:

    @echo off
    setlocal 
    set "anonymous=/?"
    
    call :%%anonymous%% a b c 3>&1 >nul
    
    if "%0" == ":%anonymous%" (
      echo(
      echo Anonymous call:
      echo %%1=%1 %%2=%2 %%3=%3
      exit /b 0
    )>&3

You can call an anonymous function only if it is defined after the `CALL` (or after finishing brackets context if the `CALL` is executed within brackets). It cannot be called from an [outside script][3] ,but is a slower than normal function call.


  [1]: http://www.dostips.com/forum/viewtopic.php?f=3&t=3803&start=15#p42446
  [2]: https://stackoverflow.com/questions/31987023/why-call-prints-the-goto-help-message-in-this-scriptand-why-command-after-that
  [3]: https://stackoverflow.com/a/30170342/388389

## Simple Function
    call :FunctionX
    rem More code...
    
    :FunctionX
    rem Some code here.
    goto :eof
This is a very simple function.
Functions are in-program commands that do multiple commands at a time.
Functions are made by creating a label and putting code in it, and once it is done, you add a `goto :eof` or `exit /b <ErrorlevelYou'dLike>` which returns to where it was invoked.
Functions are invoked with `call :functionname adparams`.



## Function With Parameters
    call :tohex 14 result
    rem More code...
    
    :tohex <innum> <outvar>
    set dec=%1
    set outvar=%~2
    rem %n and %~n are functionally identical, but %~n is slightly safer.
    goto :eof
This takes the additional parameters from the `call` as if the function was a separate Batch file.  
Note: the `<parameter>` isn't necessary, but it helps with readability.

## Function Utilizing setlocal and endlocal
    set var1=123456789
    set var2=abcdef
    call :specialvars
    echo %var1%, %var2%
    rem More code...
    
    :specialvars
    setlocal
    set var1=987654321
    set var2=fedcba
    endlocal
    goto :eof
When inside the section `setlocal` , `endlocal` section, variables are seperate from the caller's variables, hence why %var1% and %var2% weren't changed.

## Combining them all
    set importantvar=importantstuff
    call :stuff 123 var1
    rem More code...
    
    :stuff <arg1> <arg2>
    setlocal
    set importantvar=%~1
    echo Writing some stuff into %~2!
    endlocal
    set %~2=some stuff
    setlocal
    set importantvar=junk
    endlocal
    goto :eof
This utilizes the basic function, `setlocal` and `endlocal` and arguments to create an odd little function.

## Calling functions from another batch file
Lets have the following file called **library.cmd** :

    @echo off
    
    echo -/-/- Batch Functions Library -/-/-
    
    :function1
        echo argument1 - %1
        goto :eof

To execute only the **:function1** without the code of the rest of the file you should put a label **:function1** in the caller bat and use it like this:

    @echo off
    
    call :function1 ###
    exit /b %errorlevel%
    
    :function1
        library.bat %*

the output will be (the code outside the function in `library.cmd` is not executed):

>argument1 - ###

For more info check [this][1].


  [1]: https://stackoverflow.com/questions/30168091/call-a-subroutine-in-a-batch-from-another-batch-file

