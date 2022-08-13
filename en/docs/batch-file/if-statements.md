---
title: "If statements"
slug: "if-statements"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Syntax
- if [/i] StringToCompare1 == StringToCompare2 (commandA) else (commandB)

- if errorlevel 1 (commandA) else (commandB)
- if %errorlevel% == 1 (commandA) else (commandB)

- if exist Filename (commandA) else (commandB)

- if defined VariableName (commandA) else (commandB)



There are a few syntax to choose from in an `if` statement. We will use `if string1==string2` as an example.

# 1-Line Syntaxes

- `if string1==string2 commandA`
- `if string1==string2 (commandA)`


- `if string1==string2 (commandA) else (commandB)`
- `if string1==string2 (commandA) else commandB`


- `if string1==string2 (commandA)else (commandB)`
- `if string1==string2 (commandA)else commandB`

# Multiline Syntaxes

    if string1==string2 (
        commandA
    )
Or
 
    if string1==string2 (
        commandA
    ) else (
        commandB
    )

There are still some extra syntaxes available.



## Comparing Errorlevel
    If Errorlevel 1 (
        Echo Errorlevel is 1 or higher
        
        REM The phrase "1 or higher" is used because If Errorlevel 1 statement means:
        REM                                          If %Errorlevel% GEQ 1
        REM                                      Not If %Errorlevel% EQU 1
    )
or
    
    If "%Errorlevel%"=="1" (
        Echo Errorlevel is 1
    )

The script above would check the variable Errorlevel(built-in). The `not` operator can be used. 

    Set "Test=%Errorlevel%"
    
    If "%Test%" == "1" (
        Echo Errorlevel is 1
    )
This one also works.

Please note that some commands **do not affect the errorlevel**:
- Break
- Echo
- Endlocal
- For
- If
- Pause
- Rem
- Rd / Rmdir
- Set
- Title

The following commands **set but not clear errorlevel**:
- Cls
- Goto
- Keys
- Popd
- Shift

The following commands **set exit codes but not the errorlevel**:
- Rd / Rmdir

The following commands **set errorlevel but not the exit codes**:
- Md / Mkdir




## Comparing numbers with IF statement
    SET TEST=0

    IF %TEST% == 0 (
        echo TEST FAILED
    ) ELSE IF %TEST% == 1 (
        echo TEST PASSED
    ) ELSE (
        echo TEST INVALID
    )

## Comparing strings
    IF "%~1" == "-help" (
        ECHO "Hello"
    )

where `%1` refers to the first command line argument and `~` removes any quotes that were included when the script was called.

## Check if file exists
    If exist "C:\Foo\Bar.baz" (
        Echo File exist
    )
This checks if the file C:\Foo\Bar.baz's existence. If this exist, it echos File exist
The `Not` operator can also be added.


## If variable exists / set
    If Defined Foo (
        Echo Foo is defined
    )
This would check if a variable is defined or not. Again, the `Not` operator can be used.

