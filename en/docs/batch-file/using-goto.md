---
title: "Using Goto"
slug: "using-goto"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

Goto Is simple. By using simple goto statements, you can move anywhere you want to in your code. It can be also used to make functions (Showed in how to make functions).

## Syntax
 - goto :Label
 - goto Label
 - goto :EOF

## Parameters
| Parameter | Details |
| ------ | ------ |
| `:Label`   | Any label that is valid (defined by `:<LabelName>`)|
| `:EOF`       | A pre-defined label that exits the current script of function(same as `exit /b`)       |


So in other words, if the number the player inserted is 1, it'll go back to the :Name part of the code.

so if the input is equal to 1, go back to the line with :Name

Make Sure if you use this, the word begins with the Colen (:).


## Goto with variable
`Goto` accepts the use of variable value to act as the label to goto.

Example:

    @echo off
    
    echo a = 1
    echo b = 2

    set /p "foo=Enter option:"
    goto %foo%

However, you should check the input so it will not go to somewhere that does not exist. Going to an undefined label will terminate your batch script instantly.



## Example Programs
For Example:

    echo Hello!
    pause >nul
    :Name
    echo What Is Your Name
    set /p Input=Name: 
    echo so %Input% Is Your Name, right?
    echo Rename?
    echo 1 For Yes
    echo 2 For No
    set /p Input=Rename:
    if %Input%=1 goto Name

Another Example:

    @echo off
    echo 1 or 2?
    set /p input=Choice: 
    if %input%=1 goto Skip
    echo You Chose 1
    pause >nul
    echo So time for stuff
    pause >nul
    echo Random Stuf
    pause >nul
    :Skip
    echo So that's it.
    pause >nul
    

    

