---
title: "Best Practices"
slug: "best-practices"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

This topic will focus on the things that one should *(not mandatory)* do in a batch file. Using these "best practices" can enhance the effect and the function of a batch file.

## Quotes
Most online batch scripts come with a lot of quote issues.

---

# Examples and Solutions

## Example A

    if %var%==abc echo Test

This code works - when the content of `%var%` does not contains space or other special characters. Now let's assume `%var%` contains 1 whitespace. Now `cmd.exe` sees:

    if  ==abc echo Test

This would cause a failure because `cmd.exe` doesn't understand this syntax.

## Solution A

    if "%var%"=="abc" echo Test

Using quotes, `cmd.exe` sees the entire `%var%`(including space and special characters) as only one normal string. Yet this is not the safest comparison method. The safest one uses `echo`, `pipe`, and `findstr`.

---

## Example B

    cd C:\User\Spaced Name\Spaced FileName.txt

`cd` would only change directory to `C:\User\Spaced`, as `cd` only accepts one path argument.

## Solution B

Simply by adding quotes around the path, the issue would be solved.

    cd "C:\User\Spaced Name\Spaced FileName.txt"

---

There are also a few examples that work better using quotes, like the `set /a` statement, etc. But, when one works on strings that contain spaces or special characters, it is usually much safe to use quotes.



## Spaghetti Code
Spaghetti code means a code snippet that uses many, and often confusing structures. Such as `GOTO`s, exceptions and inconsistent code.

---

# Examples and Solutions
## Example A

    @echo off
    set /a counter=0

    :Loop
    set /a counter=%counter% + 1
    echo %counter%

    if %counter% equ 10 goto :exit
    goto :Loop

    :exit

This program comes with plenty of jumps, making us harder to know what exactly the script is doing.

## Solution A

    @echo off
    for /l %%G in (0,1,10) echo %%G

Using less `GOTO`s, we reduced the amount of code greatly, and we can focus on the actual code.

---

## Example B

Consider the following statements.


    :endGame
    if %player1Score% gtr %player2Score% goto :player1wins
    if %player1Score% lss %player2Score% goto :player2wins
    goto :tie

    :player1wins
    echo player 1 wins
    goto :eof

    :player2wins
    echo player 2 wins
    goto :eof

    :tie
    echo tie
    goto :eof

This snippet requires lots of `goto` statements and can be confusing to debug. To simplify these statements, we can use `call` command. Here is the above script at a better condition.


    :endGame
    if %player1Score% gtr %player2Score% call :message player 1 wins
    if %player1Score% lss %player2Score% call :message player 2 wins
    if %player1Score% equ %player2Score% call :message tie

    goto :eof

    :message
    echo %*
    goto :eof

Both scripts output the exact same result, but the new script is much shorter and clearer. 

    

