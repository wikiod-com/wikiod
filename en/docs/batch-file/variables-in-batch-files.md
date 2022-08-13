---
title: "Variables in Batch Files"
slug: "variables-in-batch-files"
draft: false
images: []
weight: 9832
type: docs
toc: true
---

## Variable Substitution
Unlike other programming languages, in a batch file a variable is substituted by its actual value **before** the batch script is run. In other words, the substitution is made when the script is _read_ into memory by the command processor, not when the script is later _run_. 

This enables the use of variables as commands within the script, and as part of other variable names in the script, etc. The "script" in this context being a line - or block - of code, surrounded by round brackets: `()`.

But this behaviour does mean that you cannot change a variable's value inside a block!

    SET VAR=Hello
    FOR /L %%a in (1,1,2) do (
        ECHO %VAR%
        SET VAR=Goodbye
    )
will print

    Hello
    Hello

since (as you see, when watching the script run in the command window) it is evaluated to:

    SET VAR=Hello
    FOR /L %%a in (1,1,2) do (
        echo Hello
        SET VAR=Goodbye
    )

In the above example, the `ECHO` command is evaluated as `Hello` when the script is read into memory, so the script will echo `Hello` forever, however many passes are made through the script.

The way to achieve the more "traditional" variable behaviour (of the variable being expanded whilst the script is running) is to enable "delayed expansion". This involves adding that command into the script prior to the loop instruction (usually a FOR loop, in a batch script), and using an exclamation mark (!) instead of a percent sign (%) in the variable's name:

    setlocal enabledelayedexpansion 
    SET VAR=Hello
    FOR /L %%a in (1,1,2) do (
        echo !VAR!
        SET VAR=Goodbye
    )
    endlocal

will print

    Hello
    Goodbye

The syntax `%%a in (1,1,2)` causes the loop to run 2 times: on the first occasion, the variable bears its initial value of 'Hello', but on the second pass through the loop - having executed the second SET instruction as the last action on the 1st pass - this has changed to the revised value 'Goodbye'.


**Advanced variable substitution**
 
Now, an advanced technique. Using the `CALL` command allows the batch command processor to expand a variable located on the same line of the script. This can deliver multilevel expansion, by repeated CALL and modifier use.

This is useful in, for example, a FOR loop. As in the following example, where we have a numbered list of variables:

 `"c:\MyFiles\test1.txt" "c:\MyFiles\test2.txt" "c:\MyFiles\test3.txt"`

We can achieve this using the following FOR loop:

    setlocal enabledelayedexpansion
    for %%x in (%*) do (
        set /a "i+=1"
        call set path!i!=%%~!i!
        call echo %%path!i!%%
    )
    endlocal

Output:

    c:\MyFiles\test1.txt
    c:\MyFiles\test2.txt
    c:\MyFiles\test3.txt

Note that the variable `!i!` is first expanded to its initial value, 1, then the resulting variable, %1, is expanded to its actual value of `c:\MyFiles\test1.txt`. This is *double expansion* of the variable `i`. On the next line, `i` is again double expanded, by use of the `CALL ECHO` command together with the `%%` variable prefix, then printed to the screen (i.e. displayed on screen).

On each successive pass through the loop, the initial number is increased by 1 (due to the code `i+=1`). Thus it increases to `2` on the 2nd pass through the loop, and to `3` on the 3rd pass. Thus the string echoed to the screen alters with each pass.



## Declaration
To create a simple variable and assign it to a value or string use the `SET` command:

    SET var=10

Here, the code declares a new variable `var` with a value of `10`. By default all variables are stored internally as strings; this means that the value `10` is no different to `foo1234` or `Hello, World!`


# Notes about quotation marks

Quotation marks used will be included in the variable's value:

    SET var="new value"             <-- %var% == '"new value"'

# Spaces in variables

Batch language considers spaces to be acceptable parts of variable names. For instance, `set var = 10` will result in a variable called <code>var </code> that contains the value <code> 10</code> (note the extra space to the right of var and the left of the 10).

# Using quotation marks to eliminate spaces

In order to prevent spaces, use quotation marks around the entire assignment; the variable name and value. This also prevents accidental trailing spaces at the end of the line (the `␣` character denotes a space):

    SET␣var=my␣new␣value␣           <-- '%var%' == 'my new value '
    SET␣"var=my␣new␣value"␣         <-- '%var%' == 'my new value'

Also, use quotation marks when joining multiple statements with `&` or `|` - alternatively, put the symbol directly after the end of the variable's value:

    SET var=val & goto :next        <-- '%var%' == 'val '
    SET "var=val" & goto :next      <-- '%var%' == 'val'
    SET var=val& goto :next         <-- '%var%' == 'val'





## Declare multiple variables
When multiple variables are defined at the beginning of the batch, a short definition form may be used by employing a [replacement][1] string.

    @echo off
    set "vars=_A=good,_B=,_E=bad,_F=,_G=ugly,_C=,_H=,_I=,_J=,_K=,_L=,_D=6
    set "%vars:,=" & set "%"
    
    for /f %%l in ('set _') do echo %%l
    exit /b
    
    _A=good
    _D=6
    _E=bad
    _G=ugly

Note in the above example, variables are natively alphabetically sorted, when printed to screen.


  [1]: http://ss64.com/nt/syntax-replace.html

## Usage
    echo %var%
This code will echo the value of `var`

If `setLocal EnableDelayedExpansion` is used, the following will echo the value of `var` (the standard expression %var% will not work in that context).

    echo !var!

In batch files, variables can be used in any context, including as parts of commands or parts of other variables. You may not call a variable prior to defining it.

Using variables as commands:

    set var=echo
    %var% This will be echoed

Using variables in other variables:

    set var=part1
    set %var%part2=Hello
    echo %part1part2%




## Using a Variable as an Array
It is possible to create a set of variables that can act similar to an array (although they are not an actual array object) by using spaces in the <code>SET</code> statement:
    
    @echo off
    SET var=A "foo bar" 123
    for %%a in (%var%) do (
        echo %%a
    )
    echo Get the variable directly: %var%

Result:

    A
    "foo bar"
    123
    Get the variable directly: A "foo bar" 123

It is also possible to declare your variable using indexes so you may retrieve specific information. This will create multiple variables, with the illusion of an array:

    @echo off
    setlocal enabledelayedexpansion
    SET var[0]=A
    SET var[1]=foo bar
    SET var[2]=123
    for %%a in (0,1,2) do (
        echo !var[%%a]!
    )
    echo Get one of the variables directly: %var[1]%

Result:

    A
    foo bar
    123
    Get one of the variables directly: foo bar

Note that in the example above, you cannot reference <code>var</code> without stating what the desired index is, because <code>var</code> does not exist in its own. This example also uses <code>setlocal enabledelayedexpansion</code> in conjunction with the exclamation points at <code>!var[%%a]!</code>. You can view more information about this in the [Variable Substitution Scope Documentation][1].


  [1]: https://www.wikiod.com/batch-file/variables-in-batch-files#Variable Substitution

## Setting variables from an input
Using the `/p` switch with the `SET` command you can define variables from an Input.

This input can be a user Input (keyboard) :

    echo Enter your name : 
    set /p name=
    echo Your name is %name%

Which can be simplified like this :

    set /p name=Enter your name :
    echo Your name is %name%

Or you can get the input from a file :

    set /p name=< file.txt
in this case you'll get the value of the first line from `file.txt`

Getting the value of various line in a file :

    (
       set /p line1=
       set /p line2=
       set /p line3=
    
    ) < file.txt






## Operations on Variables
    set var=10
    set /a var=%var%+10
    echo %var%

The final value of `var` is 20.

The second line is not working within a command block used for example on an __IF__ condition or on a __FOR__ loop as delayed expansion would be needed instead of standard environment variable expansion.

Here is another, better way working also in a command block:

    set var=10
    set /A var+=10
    echo %var%

The command prompt environment supports with signed 32-bit integer values:

- addition `+` and `+=`
- subtraction `-` and `-=`
- multiplication `*` and `*=`
- division `/` and `/=`
- modulus division `%` and `%=`
- bitwise AND `&`
- bitwise OR `|`
- bitwise NOT `~`
- bitwise XOR `^`
- bitwise left shift `<<`
- bitwise right shift `>>`
- logical NOT `!`
- unary minus `-`
- grouping with `(` and `)`

The Windows command interpreter does not support 64-bit integer values or floating point values in arithmetic expressions.

__Note:__ The operator `%` must be written in a batch file as `%%` to be interpreted as operator.

In a command prompt window executing the command line `set /A Value=8 % 3` assigns the value `2` to environment variable `Value` and additionally outputs `2`.

In a batch file must be written `set /A Value=8 %% 3` to assign the value `2` to environment variable `Value` and nothing is output respectively written to handle __STDOUT__ (standard output). A line `set /A Value=8 % 3` in a batch file would result in error message *Missing operator* on execution of the batch file.

The environment requires the switch `/A` for arithmetic operations only, not for ordinary string variables.

Every string in the arithmetic expression after `set /A` being whether a number nor an operator is automatically interpreted as name of an environment variable.

For that reason referencing the value of a variable with `%variable%` or with `!variable!` is not necessary when the variable name consists only of word characters (0-9A-Za-z_) with first character not being a digit which is especially helpful within a command block starting with `(` and ending with a matching `)`.

Numbers are converted from string to integer with C/C++ function [strtol](http://www.cplusplus.com/reference/cstdlib/strtol/) with `base` being zero which means automatic base determination which can easily result in unexpected results.

Example:

    set Divided=11
    set Divisor=3

    set /A Quotient=Divided / Divisor
    set /A Remainder=Divided %% Divisor

    echo %Divided% / %Divisor% = %Quotient%
    echo %Divided% %% %Divisor% = %Remainder%

    set HexValue1=0x14
    set HexValue2=0x0A
    set /A Result=(HexValue1 + HexValue2) * -3

    echo (%HexValue1% + %HexValue2%) * -3 = (20 + 10) * -3 = %Result%

    set /A Result%%=7
    echo -90 %%= 7 = %Result%

    set OctalValue=020
    set DecimalValue=12
    set /A Result=OctalValue - DecimalValue

    echo %OctalValue% - %DecimalValue% = 16 - 12 = %Result%

The output of this example is:

    11 / 3 = 3
    11 % 3 = 2
    (0x14 + 0x0A) * -3 = (20 + 10) * -3 = -90
    -90 %= 7 = -6
    020 - 12 = 16 - 12 = 4

Variables not defined on evaluation of the arithmetic expression are substituted with value 0.


