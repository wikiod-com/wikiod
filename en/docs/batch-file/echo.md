---
title: "Echo"
slug: "echo"
draft: false
images: []
weight: 9970
type: docs
toc: true
---

`echo` can be used to control and produce output.

## Syntax
 - ECHO [ON | OFF]
 - ECHO message
 - ECHO(message
 - ECHO(

## Parameters
| Parameter |                         Details                         |
| --------- | ------------------------------------------------------- |
| ON \| OFF | Can either be `ON` or `OFF` (case insensitive)          |
|  message  | Any string (except `ON` or `OFF` when used without `(`) |

  * `echo.` will also display an empty string. However, this is slower than `echo(` as `echo.` will search for a file named "echo". Only if this file does not exist will the command work, but this check makes it slower.
  * `echo:` will behave just like `echo(`, unless `message` looks like a file path, e.g. `echo:foo\..\test.bat`. In this case, the interpreter will see `echo:foo` as a folder name, strip `echo:foo\..\` (because it appears just to enter the directory `echo:foo` then leave it again) then execute `test.bat`, which is not the desired behaviour.

## Displaying Messages
To display "Some Text", use the command:

    echo Some Text

This will output the string `Some Text` followed by a new line.

To display the strings `On` and `Off` (case insensitive) or the empty string, use a `(` instead of white-space:

    echo(ON
    echo(
    echo(off

This will output:

>     ON
>     
>     off

It is also common to use `echo.` to output a blank line, but please see the remarks for why this is not the best idea.

To display text without including a CR/LF, use the following command:

    <nul set/p=Some Text

This command will attempt to set the variable called the empty string to the user input following a prompt. The `nul` file is redirected to the command with `<nul`, so the command will give up as soon as it tries to read from it and only the prompt string will be left. Because the user never typed a new line, there is no linefeed.

## Echo output to file
Ways to create a file with the echo command:

    echo. > example.bat (creates an empty file called "example.bat")

    echo message > example.bat (creates example.bat containing "message")
    echo message >> example.bat (adds "message" to a new line in example.bat)
    (echo message) >> example.bat (same as above, just another way to write it)

**Output to path**

A little problem you might run into when doing this:

    echo Hello how are you? > C:\Users\Ben Tearzz\Desktop\example.bat

    (This will NOT make a file on the Desktop, and might show an error message)
But then how do we do it? Well it's actually extremely simple... When typing a path or file name that has a space included in it's name, then remember to use "quotes"

    echo Hello how are you? > "C:\Users\Ben Tearzz\Desktop\example.bat"
    (This will make a file on MY Desktop)

But what if you want to make a file that outputs a new file?

    echo message > file1.bat > example.bat
    
    (This is NOT going to output:
    "message > file1.bat" to example.bat

Then how do we do this?

    echo message ^> file1.bat > example.bat
    
    (This will output: 
    "message > file1.bat" to example.bat

Same goes for other stuff in batch

**If you haven't learned what variables and statements are, then you most likely won't understand the following: [click here to learn about variables][1] | [click here to learn about "if" statements][2]**

Variables:

    set example="text"
    echo %example% > file.bat
    (This will output "text" to file.bat)

if we don't want it to output "text" but just plain %example% then write:

    echo ^%example^% > file.bat
    (This will output "%example%" to file.bat)

else statements:

    else = ||
    if ^%example^%=="Hello" echo True || echo False > file.bat
    
    (This will output:
    if %example%=="Hello" echo True
to output the whole line we write:

    if ^%example^%=="Hello" echo True ^|^| echo False > file.bat
    
    This will output:
    if %example%=="Hello" echo True || echo False
If the variable is equal to "Hello" then it will say "True", else it will say "False"

 


  [1]: https://www.wikiod.com/batch-file/variables-in-batch-files
  [2]: https://www.wikiod.com/batch-file/if-statements

## @Echo off
`@echo off` prevents the prompt and contents of the batch file from being displayed, so that only the output is visible. The `@` makes the output of the `echo off` command hidden as well.

## Echo Setting
The echo setting determines whether command echoing is on or off. This is what a sample program looks like with command echoing **on** (default):

    C:\Windows\System32>echo Hello, World!
    Hello, World!
    
    C:\Windows\System32>where explorer
    C:\Windows\System32\explorer.exe

    C:\Windows\System32>exit

This is what it looks like with echo **off**:

    Hello, World!
    C:\Windows\System32\explorer.exe

Getting and Setting
===================

To get (display) the echo setting, use `echo` with no parameters:

    > echo
    ECHO is on.

To set the echo setting, use `echo` with `on` or `off`:

    > echo off
    
    > echo
    ECHO is off.
    
    > echo on
    
    > echo
    ECHO is on.

Note that with these examples, the prompt has been represented by `> `. When changing the echo setting, the prompt will appear and disappear, but that makes for unclear examples.

Note that it is possible to prevent a command from being echoed even when echo is on, by placing an `@` character before the command.


## Echo outputs everything literally
Quotes will be output as-is:

    echo "Some Text"

>     "Some Text"

Comment tokens are ignored:

    echo Hello World REM this is not a comment because it is being echoed!

>     Hello World REM this is not a comment because it is being echoed!

However, `echo` will still output variables - see [Variable Usage](https://www.wikiod.com/batch-file/variables-in-batch-files#Usage) - and special characters still take effect:


    echo hello && echo world

>     hello
>     world


## Turning echo on inside brackets
In the following example `echo on` will take effect after the end of the brackets context is reached:

    @echo off
    (
        echo on
        echo ##
    )
    echo $$

In order to "activate" echo on in a brackets context (including `FOR` and `IF` commands) you can use [FOR /f macro][1] :

    @echo off
    setlocal
    
    :: echo on macro should followed by the command you want to execute with echo turned on
    set "echo_on=echo on&for %%. in (.) do"
    
    (
      %echo_on% echo ###
    )


  [1]: https://stackoverflow.com/questions/34853229/why-isnt-echo-on-off-working-within-a-batch-file-if-block


