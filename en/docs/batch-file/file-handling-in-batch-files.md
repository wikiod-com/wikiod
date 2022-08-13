---
title: "File Handling in batch files"
slug: "file-handling-in-batch-files"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

In this topic you will learn how to create, edit, copy, move, and delete files in batch.

## Creating a File in Batch
There may be multiple reason why you want to create a text file in batch. 
But whatever the reason may be, this is how you do it.

If you want to overwrite an existing text file use `>`.
Example:

    @echo off
    echo info to save > text.txt
    
But if you want to append text to an already existing text file use `>>`.
Example:
    
    @echo off
    echo info to save >> text.txt

If you need to save multiple lines of text to a file use `()>text.txt`
Example:

    @echo off
    (echo username
     echo password)>text.txt



## How to Copy Files in Batch
You may want to copy files from one place to another. In this example we'll teach you.

You can use the command `xcopy`. The syntax is `xcopy c:\From C:\To`

Example:

    @echo off
    xcopy C:\Folder\text.txt C:\User\Username\Desktop

There are also switches you can use. If you want to view them open up command prompt by `Start Menu -> Accessories -> Command Prompt` and then type `xcopy /?`

## Moving Files
Using the move command, you can move files:

    @echo off
    cd C:\Foo\Bat\Baz
    move /Y Meow.bat "Meow Folder" >nul

`Meow.bat` stands for which file to move. The "Meow Folder" moves `Meow.bat` to the `Meow Folder`. `/Y` says to not prompt for confirmation. Replacing that with `/-Y` makes the batch file prompt for confirmation. The `>nul` hides the command output. If it didn't have `>nul`, it would output: 
    
        1 File Moved



## Deleting Files
Using the `DEL`(alias for `ERASE`) command, one can remove files.

    @echo off
    del foo.ext

This command will delete `foo.ext` from the current directory. One can also specify path and file, such as:

    del C:\Foo\Bar\Baz.ext

But it is always ideal to put quotes (`"`) around paths, see [here][1] for the reason.

---

There are a few flags available for `DEL`.

| Flag | Function |
| ------ | ------ |
| `/P`   | Prompts user before deleting file(s)   |
| `/F`   | Forcefully remove read-only file(s) |
| `/S`   | Remove file(s) in subdirectories |
| `/Q`   | Prevents the user prompt |
| `/A`   | Filter: Only remove specific attributed file,
|        |         using the `-` character means **not** attributed as that type.




  [1]: https://www.wikiod.com/batch-file/best-practices#Quotes

## Copy Files Without xcopy
In [this example][1], user BoeNoe showed how to use the command `xcopy` to copy files. There is also an extra command called `copy`.

Here is a simple example:

    copy foo.ext bar.ext

This copies `foo.ext` to `bar.ext`, and create `bar.ext` when it doesn't exist. We can also specify paths to the file, but it is always ideal to put quotes (`"`) around paths, see [here][1] for the reason.

---

There are also many flags available for `copy`, see `copy /?` or `help copy` on a command prompt to see more.




  [1]: https://www.wikiod.com/batch-file/file-handling-in-batch-files#How to Copy Files in Batch

## Editing Nth Line of a File
Batch file does not come with a built-in method for replacing `n`th line of a file except `replace` and `append`(`>` and `>>`). Using `for` loops, we can emulate this kind of function.

---
    @echo off
    set file=new2.txt

    call :replaceLine "%file%" 3 "stringResult"

    type "%file%"
    pause
    exit /b
    
    :replaceLine <fileName> <changeLine> <stringResult>
    setlocal enableDelayedExpansion
    
    set /a lineCount=%~2-1

    for /f %%G in (%~1) do (
        if !lineCount! equ 0 pause & goto :changeLine
        echo %%G>>temp.txt
        set /a lineCount-=1
    )
    
    :changeLine
    echo %~3>>temp.txt
    
    for /f "skip=%~2" %%G in (%~1) do (
        echo %%G>>temp.txt
    )

    type temp.txt>%~1
    del /f /q temp.txt
    
    endlocal
    exit /b

- The main script calls the function `replaceLine`, with the filename/ which line to change/ and the string to replace.

- Function receives the input 
  - It loops through all the lines and `echo` them to a temporary file before the replacement line
  - It `echo`es the replacement line to the file
  - It continues to output to rest of the file
  - It copies the temporary file to the original file
  - And removes the temporary file.

- The main script gets the control back, and `type` the result.

