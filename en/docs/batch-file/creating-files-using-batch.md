---
title: "Creating Files using Batch"
slug: "creating-files-using-batch"
draft: false
images: []
weight: 9977
type: docs
toc: true
---

One useful feature of batch files is being able to create files with them. This section shows how to create files using batch code. 

## Syntax
 - echo (type here whatever you want in the to be) >> (filename)  
 - echo (variable name) >> (filename)

If a file exists, `>` will overwrite the file and `>>` will append to the end of the file. If a file does not exist, both will create a new file.

Also, the `echo` command automatically adds a newline after your string.

So

    echo 1 > num.txt
    echo 1 > num.txt 
    echo 2 >> num.txt 

will create the following file:

    1
    2

Not this:

    1 1 2

or 

    1 2

Furthermore, you cannot just modify a single line in a text file. You have to read the whole file, modify it in your code and then write to the whole file again.

## Echo to create files
Ways to create a file with the echo command:

   

    ECHO. > example.bat (creates an empty file called "example.bat")
    
    ECHO message > example.bat (creates example.bat containing "message")
    ECHO message >> example.bat (adds "message" to a new line in example.bat)
    (ECHO message) >> example.bat (same as above, just another way to write it)
    
If you want to create a file via the `ECHO` command, in a specific directory on your computer, you might run into a problem.

    ECHO Hello how are you? > C:\Program Files\example.bat

    (This will NOT make a file in the folder "Program Files", and might show an error message)

But then how do we do it? Well it's actually extremely simple... When typing a path or file name that has a space included in it's name, then remember to use "quotes"

    ECHO Hello how are you? > "C:\Program Files\example.bat"
    (This will create "example.bat" in the folder "Program Files")

But what if you want to make a file that outputs a new file?

    ECHO message > file1.bat > example.bat

    (example.bat is NOT going to contain "message > file1.bat")
    example.bat will just contain "message"... nothing else
    

Then how do we do this? Well for this we use the `^` symbol.

    ECHO message ^> file1.bat > example.bat
    
    (example.bat is going to contain "message > file1.bat")

Same goes for other stuff in batch

**The next step requires you to have some knowledge about variables and statements:**

**[click here to learn about variables][1] | [click here to learn about if and else statements][2]**


Variables:

    SET example="text"
    ECHO %example% > file.bat
    (This will output "text" to file.bat)

if we don't want it to output "text" but just plain %example% then write:

    ECHO ^%example^% > file.bat
    (This will output "%example%" to file.bat)

IF/ELSE statements:

    ELSE statements are written with "pipes" ||

    IF ^%example^%=="Hello" ECHO True || ECHO False > file.bat
    
    (example.bat is going to contain "if %example%=="Hello" echo True")
    [it ignores everything after the ELSE statement]

to output the whole line then we do the same as before.

    IF ^%example^%=="Hello" ECHO True ^|^| ECHO False > file.bat
    
    This will output:
    IF %example%=="Hello" ECHO True || ECHO False

If the variable is equal to "Hello" then it will say "True", ELSE it will say "False"


  [1]: https://www.wikiod.com/batch-file/variables-in-batch-files
  [2]: https://www.tutorialspoint.com/batch_script/batch_script_if_else_statement.htm

## Redirection
Format:

    [command] [> | >>] [filename]

`>` *saves* the output of [command] into [filename]. 

`>>` *appends* the output of [command] into [filename]. 

Examples:

1) `echo Hello World > myfile.txt` saves "Hello World" into myfile.txt

2) `echo your name is %name% >> myfile.txt` appends "your name is xxxx" into myfile.txt

3) `dir C:\ > directory.txt` saves the directory of C:\ to directory.txt



## Saving the output of many commands
Using many `ECHO` commands to create a batch file:

    (
      echo echo hi, this is the date today
      echo date /T
      echo echo created on %DATE%
      echo pause >nul
    )>hi.bat

The command interpreter treats the whole section in parenthesis as a single command, then saves all the output to `hi.bat`.

`hi.bat` now contains:

    echo hi, this is the date today
    date /T
    echo created on [date created]
    pause >nul


