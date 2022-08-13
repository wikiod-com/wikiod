---
title: "Compiling"
slug: "compiling"
draft: false
images: []
weight: 9958
type: docs
toc: true
---

Compile Progress code as called "r-code" and is normally saved in a file with the extension .r. There are a couple of different ways of compiling: using the `COMPILE` statement or on Linux or AppBuilder: the built in Application Compiler. Developer Studio (the Eclipse environment) has compiling built into it's build process.


You must have 4GL Development or OpenEdge Studio installed to compile 4GL programs which update the database.

## Syntax
- COMPILE program.p SAVE. //Compile program.p and save it's r-code
- COMPILE VALUE(var) SAVE. //Compile the named saved in the variable "var" and save it's r-code
- COMPILE prog.p XREF prog.xref LISTING prog.list. //Compile prog.p and create xref and listing-files. Don't save the r-code.
- COMPILE program.p SAVE NO-ERROR. //Compile program.p, save r-code and supress errors to stop the execution.

## Application Compiler
**Windows AppBuilder**

In the Windows Appbuilder the Application Compiler is found in the Tools Menu.
[![Windows AppBuilder][1]][1]

**Procedure Editor (Linux - `pro` or Windows `pro.exe`**

In the Procedure Editor (both Linux and Windows) the Compiler if found in the Tools menu.
[![Linux Procedure Editor Editor][2]][2]


**Application Compiler**

[![Windows Application Compiler][3]][1]
[![Linux Application Compiler][4]][3]

Regardless of OS the functionality of the compiler is the same. You can add directories and/or files and compile them. 

Main settings (more below):
- Save new .r File. If not checked the files will simply be compiled but not saved. Useful for error tracking for instance.
- Look in Subdirectories. Otherwise subdirectories will have to be added.
- Remove old .r Files. Overwrite old .r file.
- Onlu Compile if No .r File. Only compiles uncompiled files.

Options:
- Propath - shows you the propath and let's you select directories to compile from it.
- Add - lets you input a directory or file.
- Modify - lets you modify an existing entry.
- Delete - Deletes an entry.
- Start Compile - Starts the compiler. Shortcut: <kbd>F2</kbd> 

The main menu choices:

- File -> Exit: Exits the compiler
- Compile -> Start Compile : Starts the compiler. Shortcut: <kbd>F2</kbd>
- Tools -> Access to other tools
- Option -> Compiler... : Settings, se below. 
- Help -> OpenEdge Help (Windows Only). Online help. Shortcut: <kbd>F1</kbd> 

**Settings**

[![Settings][5]][5]
- Default File Spec: Filename extensions to compile
- Message Log File: File to save messages, warnings and errors in
- Save into: Where to store .r file. If blank the same directory as the code.
- Languages: for translations. Not covered here.
- V6Frame: Old and unuseful...
- Steam-IO: If you want to print the compiler output. Most likely not.
- Listing File: If you want the compiler to create a listing file. Useful for debugging
- Append: add to the existing listing file. Otherwise overwrite.
- Page Width + Length: Format of listing file.
- Xref File: If you want the compiler to create a `XREF`. Useful for debugging, checking index usage etc.
- XML Format: If the compiler xref should be an xml. Otherwise "plain" text.
- Append: add to the existing xref file. Otherwise overwrite.
- Debug File: Debugger listing file.
- Encryption Key: If the source file is encrypted using `xcode` insert the key here.
- Minimize R-code Size: Remove some debugging information to keep the r-code small. 
- Generate MD-5: Mostly for WebClient compiling.

**Basic usage**

1. Start the compiler
2. Add a path (if not already saved from last session)
3. Press <kbd>F2</kbd> to compile.
4. Observe any errors.
5. Exit

  [1]: https://i.stack.imgur.com/U2HHj.png
  [2]: https://i.stack.imgur.com/9ZAwg.png
  [3]: https://i.stack.imgur.com/pSAhb.png
  [4]: https://i.stack.imgur.com/xnhCh.png
  [5]: https://i.stack.imgur.com/jf0ub.png

## COMPILE statement
The compile statement lets you compile programs in Progress ABL:

Basic usage:

    COMPILE hello-world.p SAVE.

With a variable:

    DEFINE VARIABLE prog AS CHARACTER   NO-UNDO.
    
    prog = "hello.p".
    
    COMPILE VALUE(prog) SAVE.

There are several options to the `COMPILE`-statement:

`SAVE` states that the .r-code should be saved for future use.

    COMPILE hello-world.p SAVE.

`SAVE INTO dir OR SAVE INTO VALUE(dir-variable)` saves the r-code in the specified directory:

    COMPILE hello-world.p SAVE INTO /usr/sources.

`LISTING file`. Creates a listing file containing debug information regarding blocks, includes etc.

    COMPILE program.p SAVE LISTING c:\temp\listing.txt.

Listing has a couple of options for appending files, page-size and page-width:

    APPEND PAGE-SIZE num PAGE-WIDTH num 

`XREF xreffile` will save a compiler xref file containing information about string and index usage etc. You can also `APPEND` this one.

    COMPILE checkFile.p SAVE XREF c:\directory\xref-file.txt.

`XREF-XML xreffile-or-dir` will do the same thing as `XREF` but save the file in an xml-format instead. If you use a directory the xref-file will be named `programname.xref.xml`.

    COMPILE file.p SAVE XREF c:\temp\.

`NO-ERROR` will supress any errors from stopping your program. 

    COMPILE program SAVE NO-ERROR.

`DEBUG-LIST file` generates a debug file with line numbers.

    COMPILE checkFile.p SAVE DEBUG-LIST c:\temp\debug.txt.

`PREPROCESS file` will first translate all preprocessors and then create a new .p-file with the code prior to compiling.

     COMPILE checkFile.p SAVE PREPROCESS c:\temp\PREPROC.txt.

`XCODE key` will compile an encrypted source code with `key` as key. You cannot use XCODE with the XREF, XREF-XML, STRING-XREF, or LISTING options together. 

    COMPILE program.p SAVE XCODE myKey.

You can combine several options:

    COMPILE prog.p SAVE INTO /usr/r-code XREF /usr/xrefs/xref.txt APPEND LISTING /usr/listings.txt APPEND NO-ERROR.



## COMPILER system handle
The `COMPILER` system handle let's you look at information regarding a recent compile. 

Assuming `ok-program.p` is a program without any errors or warning:

    COMPILE ok-program.p SAVE NO-ERROR.
    
    DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
    
    MESSAGE  
        "Errors: "   COMPILER:ERROR SKIP
        "Warnings: " COMPILER:WARNING SKIP
        "Messages: " COMPILER:NUM-MESSAGES
        VIEW-AS ALERT-BOX INFORMATION.

This will procude:

[![No errors][1]][1]

**Compiling  a program with a warning:**

    /* program-with-warning.p */
    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    c = "hello".
    DISPLAY c.
    //This RETURN makes the program exit here and the code below unreachable.
    RETURN.
    
    IF TRUE THEN DO:
        i = 10. 
    END.

Compiling the program:

    COMPILE program-with-warning.p SAVE.
    
    DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
    
    MESSAGE  
        "Errors: "   COMPILER:ERROR SKIP
        "Warnings: " COMPILER:WARNING SKIP
        "Messages: " COMPILER:NUM-MESSAGES
        VIEW-AS ALERT-BOX INFORMATION.
    
    DO iError = 1 TO COMPILER:NUM-MESSAGES:
        DISPLAY 
            COMPILER:GET-FILE-NAME(iError)    LABEL "Filename" FORMAT "x(20)"
            COMPILER:GET-MESSAGE(iError)      LABEL "Message"  FORMAT "x(50)"
            COMPILER:GET-NUMBER(iError)       LABEL "Msg#"  
            COMPILER:GET-ERROR-COLUMN(iError) LABEL "Column"
            COMPILER:GET-ERROR-ROW(iError)    LABEL "Row"
                WITH FRAME fr1 SIDE-LABELS 1 COLUMNS.
    END.

Result:

[![enter image description here][2]][2]

[![enter image description here][3]][3]

**Compiling a program with an error**

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    c = "hello".
    DISPLAY c.
    //Casting should be required below...
    IF TRUE THEN DO:
        i = c.
    END.

Compiling the program:

    //Use no-errors to supress any error messages from interrupting us.
    COMPILE c:\temp\program-with-error.p SAVE NO-ERROR.
    
    DEFINE VARIABLE iError AS INTEGER     NO-UNDO.
    
    MESSAGE  
        "Errors: "   COMPILER:ERROR SKIP
        "Warnings: " COMPILER:WARNING SKIP
        "Messages: " COMPILER:NUM-MESSAGES
        VIEW-AS ALERT-BOX INFORMATION.
    
    DO iError = 1 TO COMPILER:NUM-MESSAGES:
        DISPLAY 
            COMPILER:GET-FILE-NAME(iError)    LABEL "Filename" FORMAT "x(20)"
            COMPILER:GET-MESSAGE(iError)      LABEL "Message"  FORMAT "x(50)"
            COMPILER:GET-NUMBER(iError)       LABEL "Msg#"  
            COMPILER:GET-ERROR-COLUMN(iError) LABEL "Column"
            COMPILER:GET-ERROR-ROW(iError)    LABEL "Row"
                WITH FRAME fr1 SIDE-LABELS 1 COLUMNS 20 DOWN.
    
        DOWN WITH FRAME fr1.
    END.


Result, there's almost always two errors per error. "Could not understand" is followed by the actual error:
[![enter image description here][4]][4]

[![enter image description here][5]][5]



  [1]: https://i.stack.imgur.com/WN6o5.png
  [2]: https://i.stack.imgur.com/0maPd.png
  [3]: https://i.stack.imgur.com/pFTfD.png
  [4]: https://i.stack.imgur.com/LFyhw.png
  [5]: https://i.stack.imgur.com/O1TDz.png

