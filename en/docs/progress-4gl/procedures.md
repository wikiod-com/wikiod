---
title: "Procedures"
slug: "procedures"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

There are two types of procedures in Progress ABL: internal procedures and procedure prototypes that are facades to Windows dlls or Unix/Linux shared library procedures.

Just like with functions, procedures cannot be nested. You cannot nest functions in procedures and vice versa.

A procedure is called with the `RUN` statement.

## Syntax
- RUN procedurename. //Runs a procedure called procedurename.
- RUN proc1(INPUT "HELLO"). //Inputs the string HELLO to proc1
- RUN proc2(INPUT var1, output var2). //Inputs var1 and outputs var2 to/from proc2
- RUN proc3(input "name = 'joe'", OUTPUT TABLE ttResult). //Inputs name=joe and outputs records in a table

- PROCEDURE proc: // Declares a procedure named proc
- END PROCEDURE. // Ends the current procedure

## A basic internal procedure
Unlike functions, there's no need to forward declare a procedure. It can be placed anywhere in your code, before or after you call it using `RUN`.

    RUN proc.
    
    //Procedure starts here
    PROCEDURE proc:
    
    //Procedure ends here
    END PROCEDURE. 

The procedure name is folowed by a colon sign telling us that this is the start of a block. The block ends with `END PROCEDURE.` (but this can be replaced with simply `END.`).



## INPUT and OUTPUT parameters
A procedure can have parameters of different kinds: input, output, input-output (bidirectional) and also some special types like temp-tables and datasets).

In the run statement it's optional to declare `INPUT` (it's considered default) - all other directions must be specifically declared.

A procedure taking two integers as input and outputting a decimal.

    PROCEDURE divideAbyB:
        DEFINE INPUT  PARAMETER piA       AS INTEGER     NO-UNDO.
        DEFINE INPUT  PARAMETER piB       AS INTEGER     NO-UNDO.
        DEFINE OUTPUT PARAMETER pdeResult AS DECIMAL     NO-UNDO.
    
        pdeResult = piA / piB.
    
    END PROCEDURE.
    
    DEFINE VARIABLE de AS DECIMAL     NO-UNDO.
    
    RUN divideAbyB(10, 2, OUTPUT de).
    
    DISPLAY de. //5.00

Parameters are totally optional. You can mix and match any way you want. The order of the parameters are up to you but it's handy to start with input and end with output - you have to put them in the right order in the run statement and mixing directions can be annoying.


    


## Recursion - see recursion
Recursion is easy - `RUN` the procedure itself from inside the procedure. However if you recurse too far the stack will run out of space.

A procedure calculation the factorial.

    PROCEDURE factorial:
        DEFINE INPUT  PARAMETER piNum AS INTEGER     NO-UNDO.
        DEFINE OUTPUT PARAMETER piFac AS INTEGER     NO-UNDO.
    
        DEFINE VARIABLE iFac AS INTEGER     NO-UNDO.
    
        IF piNum = 1 THEN DO:
            pifac = 1.
        END.
        ELSE DO:
            RUN factorial(piNum - 1, OUTPUT iFac).
            piFac = piNum * iFac.
        END.
    
    END PROCEDURE.
    
    DEFINE VARIABLE f AS INTEGER     NO-UNDO.
    
    RUN factorial(7, OUTPUT f).
    
    DISPLAY f.

## Scope
The procedure has it's own scope. The outside scope will "bleed" into the procedure but not the other way arround.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO INIT 1.
    DEFINE VARIABLE j AS INTEGER     NO-UNDO.
    
    PROCEDURE p:
    
        MESSAGE i VIEW-AS ALERT-BOX. // 1
        MESSAGE j VIEW-AS ALERT-BOX. // 0
    
        j = 2.
    
    END PROCEDURE.
    
    RUN p.
    
    MESSAGE i VIEW-AS ALERT-BOX. // 1
    MESSAGE j VIEW-AS ALERT-BOX. // 2

Declaring a variable inside a procedure that has the same name as a parameter on the outside will only create a local variable.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO INIT 1.
    DEFINE VARIABLE j AS INTEGER     NO-UNDO.
    
    PROCEDURE p:
        
        DEFINE VARIABLE i AS INTEGER     NO-UNDO INIT 5.
    
        MESSAGE i VIEW-AS ALERT-BOX. // 5
        MESSAGE j VIEW-AS ALERT-BOX. // 0
    
        j = 2.
    
    END PROCEDURE.
    
    RUN p.
    
    MESSAGE i VIEW-AS ALERT-BOX. // 1
    MESSAGE j VIEW-AS ALERT-BOX. // 2

Any variable created on the inside of a procedure is accessible to that procedure only.

This will generate a compiler error:

    PROCEDURE p:
        
        DEFINE VARIABLE i AS INTEGER     NO-UNDO INIT 5.
    
    END PROCEDURE.
    
    RUN p.
    
    MESSAGE i VIEW-AS ALERT-BOX. // Unknown Field or Variable name i - error 201



