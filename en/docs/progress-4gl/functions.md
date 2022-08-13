---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9972
type: docs
toc: true
---

A user defined function in Progress ABL is a reusable program module.

* A function must be declared in the "main" procedure. It cannot be declared inside a procedure or inside another function. 
* A function in Progress ABL isn't a "first class citizen" unlike in programming languages like Haskell or  Javascript. You cannot pass a function as an input or output parameter. You can however invioke them dynamically using `DYNAMIC-FUNCTION` or the `CALL` object.
* Calling functions in your queries can lead to bad performance since index matching will hurt. Try to assign the value of the function to a variable and use that variable in the `WHERE`-clause instead.

## Simple function
    /* This function returns TRUE if input is the letter "b" and false otherwise */
    FUNCTION isTheLetterB RETURNS LOGICAL (INPUT pcString AS CHARACTER):
      IF pcString = "B" THEN 
        RETURN TRUE.
      ELSE 
        RETURN FALSE.
    END FUNCTION.
    
    /* Calling the function with "b" as input - TRUE expected */
    DISPLAY isTheLetterB("b").
    
    /* Calling the function with "r" as input - FALSE expected */
    DISPLAY isTheLetterB("r").

Parts of the syntax is actually not required:

    /* RETURNS isn't required, INPUT isn't required on INPUT-parameters */
    FUNCTION isTheLetterB LOGICAL (pcString AS CHARACTER):
      IF pcString = "B" THEN 
        RETURN TRUE.
      ELSE 
        RETURN FALSE.
    /* END FUNCTION can be replaced with END */
    END.



## Forward declaring functions
A function can be forward declared, this is similar to specifications in a C header file. 
That way the compiler knows that a function will be made available later on.

Without forward declarations the function MUST be declared before it's called in the 
code. The forward declaration consists of the `FUNCTION` specification (function name, return type and parameter data types and order). If the forward declaration doesn't match the actual function the compiler will produce errors and the code will fail to run. 

    FUNCTION dividableByThree LOGICAL (piNumber AS INTEGER) FORWARD.
    
    DISPLAY dividableByThree(9).
    
    FUNCTION dividableByThree LOGICAL (piNumber AS INTEGER):
    
        IF piNumber MODULO 3 = 0 THEN
            RETURN TRUE.
        ELSE 
            RETURN FALSE.
    END.

## Multiple input parameters
/* This will popup a message-box saying "HELLO WORLD" */

    FUNCTION cat RETURNS CHARACTER ( c AS CHARACTER, d AS CHARACTER):
    
        RETURN c + " " + d.
    
    END.
    
    MESSAGE cat("HELLO", "WORLD") VIEW-AS ALERT-BOX.



## Multiple return statements (but a single return value)
A function can have multiple return statements and they can be placed in different parts of the actual function. They all need to return the same data type though.


    FUNCTION returning DATE ( dat AS DATE):
        IF dat < TODAY THEN DO:
            DISPLAY "<".
            RETURN dat - 200.
        END.
        ELSE DO:
            DISPLAY ">".
            RETURN TODAY. 
        END.
    END.
    
    MESSAGE returning(TODAY + RANDOM(-50, 50)) VIEW-AS ALERT-BOX.



A function actually don't have to return anything at all. Then it's return value will be ? (unknown). The compiler will not catch this (but your colleagues will so avoid it).


    /* This function will only return TRUE or ?, never FALSE, so it might lead to troubles */
    FUNCTION inTheFuture LOGICAL ( dat AS DATE):
        IF dat > TODAY THEN DO:
            RETURN TRUE.
        END.
    END.
    MESSAGE inTheFuture(TODAY + RANDOM(-50, 50)) VIEW-AS ALERT-BOX.








## Output and input-output parameters
A function can only return a single value but there's one way around that: the parameters are not limited to input parameters. You can declare `INPUT`, `OUTPUT` and `INPUT-OUTPUT` parameters. 

Unlike `INPUT` parameters you must specify `OUTPUT` or `INPUT-OUTPUT` before the parameters.

Some coding conventions might not like this but it can be done.


    /* Function will add numbers and return a sum (AddSomSumbers(6) = 6 + 5 + 4 + 3 + 2 + 1 = 21 */
    /* It will also have a 1% per iteration of failing                                           */
    /* To handle that possibility we will have a status output parameter                         */
    FUNCTION AddSomeNumbers INTEGER ( INPUT number AS INTEGER, OUTPUT procstatus AS CHARACTER):
        
        procStatus = "processing".
    
        DEFINE VARIABLE i AS INTEGER     NO-UNDO.
        DEFINE VARIABLE n AS INTEGER     NO-UNDO.
        /* Iterate number times */
        DO i = 1 TO number:
            /* Do something */
    
            n = n + i.
            
            /* Fake a 1% chance for an error that breaks the function */
            IF RANDOM(1,100) = 1 THEN
                RETURN 0.
        END.
    
        procStatus = "done".
        RETURN n.
    END.
    
    
    DEFINE VARIABLE ret   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE stat  AS CHARACTER   NO-UNDO.
    
    /* Call the function */
    ret = AddSomeNumbers(30, OUTPUT stat).
    
    /* If "stat" is done we made it! */
    IF stat = "done" THEN DO:
        MESSAGE "We did it! Sum:" ret VIEW-AS ALERT-BOX.
    END.
    ELSE DO:
        MESSAGE "An error occured" VIEW-AS ALERT-BOX ERROR.
    END.

Here's an example of an `INPUT-OUTPUT` parameter:

    /* Function doubles a string and returns the length of the new string */
    FUNCTION doubleString RETURN INTEGER (INPUT-OUTPUT str AS CHARACTER).
    
        str = str + str.
    
        RETURN LENGTH(str).
    
    END.
    
    DEFINE VARIABLE str AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE len AS INTEGER     NO-UNDO.
    
    str = "HELLO".
    
    len = doubleString(INPUT-OUTPUT str).
    
    MESSAGE 
        "New string: " str SKIP
        "Length: " len VIEW-AS ALERT-BOX.


## Recursion
*See recursion*

A function can call itself and thereby recurse.

    FUNCTION factorial INTEGER (num AS INTEGER).
    
        IF num = 1 THEN 
            RETURN 1.
        ELSE 
            RETURN num * factorial(num - 1).
    
    END FUNCTION.
    
    DISPLAY factorial(5).

With standard settings (startup parameter) the Progress session wont be able to handle very large numbers in this example. `factorial(200)` will fill the stack and raise an error.


## Dynamic call of a function
Using `DYNAMIC-FUNCTION` or the `CALL`-object you can dynamically call functions.

    DEFINE VARIABLE posY      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE posX      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE OKkeys    AS CHARACTER   NO-UNDO INIT "QLDRUS".
    DEFINE VARIABLE Step      AS INTEGER     NO-UNDO INIT 1.
    DEFINE VARIABLE moved     AS LOGICAL     NO-UNDO.
    /* Set original position */
    posY = 10.
    posX = 10.
    
    /* Move up (y coordinates - steps ) */
    FUNCTION moveU LOGICAL (INPUT steps AS INTEGER):
    
        IF posY = 0 THEN
            RETURN FALSE.
    
        posY = posY - steps.
    
        IF posY < 0 THEN
            posY = 0.
    
        RETURN TRUE.
    END FUNCTION.
    
    /* Move down (y coordinates + steps ) */
    FUNCTION moveD LOGICAL (INPUT steps AS INTEGER):
    
        IF posY = 20 THEN
            RETURN FALSE.
    
        posY = posY + steps.
    
        IF posY > 20 THEN
            posY = 20.
            
    END FUNCTION.
    
    /* Move left (x coordinates - steps ) */
    FUNCTION moveL LOGICAL (INPUT steps AS INTEGER):
    
        IF posX = 0 THEN
            RETURN FALSE.
    
        posX = posX - steps.
    
        IF posX < 0 THEN
            posX = 0.
    
        RETURN TRUE.
    END FUNCTION.
    
    /* Move down (x coordinates + steps ) */
    FUNCTION moveR LOGICAL (INPUT steps AS INTEGER):
    
        IF posX = 20 THEN
            RETURN FALSE.
    
        posX = posX + steps.
    
        IF posX > 20 THEN
            posX = 20.
            
    END FUNCTION.
    
    
    REPEAT:
    
        DISPLAY posX posY step WITH FRAME x1 1 DOWN.
        READKEY.
    
        IF INDEX(OKKeys, CHR(LASTKEY)) <> 0 THEN DO:
            IF CHR(LASTKEY) = "q"  THEN LEAVE.
            IF CAPS(CHR(LASTKEY)) = "s" THEN UPDATE step WITH FRAME x1.
            ELSE DO:
                moved = DYNAMIC-FUNCTION("move" + CAPS(CHR(LASTKEY)), INPUT step).
                IF moved = FALSE THEN
                    MESSAGE "Out of bounds".
            END.
        END.
    END.


The `CALL` object is  not as lightweight as the DYNAMIC-FUNCTION. It can be used to call different things: functions, procedures, external program, Windows DLL-functions. It can also invoke methods on objects and access getters/setters.

    DEFINE VARIABLE functionHandle AS HANDLE      NO-UNDO.
    DEFINE VARIABLE returnvalue    AS CHARACTER   NO-UNDO.
    
    FUNCTION isPalindrome LOGICAL (INPUT txt AS CHARACTER, OUTPUT txtBackwards AS CHARACTER):
        DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
        DO i = LENGTH(txt) TO 1 BY -1:
            txtBackwards = txtBackwards + SUBSTRING(txt, i, 1).
        END.
    
        IF txt = txtBackwards THEN
            RETURN TRUE.
        ELSE 
            RETURN FALSE.
    
    END FUNCTION.
    
    CREATE CALL functionHandle.
    functionHandle:CALL-NAME      = "isPalindrome".
    /* Sets CALL-TYPE to the default */
    functionHandle:CALL-TYPE  = FUNCTION-CALL-TYPE.
    functionHandle:NUM-PARAMETERS = 2.
    functionHandle:SET-PARAMETER(1, "CHARACTER", "INPUT", "HELLO WORLD").
    functionHandle:SET-PARAMETER(2, "CHARACTER", "OUTPUT", returnvalue). 
    functionHandle:INVOKE.
    
    MESSAGE "Text backwards: " returnvalue "Is it a palindrome? " functionHandle:RETURN-VALUE VIEW-AS ALERT-BOX.
    
    DELETE OBJECT functionHandle.
    
    
    CREATE CALL functionHandle.
    functionHandle:CALL-NAME      = "isPalindrome".
    /* Sets CALL-TYPE to the default */
    functionHandle:CALL-TYPE  = FUNCTION-CALL-TYPE.
    functionHandle:NUM-PARAMETERS = 2.
    functionHandle:SET-PARAMETER(1, "CHARACTER", "INPUT", "ANNA").
    functionHandle:SET-PARAMETER(2, "CHARACTER", "OUTPUT", returnvalue). 
    functionHandle:INVOKE.
    
    MESSAGE "Text backwards: " returnvalue "Is it a palindrome? " functionHandle:RETURN-VALUE VIEW-AS ALERT-BOX.
    
    DELETE OBJECT functionHandle.

 





