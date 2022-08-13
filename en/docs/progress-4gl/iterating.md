---
title: "Iterating"
slug: "iterating"
draft: false
images: []
weight: 9998
type: docs
toc: true
---

There are several ways of iterating (looping) in Progress ABL. 

## DO WHILE
A `DO WHILE` loop will continue to loop unless the `WHILE`-part is met. This makes it easy to run forever and eat up all time from one CPU core.

> DO WHILE *expression*:
>
> END.
>
> *expression* is any combination of boolean logic, comparisons, variables, fields etc that evaluates to a true value. 

    /* This is a well defined DO WHILE loop that will run as long as i is lower than 10*/
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DO WHILE i < 10:
        i = i + 1.
    END.
    
    DISPLAY i. // Will display 10

You can use any number of checks in the `WHILE`-part:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DO WHILE TODAY = DATE("2017-02-06") AND RANDOM(1,100) < 99:
        i = i + 1.
    END.
    
    MESSAGE i "iterations done" VIEW-AS ALERT-BOX.

However, the compiler wont help you so check that the `WHILE`-part eventually is met:

    /* Oops. Didnt increase i. This will run forever... */
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DO WHILE i < 10:
        i = 1.
    END.
    




## DO var = start TO finish [BY step]
This iteration changes a value from a starting point to an end, optionally by a specified value for each step. The default change is 1.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    DO i = 10 TO 15:
        DISPLAY i WITH FRAME x1 6 DOWN .
        DOWN WITH FRAME x1.
    END.

Result:

    ---------------i
    
            10
            11
            12
            13
            14
            15

You can iterate over dates as well:

    DEFINE VARIABLE dat AS INTEGER        NO-UNDO.
    
    DO dat = TODAY TO TODAY + 3:
    
    END.

And over decimals. But then you most likely want to use `BY` - otherwise an `INTEGER` would have done just as fine...

    DEFINE VARIABLE de AS DECIMAL     NO-UNDO.
    
    DO de = 1.8 TO 2.6 BY 0.2:
        DISPLAY "Value" de.
    END.

Using `BY` a negative number you can also go from a higher to a lower value:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    DO i = 5 TO 1 BY -1:
        
    END.

The expression will be tested until it's no longer met. This makes the counter be higher (if moving upwards) or lower (if moving downwards) once the loop is finished:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    DO i = 5 TO 1 BY -1:
        
    END.
    
    MESSAGE i. // Will message 0

Another example:

    DEFINE VARIABLE da AS DATE     NO-UNDO.
    
    DISPLAY TODAY. //17/02/06
    DO da = TODAY TO TODAY + 10:
        
    END.
    DISPLAY da. //17/02/17 (TODAY + 11)



## REPEAT
REPEAT, will repeat itself forever unless you explicitly exit the loop:

    //Runs forever
    REPEAT:
        // Do stuff
    END.

To exit the loop you can use the `LEAVE` keyword. With or without a label. Without a label `LEAVE` will always effect the current loop. With a name you can specify what loop to `LEAVE`.

    /* Without a label */
    REPEAT:
      //Do stuff
      IF TRUE THEN LEAVE.
    END.

    /* With a label */
    loopLabel:
    REPEAT:
      //Do stuff
      IF <somecondition> THEN LEAVE loopLabel.
    END.
    
    /* Two nested REPEATS */
    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    loopLabelOne:
    REPEAT:
        loopLabelTwo:
        REPEAT:
            i = i + 1.
            IF RANDOM(1,100) = 1  THEN LEAVE loopLabelTwo.
            IF RANDOM(1,100) = 1  THEN LEAVE loopLabelOne.
        END.
        IF RANDOM(1,100) = 1  THEN LEAVE loopLabelOne.
    END.
    DISPLAY i.





