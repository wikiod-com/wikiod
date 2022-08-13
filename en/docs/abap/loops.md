---
title: "Loops"
slug: "loops"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

When looping over internal tables, it is generally preferable to `ASSIGN` to a field symbol rather than loop `INTO` a work area. Assigning field symbols simply updates their reference to point to the next line of the internal table during each iteration, whereas using `INTO` results in the line of the table being copied into the work area, which can be expensive for long/wide tables.

## Internal Table Loop
    LOOP AT itab INTO wa.
    ENDLOOP.

    FIELD-SYMBOLS <fs> LIKE LINE OF itab.
    LOOP AT itab ASSIGNING <fs>.
    ENDLOOP.

    LOOP AT itab ASSIGNING FIELD-SYMBOL(<fs>).
    ENDLOOP.

    LOOP AT itab REFERENCE INTO dref.
    ENDLOOP.

    LOOP AT itab TRANSPORTING NO FIELDS.
    ENDLOOP.

**Conditional Looping**

If only lines that match a certain condition should be taken into the loop, addition `WHERE` can be added.

    LOOP AT itab INTO wa WHERE f1 = 'Max'.
    ENDLOOP.



## While Loop
ABAP also offers the conventional `WHILE`-Loop which runs until the given expression evaluates to false. The system field `sy-index` will be increased for every loop step.

    WHILE condition.
    * do something
    ENDWHILE 

## Do Loop
Without any addition the `DO`-Loop runs endless or at least until it gets explicitly exited from inside.
The system field `sy-index` will be increased for every loop step.

    DO.
    * do something... get it?
    * call EXIT somewhere
    ENDDO.

The `TIMES` addition offers a very convenient way to repeat code (`amount` represents a value of type `i`).

    DO amount TIMES.
    * do several times
    ENDDO.



## General Commands
To break loops, the command `EXIT` can be used.
    
    DO.
        READ TABLE itab INDEX sy-index INTO DATA(wa).
        IF sy-subrc <> 0.
            EXIT. "Stop this loop if no element was found
        ENDIF.    
        " some code
    ENDDO.

To skip to the next loop step, the command `CONTINUE` can be used.

    DO.
        IF sy-index MOD 1 = 0.
            CONTINUE. " continue to next even index
        ENDIF.
        " some code  
    ENDDO.


The `CHECK` statement is a `CONTINUE` with condition. If the condition turns out to be **false**, `CONTINUE` will be executed. In other words: *The loop will only carry on with the step if the condition is true*.

This example of `CHECK` ...
    
    DO.
        " some code
        CHECK sy-index < 10.        
        " some code
    ENDDO.

... is equivalent to ...

    DO.
        " some code
        IF sy-index >= 10.
            CONTINUE.
        ENDIF.
        " some code
    ENDDO.

