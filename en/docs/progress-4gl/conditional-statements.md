---
title: "Conditional statements"
slug: "conditional-statements"
draft: false
images: []
weight: 9993
type: docs
toc: true
---

Progress ABL supports two contitional statements: `IF/THEN/ELSE` and `CASE`.

## IF ... THEN ... ELSE-statement
In the `IF THEN ELSE` statement the result can be either a single statement:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    IF i = 0 THEN
        MESSAGE "Zero".
    ELSE 
        MESSAGE "Something else".

Or a block, for instance by adding a `DO`-block:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    IF i = 0 THEN DO:
        RUN procedure1.
        RUN procedure2.
    END.
    ELSE DO: 
        RUN procedure3.
        RUN procedure4.
    END.

Several `IF`-statements can be nested with the `ELSE IF`-syntax:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    
    IF i = 0 THEN DO:
        RUN procedure1.
        RUN procedure2.
    END.
    ELSE IF i = 1 THEN DO:
        RUN procedure3.
        RUN procedure4.
    
    END.
    ELSE DO: 
        RUN procedure5.
        RUN procedure6.
    END.

The `ELSE`-part is not mandatory:

    DEFINE VARIABLE l AS LOGICAL     NO-UNDO.
    
    l = TRUE.
    
    IF l = TRUE THEN DO:
        MESSAGE "The l variable has the value TRUE" VIEW-AS ALERT-BOX.
    END.

The `IF`/`ELSE IF` can compare several conditionals, with or without internal connections. This leaves you free to mess up your code in several ways:

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE l AS LOGICAL     NO-UNDO.
    
    IF i < 30 OR l = TRUE THEN DO:
        
    END.
    ELSE IF i > 30 AND l = FALSE OR TODAY = DATE("2017-08-20") THEN DO:
    
    END.
    ELSE DO:
        MESSAGE "I dont really know what happened here".
    END.



## CASE
The `CASE`-statement is a lot more strict than the `IF/ELSE`-conditional. It can only compare a single variable and only equality, not larget/smaller than etc.

DEFINE VARIABLE c AS CHARACTER   NO-UNDO.

    CASE c:
        WHEN "A" THEN DO:
            RUN procedureA.
        END.
        WHEN "B" THEN DO:
            RUN procedureB.
        END.
        OTHERWISE DO:
            RUN procedureX.
        END.
    END CASE.

Using an `OR` each `WHEN` can compare different values:

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    CASE c:
        WHEN "A" THEN DO:
            RUN procedureA.
        END.
        WHEN "B" OR WHEN "C" THEN DO:
            RUN procedureB-C.
        END.
        OTHERWISE DO:
            RUN procedureX.
        END.
    END CASE.

Just like with the `IF`-statement each branch can either be a single statement or a block. Just like with the `ELSE`-statement, `OTHERWISE` is not mandatory.


    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    CASE c:
        WHEN "A" THEN
            RUN procedureA.
        WHEN "B" OR WHEN "C" THEN
            RUN procedureB-C.
    END CASE.

Unlike a c-style `switch`-clause there's no need to escape the `CASE`-statement - only one branch will be executed. If several `WHEN`s match only the first one will trigger. `OTHERWISE` must be last and will only trigger if none of the branches above match.

    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    c = "A".
    
    CASE c:
        WHEN "A" THEN
            MESSAGE "A" VIEW-AS ALERT-BOX. //Only "A" will be messaged
        WHEN "A" OR WHEN "C" THEN
            MESSAGE "A or C" VIEW-AS ALERT-BOX.
    END CASE.




## IF ... THEN ... ELSE-function
`IF THEN ELSE` can also be used like a function to return a single value. This is a lot like the ternary `?`-operator of C.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    /* Set c to "low" if i is less than 5 otherwise set it to "high"    
    c = IF i < 5 THEN "low" ELSE "high".

Using parenthesis can ease readability for code like this.

    DEFINE VARIABLE i AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c AS CHARACTER   NO-UNDO.
    
    c = (IF i < 5 THEN "low" ELSE "high").

The value of the `IF`-part and the value of the `ELSE`-part must be of the same datatype. It's not possible to use `ELSE IF` in this case.

    DEFINE VARIABLE dat                AS DATE        NO-UNDO.
    DEFINE VARIABLE beforeTheFifth     AS LOGICAL   NO-UNDO.
    
    dat = TODAY.
    
    beforeTheFifth = (IF DAY(dat) < 5 THEN TRUE ELSE FALSE).

Several comparisons can be done in the `IF`-statement:

    DEFINE VARIABLE between5and10 AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER     NO-UNDO INIT 7.

    between5and10 = (IF i >= 5 AND i <= 10 THEN TRUE ELSE FALSE).
    
    MESSAGE between5and10 VIEW-AS ALERT-BOX.





