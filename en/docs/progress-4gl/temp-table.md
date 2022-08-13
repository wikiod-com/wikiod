---
title: "TEMP-TABLE"
slug: "temp-table"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

The `TEMP-TABLE` is a very powerful feature of Progress ABL. It's a temporary in-memory (mostly at least) table that can be used for writing complex logic. It can be used as input/output parameters to procedures, functions and other programs. One or more temp-tables can make up the foundation of a `DATASET` (often called ProDataset).

Almost anything that can be done with a native Progress database table can be done with a temp-table.

## Defining a simple temp-table
This is the definition of a `TEMP-TABLE` named ttTempTable with three fields. `NO-UNDO` indicates that no undo handling is needed (this is usually what you want to do unless you really need the opposite). 

    DEFINE TEMP-TABLE ttTempTable NO-UNDO
        FIELD field1 AS INTEGER
        FIELD field2 AS CHARACTER
        FIELD field3 AS LOGICAL.



## A temp-table with an index
Temp-tables can (and should) be created with indices if you plan to run queries against them.

This table has one index (index1) containing of one field (field1). This index is primary and unique (meaning not two records can have the same contents of field1).

    DEFINE TEMP-TABLE ttTempTable NO-UNDO
        FIELD field1 AS INTEGER
        FIELD field2 AS CHARACTER
        FIELD field3 AS LOGICAL
        INDEX index1 IS PRIMARY UNIQUE field1 .

## More indexes - indices...
You can define multiple indices for each temp-table. If you need them - define them. Basically an index matching your query and/or sort order will help performance!

    DEFINE TEMP-TABLE ttWithIndex NO-UNDO
        FIELD field1 AS INTEGER
        FIELD field2 AS CHARACTER
        FIELD field3 AS LOGICAL
        INDEX field1 field1.
    
    DEFINE TEMP-TABLE ttWithoutIndex NO-UNDO
        FIELD field1 AS INTEGER
        FIELD field2 AS CHARACTER
        FIELD field3 AS LOGICAL.

    DEFINE VARIABLE i              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWithCreate    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWithFind      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWithoutCreate AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iWithoutFind   AS INTEGER     NO-UNDO.
    
    ETIME(TRUE).
    DO i = 1 TO 1000:
        CREATE ttWithIndex.
        ttWithIndex.field1 = i.
    END.
    iWithCreate = ETIME.
    
    ETIME(TRUE).
    DO i = 1 TO 1000:
        CREATE ttWithoutIndex.
        ttWithoutIndex.field1 = i.
    END.
    iWithoutCreate = ETIME.
    
    RELEASE ttWithIndex.
    RELEASE ttWithoutIndex.
    
    ETIME(TRUE).
    DO i = 1 TO 1000:
        FIND FIRST ttWithIndex WHERE ttWithIndex.field1 = i NO-ERROR.
    END.
    iWithFind = ETIME.
    
    ETIME(TRUE).
    DO i = 1 TO 1000:
        FIND FIRST ttWithoutIndex WHERE ttWithoutIndex.field1 = i NO-ERROR.
    END.
    iWithoutFind = ETIME.
    
    MESSAGE 
        "With index took" iWithFind "ms to find and" iWithCreate "ms to create" SKIP 
        "Without index took" iWithoutFind "ms to find and" iWithoutCreate "ms to create"
        VIEW-AS ALERT-BOX.

[![enter image description here][1]][1]

Searching with index was roughly 70 times faster compared to no index! This is just one run of course so not a scientific proof but your index setup will make impact. 

  [1]: https://i.stack.imgur.com/G8ecw.png

## Inputting and outputting temp-tables
It's very simple to pass temp-tables in and out of programs, procedures and functions.

This can be handy if you want a procedure to process a bigger number of data than you can easily store in a string or similar. You can pass temp-tables as `INPUT`, `OUTPUT` and `INPUT-OUTPUT` data.

Inputting one temp-table and outputting another:

    DEFINE TEMP-TABLE ttRequest NO-UNDO
        FIELD fieldA AS CHARACTER
        FIELD fieldB AS CHARACTER.
    
    /* Define a temp-table with the same fields and indices */
    DEFINE TEMP-TABLE ttResponse NO-UNDO LIKE ttRequest.
    
    /* A procedure that simply swap the values of fieldA and fieldB */
    PROCEDURE swapFields:
        DEFINE INPUT  PARAMETER TABLE FOR ttRequest.
        DEFINE OUTPUT PARAMETER TABLE FOR ttResponse.
    
        FOR EACH ttRequest:
            CREATE ttResponse.
            ASSIGN 
                ttResponse.fieldA = ttRequest.fieldB
                ttResponse.fieldB = ttRequest.fieldA.
        END.
    END PROCEDURE.
    
    CREATE ttRequest.
    ASSIGN ttRequest.fieldA = "A"
           ttRequest.fieldB = "B".
    
    CREATE ttRequest.
    ASSIGN ttRequest.fieldA = "B"
           ttRequest.fieldB = "C".
    
    CREATE ttRequest.
    ASSIGN ttRequest.fieldA = "C"
           ttRequest.fieldB = "D".
    
    /* Call the procedure */
    RUN swapFields ( INPUT  TABLE ttRequest
                   , OUTPUT TABLE ttResponse).
    
    FOR EACH ttResponse:
        DISPLAY ttResponse.
    END.

Result:

    fieldA--------fieldB--------
    
    B             A
    C             B
    D             C

Input-outputting a temp-table:

    DEFINE TEMP-TABLE ttCalculate NO-UNDO
        FIELD num1     AS INTEGER
        FIELD num2     AS INTEGER
        FIELD response AS DECIMAL.
    
    PROCEDURE pythagoras:
        DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttCalculate.
    
        FOR EACH ttCalculate:
            ttCalculate.response = SQRT( EXP(num1, 2) + EXP(num2, 2)).
        END.
    
    END PROCEDURE.
    
    CREATE ttCalculate.
    ASSIGN ttCalculate.num1 = 3
           ttCalculate.num2 = 4.
    
    CREATE ttCalculate.
    ASSIGN ttCalculate.num1 = 6
           ttCalculate.num2 = 8.
    
    CREATE ttCalculate.
    ASSIGN ttCalculate.num1 = 12
           ttCalculate.num2 = 16.
    
    /* Call the procedure */
    RUN pythagoras ( INPUT-OUTPUT  TABLE ttCalculate ).
    
    FOR EACH ttCalculate:
        DISPLAY ttCalculate.
    END.

Result:

    ----------num1-- ----------num2-- -------response-
    
             3                4             5.00
             6                8            10.00
            12               16            20.00

**Passing to functions**

    DEFINE TEMP-TABLE ttNumbers NO-UNDO
        FIELD num1     AS INTEGER
        FIELD num2     AS INTEGER
        INDEX index1 num1 num2.
    
    DEFINE VARIABLE iNum AS INTEGER     NO-UNDO.
    
    /* Forward declare the function */
    FUNCTION hasAPair RETURNS LOGICAL (INPUT TABLE ttNumbers) FORWARD.
    
    DO iNum = 1 TO 100:
        CREATE ttNumbers.
        ASSIGN ttNumbers.num1 = RANDOM(1,100)
               ttNumbers.num2 = RANDOM(1,100).
    END.
    
    MESSAGE hasAPair(INPUT TABLE ttNumbers) VIEW-AS ALERT-BOX.
    
    
    /* Function to check if two records has the same value in num1 and num2 */
    FUNCTION hasAPair RETURNS LOGICAL (INPUT TABLE ttNumbers):
    
        FIND FIRST ttNumbers WHERE ttNumbers.num1 = ttNumbers.num2 NO-ERROR.
        IF AVAILABLE ttNumbers THEN
            RETURN TRUE.
        ELSE 
            RETURN FALSE.
    
    END FUNCTION.
    
**Passing to program files**

You pass temp-tables to and from other .p-programs the same way you pass them to other procedures. The only difference is that both the calling and the called program must have the same temp-table declaration. One easy way is to store the temp-table program in a third file - an include that's used in both programs.


Include file containing temp-table definition:
    /* ttFile.i */
    DEFINE TEMP-TABLE ttFile NO-UNDO
        FIELD fName        AS CHARACTER FORMAT "x(20)"
        FIELD isADirectory AS LOGICAL.

Program checking all files in a temp-table. Are they directories?

    /* checkFiles.p */
    {ttFile.i}

    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFile.
    
    FOR EACH ttFile:
        FILE-INFO:FILE-NAME = ttFile.fName.
    
        IF FILE-INFO:FILE-TYPE BEGINS "D" THEN
            ttFile.isADirectory = TRUE.
    END.


Main program:

    {ttFile.i}
    
    CREATE ttFile.
    ASSIGN ttFile.fname = "c:\temp\".
    
    CREATE ttFile.
    ASSIGN ttFile.fname = "c:\Windows\".
    
    CREATE ttFile.
    ASSIGN ttFile.fname = "c:\Windoose\".
    
    RUN checkFiles.p(INPUT-OUTPUT TABLE ttFile).
    
    FOR EACH ttFile:
        DISPLAY ttFile.
    END.

Result:

    fName----------------- isADirector
    
    c:\temp\               yes
    c:\Windows\            yes
    c:\Windoose\           no



