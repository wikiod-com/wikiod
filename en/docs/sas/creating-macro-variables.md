---
title: "Creating Macro Variables"
slug: "creating-macro-variables"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

Using Macro Variables throughout your SAS programs is a basic functionality that every SAS programmer must be familiar with. Using Macro Variables can help you to keep your code simple and generic. Generic code is reusable code.

## Using %LET
I would describe %LET as being the most simple way to creating a Macro Variable in SAS.

    %LET variableName = variableValue;

Now, anywhere you use `&variableName`, it will resolve to `variableValue`. 
>NOTE:you may want to consider that `variableValue` all on its own might bring you syntax errors, depening on what the value is and how it's used. For example, if it is a date and you're using it in the WHERE of a PROC SQL statement, it will need to be written as `"&variableName"d` to work properly.

## Using PROC SQL
Using PROC SQL is a good way to get quick results from a table and throw them into variables. I usually find that when I want to get a count of records I just loaded to a table, I can get that count into a variable with a quick PROC SQL call.

    PROC SQL;
    SELECT
        COUNT(*) INTO:aVariable
    FROM
        MyTable
    
    ;QUIT;
In the example above, `aVariable` will represent how many records exist in `MyTable`.

You can also use PROC SQL for creating multiple Macro Variables.




    PROC SQL;
    SELECT
        a,
        b,
        c INTO:aVariable, :bVariable, :cVariable
    FROM
        MyTable

    
    ;QUIT;
In the example above, the variables created in the INTO statement will match up to the columns pulled in the order they are returned from the SELECT statement. However, only the first row of results will be used to fill those 3 variables.

If you want to store more than a single row, and you're on version 6.11 or beyond, use the following example:

    PROC SQL;
        SELECT DISTINCT
            a,
            b,
            c INTO :aVariable1 - :aVariable5, 
                   :bVariable1 - :bVariable5,
                   :cVariable1 - :cVariable5
        FROM
            MyTable
    ;QUIT;
>The keywords `THROUGH` and `THRU` can he used en lieu of the dash `-`

## Using Call Symput() in a DATA step
    DATA _null_;
                CALL SYMPUT('testVariable','testValueText');
    ;RUN;
In the example above, `%PUT &testVariable;` will resolve to `testvalueText`.


You may find the need to format your variable within the SYMPUT() call.

    DATA _null_;
                CALL SYMPUT('testDate',COMPRESS(PUT(today(),date9.)));
    ;RUN;
    
In the example above,`%PUT &testDate;` will resolve to `10MAR2017`

