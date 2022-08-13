---
title: "Resolving Macro Variables in quotes within PROC SQL Pass-throughs"
slug: "resolving-macro-variables-in-quotes-within-proc-sql-pass-throughs"
draft: false
images: []
weight: 9988
type: docs
toc: true
---

One of the challenges I faced when I first started using SAS was not only passing Macro Variable data into a PROC SQL pass-through, but having it resolve properly if it needed quotes around it. When passing a string like value or date/datetime into a PROC SQL pass-through, it most likely needs to have single quotes around it when it resolves.

I have found the best results when using the %BQUOTE function to accomplish this.

More information on the %BQUOTE function can be found here:
https://v8doc.sas.com/sashtml/macro/z4bquote.htm

## Pass-through with Macro Variable that is a Date
First, I will place my date into a Macro Variable. 

> NOTE: I find that
> date9. works great with IBM® Netezza® SQL and Transact-SQL. Use whichever format that works for the type of SQL you're executing.

    data _null_;
                call symput('testDate',COMPRESS(put(today(),date9.)));
    ;RUN;
    %PUT &testDate;
My %PUT statement resolves to: 10MAR2017

Next, I want to run a PROC SQL Pass-through and resolve that Macro Variable inside to specify a date.


    PROC SQL;
    CONNECT TO odbc AS alias (dsn=myServer user=userName password= pass);
    CREATE TABLE TableName AS 
    SELECT * 
    FROM connection to alias
        (
            SELECT *
            FROM
                Database.schema.MyTable
            WHERE
                DateColumn = %bquote('&testDate')
        );
    QUIT;

  %bquote('&testDate') will resolve to '10MAR2017' when the code executes.

