---
title: "FIND statement"
slug: "find-statement"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The `FIND` statement is used to retrieve a single record from a single table. It has some limitations compared to `FOR` or `QUERY`, but it's a simple and handy statement for fast access to records.

## FIND basic examples
A simple sports2000 example:

    FIND FIRST Customer NO-LOCK WHERE CustNum = 1 NO-ERROR.
    IF AVAILABLE Customer THEN DO:
        DISPLAY Customer.NAME.
    END.
    ELSE DO:
        MESSAGE "No record available".
    END.

   
> FIRST - find  the first record that matches the query
> 
> NO-LOCK - don't lock the record - meaning we will only read and not change the record.
>
> WHERE - this is the query
>
> NO-ERROR - don't fail if there isn't any record available.
> 
> (IF) AVAILABLE Customer - check if we found a record or not

    findLoop:
    REPEAT :
        FIND NEXT Customer NO-LOCK WHERE NAME BEGINS "N" NO-ERROR.
    
        IF AVAILABLE customer THEN DO:
            DISPLAY Customer.NAME.
        END.
        ELSE DO:
            LEAVE findLoop.
        END.
    END.





## Availability and scope
The latest find is always the one the availability check will work against - a unsuccesful find will make `AVAILABLE` return false:

    DEFINE TEMP-TABLE tt NO-UNDO
        FIELD nr AS INTEGER.
    
    CREATE tt.
    tt.nr = 1.
    
    CREATE tt.
    tt.nr = 2.
    
    CREATE tt.
    tt.nr = 3.
    
    DISPLAY AVAILABL tt. // yes (tt with nr = 3 is still available)
    
    FIND FIRST tt WHERE tt.nr = 1 NO-ERROR.
    DISPLAY AVAILABLE tt. //yes
    
    FIND FIRST tt WHERE tt.nr = 2 NO-ERROR.
    DISPLAY AVAILABLE tt. //yes
    
    FIND FIRST tt WHERE tt.nr = 3 NO-ERROR.
    DISPLAY AVAILABLE tt. //yes
    
    FIND FIRST tt WHERE tt.nr = 4 NO-ERROR.
    DISPLAY AVAILABLE tt. //no

A record found in "main" will be available in any procedures.


    DEFINE TEMP-TABLE tt NO-UNDO
        FIELD nr AS INTEGER.
    
    PROCEDURE av:
        DISPLAY AVAILABLE tt.
    
        IF AVAILABLE tt THEN DO:
            DISPLAY tt.nr.
        END.
    END PROCEDURE.
    
    CREATE tt.
    tt.nr = 1.
            
    RUN av. // yes. tt.nr = 1
    
    CREATE tt.
    tt.nr = 2.
    
    RUN av. // yes. tt.nr = 2
    
    FIND FIRST tt WHERE tt.nr = 4 NO-ERROR.
    
    RUN av. // no (and no tt.nr displayed)

Also, a record found in a procedure will still be available after that procedure has exited.

    DEFINE TEMP-TABLE tt NO-UNDO
        FIELD nr AS INTEGER.
    
    PROCEDURE av:
        FIND FIRST tt WHERE tt.nr = 1.
    END PROCEDURE.
    
    CREATE tt.
    tt.nr = 1.
    
    CREATE tt.
    tt.nr = 2.
    
    DISPLAY AVAILABLE tt WITH FRAME x1. // yes.
    
    IF AVAILABLE tt THEN DO:
        DISPLAY tt.nr WITH FRAME x1. //tt.nr = 2
    END.
    
    PAUSE.
    
    RUN av. 
    
    DISPLAY AVAILABLE tt WITH FRAME x2. // yes.
    
    IF AVAILABLE tt THEN DO:
        DISPLAY tt.nr WITH FRAME x2. //tt.nr = 1
    END.

## FIND and locking
Whenever you `FIND` a record you can aquire a lock of it. 

NO-LOCK: Used for read only operations. If you do a `FIND <record> NO-LOCK` you cannot in any way modify the record.

    FIND FIRST Customer NO-LOCK NO-ERROR.

EXCLUSIVE-LOCK: Used for updates and deletes. If you do this you will "own" the record and nobody else can modify it or delete it until you're done. They can read it (with no-lock) as long as you haven't deleted it.

    FIND FIRST Customer EXCLUSIVE-LOCK NO-ERROR.

SHARE-LOCK: Avoid at all cost. This will cause mad headache.

    FIND FIRST Customer EXCLUSIVE-LOCK NO-ERROR. //Do this instead.

**UPGRADING your lock from NO-LOCK to EXCLUSIVE-LOCK**

You can easily move from a `NO-LOCK` to an `EXCLUSIVE-LOCK` if the need to modify a record has arisen:

    FIND FIRST Customer NO-LOCK NO-ERROR. 
    // Some code goes here
    // Now we shall modify
    FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.

You can go from EXCLUSIVE-LOCK to NO-LOCK as well. 

**LOCKED RECORDS**

Whenever other users might aquire a lock of a record you better take this possibility into account. Collisions will occur!

It's better to handle this programmatically using the `NO-WAIT` statement. This tells the AVM to just pass the FIND if the record is locked by somebody else and let you handle this problem.


    FIND FIRST Customer EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    
    /* Check for availability */
    IF AVAILABLE Customer THEN DO:
    
        /* Check that no lock (from somebody else) is present */
        IF NOT LOCKED Customer THEN DO:
            /* Do your stuff here */
        END.
        ELSE DO:
            MESSAGE "I'm afraid somebody else has locked this record!" VIEW-AS ALERT-BOX ERROR.
        END.
    END.




