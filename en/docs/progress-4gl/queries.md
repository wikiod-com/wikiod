---
title: "Queries"
slug: "queries"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

The examples will be based on a copy of the demo database `Sports 2000` provided with the setup of Progress.

When working with queries in Progress you need to:

`DEFINE` the query and set what buffers (tables) and fields it works against.

`OPEN` the query with a specific `WHERE`-clause that defines how to retrieve the records. Possibly also sorting (`BY`/`BREAK BY`)

`GET` the actual data - that can be the `FIRST`, `NEXT`, `PREV` (for previous) or `LAST` matching record. 

## Syntax
- DEFINE QUERY query-name FOR buffer-name. //General query definition for one buffer
- DEFINE QUERY query-name FOR buffer-name1, buffer-name2. //Joining two buffers
- DEFINE QUERY query-name FOR buffer-name FIELDS (field1 field2). //Only retreive field1 and field2
- DEFINE QUERY query-name FOR buffer-name EXCEPT (field3). //Retreive all fields except field3.
   

## Multi-Tables Query
This query will join three tables: Customer, Order and Orderline. 

The use of the `OF` statement as in `childtable OF parenttable` assumes that indexes are constructed in a specific way. That is the case in the sports2000-database.

    DEFINE QUERY q1 FOR Customer, Order, Orderline.
    
    OPEN QUERY q1 FOR EACH Customer WHERE Customer.state = 'TX'
        , EACH Order OF customer WHERE order.custnum < 1000
        , EACH orderline OF order.
    
    GET FIRST q1.
    DO WHILE NOT QUERY-OFF-END('q1'):
        DISPLAY Customer.Name Order.OrderNum OrderLine.LineNum 
            WITH FRAME frameA 20 DOWN.
        DOWN WITH FRAME frameA.
        GET NEXT q1.
    END.
    
    CLOSE QUERY q1.

Result: In Windows GUI:

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/QMvMG.png

## Basic Query
    

    /* Define a query named q1 for the Customer table */
    DEFINE QUERY q1 FOR Customer.
    /* Open the query for all Customer records where the state is "tx" */
    OPEN QUERY q1 FOR EACH Customer WHERE Customer.state ='TX'.                                                                                                                                                                               
    /* Get the first result of query q1 */
    GET FIRST q1.                                                                                                                                                                                                                   
    
    /* Repeat as long as query q1 has a result */
    DO WHILE NOT QUERY-OFF-END('q1'):          
        /* Display Customer.Name in a frame called frame1 with 10 rows */
        DISPLAY Customer.Name WITH FRAME frame1 10 DOWN.
        /* Move down the target line where to display the next record */
        DOWN WITH FRAME frame1.
        /* Get the next result of query q1 */
        GET NEXT q1.
    END.
    /* Display how many results query q1 had. */
    DISPLAY NUM-RESULTS('q1') LABEL "Number of records".
    
    /* Close the query */
    CLOSE QUERY q1.                                              

Output (third screen in Windows gui):

[![enter image description here][1]][1]


  [1]: https://i.stack.imgur.com/0loHs.png

## Moving poisition withing a query using next, first, prev and last


    DEFINE QUERY q1 FOR Customer.
    
    OPEN QUERY q1 FOR EACH Customer.
    
    GET FIRST q1.
    
    loop:
    REPEAT:
        IF AVAILABLE Customer THEN DO:
            DISPLAY Customer.NAME CustNum WITH FRAME frClient TITLE "Client data".
            
            DISPLAY
                "(P)revious" SKIP 
                "(N)ext" SKIP
                "(F)irst" SKIP
                "(L)ast" SKIP
                "(Q)uit" SKIP
                WITH FRAME frInstr
                    TITLE "Instructions".
        END.
        
        READKEY.
    
        IF LASTKEY = ASC("q") THEN LEAVE loop.
        ELSE IF LASTKEY = ASC("n") THEN
            GET NEXT q1.
        ELSE IF LASTKEY = ASC("p") THEN
            GET PREV q1.
        ELSE IF LASTKEY = ASC("l") THEN
            GET LAST q1.
        ELSE IF LASTKEY = ASC("f") THEN
            GET FIRST q1.
    
    END.
    
    MESSAGE "Bye" VIEW-AS ALERT-BOX.



