---
title: "Proc SQL"
slug: "proc-sql"
draft: false
images: []
weight: 9990
type: docs
toc: true
---

## Create an empty dataset based on an existing dataset
Method 1:   

     proc sql;
        create table foo like sashelp.class;
        quit;

Method 2:

    proc sql;
    create table bar as 
        select * from sashelp.class (obs=0);
    quit;

Method 1 should be the preferred option

## SELECT Syntax
    PROC SQL options;
      SELECT column(s)
    FROM table-name | view-name
       WHERE expression
       GROUP BY column(s)
       HAVING expression
    ORDER BYcolumn(s);
    QUIT;

Example 1:

    proc sql;
    select name
           ,sex
        from sashelp.class ;
    quit;


The SELECT statement is specified in this order :

    1.select;
    2.from;
    3.where;
    4.group by;
    5.having;
    6.order by.

"select" and "from" are required. The other clauses are optional.




