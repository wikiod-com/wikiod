---
title: "With Ties Option "
slug: "with-ties-option-"
draft: false
images: []
weight: 9966
type: docs
toc: true
---

## Test Data
    CREATE TABLE #TEST
    (
    Id INT,
    Name VARCHAR(10)
    )
    
    Insert Into #Test
    select 1,'A'
    Union All
    Select 1,'B'
    union all
    Select 1,'C'
    union all
    Select 2,'D'

  Below is the output of above table,As you can see Id Column is repeated three times..

    Id   Name
    1    A
    1    B
    1    C
    2    D

Now Lets check the output using simple order by..

    Select Top (1)  Id,Name From 
    #test
    Order By Id ;

***Output :(Output of above query is not guaranteed to be same every time )***

    Id   Name
    1    B

Lets run the Same query With Ties Option..


    Select Top (1) With Ties Id,Name
     From 
    #test
    Order By Id 

***Output :***

    Id   Name
    1    A
    1    B
    1    C


As you can see SQL Server outputs all the Rows **which are tied with**  Order by Column.
Lets see one more Example to understand this better..

    Select Top (1) With Ties Id,Name
     From 
    #test
    Order By Id ,Name

**Output:**  

    Id   Name
    1    A



In Summary ,when we use with Ties Option,SQL Server Outputs all the Tied rows irrespective of limit we impose






