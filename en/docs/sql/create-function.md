---
title: "CREATE FUNCTION"
slug: "create-function"
draft: false
images: []
weight: 9959
type: docs
toc: true
---

## Syntax
 - CREATE FUNCTION function_name ( [list_of_paramenters] ) RETURNS return_data_type 
AS
BEGIN 
    function_body 
    RETURN scalar_expression
END





## Parameters
| Argument | Description |
| ------ | ------ |
| function_name   | the name of function   |
| list_of_paramenters   | parameters that function accepts    |
| return_data_type   | type that function returs. Some SQL [data type][1]   |
| function_body   | the code of function  |
| scalar_expression   | scalar value returned by function   |


  [1]: http://www.w3schools.com/sql/sql_datatypes_general.asp

CREATE FUNCTION creates a user-defined function that can be used when doing a SELECT, INSERT, UPDATE, or DELETE query. The functions can be created to return a single variable or a single table.

## Create a new Function
    CREATE FUNCTION FirstWord (@input varchar(1000))
    RETURNS varchar(1000)
    AS
    BEGIN
        DECLARE @output varchar(1000)
        SET @output = SUBSTRING(@input, 0, CASE CHARINDEX(' ', @input)
            WHEN 0 THEN LEN(@input) + 1
            ELSE CHARINDEX(' ', @input)
        END)
    
        RETURN @output
    END

This example creates a function named **FirstWord**, that accepts a varchar parameter and returns another varchar value.

