---
title: "Functions"
slug: "functions"
draft: false
images: []
weight: 9983
type: docs
toc: true
---

## Syntax
 - CREATE [OR REPLACE] FUNCTION function_name    [ (parameter
   [,parameter]) ]
   
      RETURN return_datatype
   
   IS | AS
   
      [declaration_section]
   
   BEGIN    executable_section
   
   [EXCEPTION    exception_section]
   
   END [function_name];

## Calling Functions
There are a few ways to use functions.

Calling a function with an assignment statement
   
    DECLARE
        x NUMBER := functionName(); --functions can be called in declaration section
    BEGIN
        x := functionName();
    END;

Calling a function in IF statement
    
    IF functionName() = 100 THEN
        Null;
    END IF;

Calling a function in a SELECT statement
    
    SELECT functionName() FROM DUAL;


## Generate GUID
    Create Or Replace Function Generateguid
    Return Char Is
        V_Guid Char(40);
    Begin
        Select Substr(Sys_Guid(),1,8)||'-'||Substr(Sys_Guid(),9,4)||'-'
                            ||Substr(Sys_Guid(),13,4)||'-'||Substr(Sys_Guid(),17,4)||'-'
                            ||Substr(Sys_Guid(),21) Into V_Guid 
                            From Dual;
        Return V_Guid;
    Exception
        When Others Then
        dbms_output.put_line('Error '|| SQLERRM);
    End Generateguid;

