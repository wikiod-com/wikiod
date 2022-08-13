---
title: "IF-THEN-ELSE Statement"
slug: "if-then-else-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
- IF [condition 1] THEN
-    [statements to execute when condition 1 is TRUE];

- ELSIF [condition 2] THEN
-   [statements to execute when condition 2 is TRUE];

- ELSE
-    [statements to execute when both condition 1 & condition 2 are FALSE];

- END IF;

## IF-THEN
    DECLARE
    v_num1 NUMBER(10);
    v_num2 NUMBER(10);
    
    BEGIN
      v_num1 := 2;
      v_num2 := 1;
      
      IF v_num1 > v_num2 THEN
         dbms_output.put_line('v_num1 is bigger than v_num2');
      END IF;
    END;

## IF-THEN-ELSE
    DECLARE
    v_num1 NUMBER(10);
    v_num2 NUMBER(10);
    
    BEGIN
      v_num1 := 2;
      v_num2 := 10;
      
      IF v_num1 > v_num2 THEN
         dbms_output.put_line('v_num1 is bigger than v_num2');
      ELSE
        dbms_output.put_line('v_num1 is NOT bigger than v_num2');
      END IF;
    END;

## IF-THEN-ELSIF-ELSE
    DECLARE
    v_num1 NUMBER(10);
    v_num2 NUMBER(10);
    
    BEGIN
      v_num1 := 2;
      v_num2 := 2;
      
      IF v_num1 > v_num2 THEN
         dbms_output.put_line('v_num1 is bigger than v_num2');
      ELSIF v_num1 < v_num2 THEN
        dbms_output.put_line('v_num1 is NOT bigger than v_num2');
      ELSE
        dbms_output.put_line('v_num1 is EQUAL to v_num2');
      END IF;
    END;

