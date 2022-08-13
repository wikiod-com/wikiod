---
title: "Data Declaration"
slug: "data-declaration"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Inline Data Declaration
In certain situations, data declarations can be performed inline.

    LOOP AT lt_sflight INTO DATA(ls_sflight).
        WRITE ls_sflight-carrid.
    ENDLOOP.

## Single Variable Declaration
    DATA begda TYPE sy-datum.

## Multiple Variable Declaration
    DATA: begda TYPE sy-datum,
          endda TYPE sy-datum.

## Inline Data Declaration in SELECT Statement
When using an inline data declaration inside of a `SELECT...ENDSELECT` block or `SELECT SINGLE` statement, the `@` character must be used as an escape character for the `DATA(lv_cityto)` expression. Once the escape character has been used, all further host variables must also be escaped (as is the case with `lv_carrid` below).

    DATA lv_carrid TYPE s_carr_id VALUE 'LH'.
    SELECT SINGLE cityto FROM spfli
           INTO @DATA(lv_cityto)
           WHERE carrid = @lv_carrid
           AND   connid = 2402.
    WRITE: / lv_cityto.
    
Outputs `BERLIN`.

## Variable Declaration Options
Different types of variables may be declared with special options.

    DATA: lv_string   TYPE string, " standard declaration
          lv_char     TYPE c,      " declares a character variable of length 1
          lv_char5(5) TYPE c,      " declares a character variable of length 5
          l_packed TYPE p LENGTH 10 DECIMALS 5 VALUE '1234567890.123456789'. " evaluates to 1,234,567,890.12346 
          

