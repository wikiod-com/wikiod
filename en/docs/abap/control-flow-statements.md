---
title: "Control Flow Statements"
slug: "control-flow-statements"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

## IF/ELSEIF/ELSE
    IF lv_foo = 3.
      WRITE: / 'lv_foo is 3'.
    ELSEIF lv_foo = 5.
      WRITE: / 'lv_foo is 5'.
    ELSE.
      WRITE: / 'lv_foo is neither 3 nor 5'.
    ENDIF.

## CASE
    CASE lv_foo.
      WHEN 1.
        WRITE: / 'lv_foo is 1'.
      WHEN 2.
        WRITE: / 'lv_foo is 2'.
      WHEN 3.
        WRITE: / 'lv_foo is 3'.
      WHEN OTHERS.
        WRITE: / 'lv_foo is something else'.
    ENDCASE

## CHECK
`CHECK` is a simple statement that evaluates a logical expression and exits the current processing block if it is false.

    METHOD do_something.
      CHECK iv_input IS NOT INITIAL. "Exits method immediately if iv_input is initial

      "The rest of the method is only executed if iv_input is not initial
    ENDMETHOD.  

## ASSERT
`ASSERT` is used in sensitive areas where you want to be absolutely sure, that a variable has a specific value. If the logical condition after `ASSERT` turns out to be false, an unhandleable exception (`ASSERTION_FAILED`) is thrown.

    ASSERT 1 = 1. "No Problem - Program continues

    ASSERT 1 = 2. "ERROR

## COND/SWITCH
`SWITCH` and `COND` offer a special form of conditional program flow. Unlike `IF` and `CASE`, they respresent different values based on an expression rather than executing statements. That's why they count as functional.

# COND

Whenever multiple conditions have to be considered, `COND` can do the job. The syntax is fairly simple: 
``` abap
COND <type>( 
    WHEN <condition> THEN <value> 
    ... 
    [ ELSE <default> | throw <exception> ]
).
```

## Examples

``` abap
" Set screen element active depending on radio button
screen-active = COND i(
    WHEN p_radio = abap_true THEN 1
    ELSE 0 " optional, because type 'i' defaults to zero
).

" Check how two operands are related to each other
" COND determines its type from rw_compare
rw_compare = COND #( 
    WHEN op1 < op2 THEN 'LT'
    WHEN op1 = op2 THEN 'EQ'
    WHEN op1 > op2 THEN 'GT' 
).
```

# SWITCH

`SWITCH` is a neat tool for mapping values, as it checks for equality only, thus being shorter than `COND` in some cases. If an unexpected input was given, it is also possible to throw an exception. The syntax is a little bit different:
``` abap
SWITCH <type>( 
    <variable>
    WHEN <value> THEN <new_value> 
    ... 
    [ ELSE <default> | throw <exception> ]
).
```

## Examples

``` abap
DATA(lw_language) = SWITCH string(
    sy-langu
    WHEN 'E' THEN 'English'
    WHEN 'D' THEN 'German'
    " ...
    ELSE THROW cx_sy_conversion_unknown_langu( )
).
```

