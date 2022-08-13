---
title: "Regular Expressions"
slug: "regular-expressions"
draft: false
images: []
weight: 9982
type: docs
toc: true
---

## Evaluating Regular Expressions with a Predicate Function
The predicate function `matches` can be used to evaluate strings on the fly without use of any object declarations.
    
    IF matches( val = 'Not a hex string'
                regex = '[0-9a-f]*' ).
      cl_demo_output=>display( 'This will not display' ).
    ELSEIF matches( val = '6c6f7665'
                    regex = '[0-9a-f]*' ).
      cl_demo_output=>display( 'This will display' ).
    ENDIF.

## Replacing
The `REPLACE` statement can work with regular expressions directly:    

    DATA(lv_test) = 'The quick brown fox'.
    REPLACE ALL OCCURRENCES OF REGEX '\wo' IN lv_test WITH 'XX'.
    
The variable `lv_test` will evaluate to `The quick bXXwn XXx`.
    

## Object-Oriented Regular Expressions
For more advanced regex operations it's best to use `CL_ABAP_REGEX` and its related classes.

    DATA: lv_test  TYPE string,
          lo_regex TYPE REF TO cl_abap_regex.
    
    lv_test = 'The quick brown fox'.
    CREATE OBJECT lo_regex
      EXPORTING
        pattern = 'q(...)\w'.
    
    DATA(lo_matcher) = lo_regex->create_matcher( text = lv_test ).
    WRITE: / lo_matcher->find_next( ).      " X
    WRITE: / lo_matcher->get_submatch( 1 ). " uic
    WRITE: / lo_matcher->get_offset( ).     " 4

## Getting SubMatches Using OO-Regular Expressions
By using the method `GET_SUBMATCH` of class `CL_ABAP_MATCHER`, we can get the data in the groups/subgroups.

Goal: get the token to the right of the keyword 'Type'.

    DATA: lv_pattern TYPE string VALUE 'type\s+(\w+)',
          lv_test TYPE string VALUE 'data lwa type mara'.

    CREATE OBJECT ref_regex
      EXPORTING
            pattern     = lv_pattern
            ignore_case = c_true.

    ref_regex->create_matcher(
        EXPORTING
            text    = lv_test
        RECEIVING
            matcher = ref_matcher
           ).

    ref_matcher->get_submatch(
                EXPORTING
                     index = 0
                RECEIVING
                     submatch = lv_smatch.

The resulting variable `lv_smatch` contains the value `MARA`.


## Searching
The `FIND` statement can work with regular expressions directly:

    DATA(lv_test) = 'The quick brown fox'.

    FIND REGEX '..ck' IN lv_test.
    " sy-subrc == 0
    
    FIND REGEX 'a[sdf]g' IN lv_test.
    " sy-subrc == 4

