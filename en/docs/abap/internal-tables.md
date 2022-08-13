---
title: "Internal Tables"
slug: "internal-tables"
draft: false
images: []
weight: 9965
type: docs
toc: true
---

## Types of Internal tables
    DATA: <TABLE NAME> TYPE <SORTED|STANDARD|HASHED> TABLE OF <TYPE NAME> 
          WITH <UNIQUE|NON-UNIQUE> KEY <FIELDS FOR KEY>.
    
**Standard Table**

This table has all of the entries stored in a linear fashion and records are accessed in a linear way. For large table sizes, table access can be slow.

**Sorted Table**

Requires the addition `WITH UNIQUE`|`NON-UNIQUE KEY`. Searching is quick due to performing a binary search. Entries cannot be appended to this table as it might break the sort order, so they are always inserted using the `INSERT` keyword.

**Hashed Table**

Requires the addition `WITH UNIQUE`|`NON-UNIQUE KEY`. Uses a proprietary hashing algorithm to maintain key-value pairs. Theoretically searches can be as slow as `STANDARD` table but practically they are faster than a `SORTED` table taking a constant amount of time irrespective of the size of the table.



## Declaration of ABAP Internal Tables
# Internal Table Declaration Based on Local Type Definition

    " Declaration of type
    TYPES: BEGIN OF ty_flightb,
             id        TYPE fl_id,
             dat       TYPE fl_date,
             seatno    TYPE fl_seatno,
             firstname TYPE fl_fname, 
             lastname  TYPE fl_lname,
             fl_smoke  TYPE fl_smoker,
             classf    TYPE fl_class,
             classb    TYPE fl_class,
             classe    TYPE fl_class,
             meal      TYPE fl_meal,
             service   TYPE fl_service,
             discout   TYPE fl_discnt,
           END OF lty_flightb.

    " Declaration of internal table
    DATA t_flightb TYPE STANDARD TABLE OF ty_flightb.

# Declaration based on Database Table

    DATA t_flightb TYPE STANDARD TABLE OF flightb.

# Inline Internal Table Declaration
*Requires ABAP version > 7.4*

    TYPES t_itab TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA(t_inline) = VALUE t_itab( ( 1 ) ( 2 ) ( 3 ) ).

# Internal Table with Header Lines Declaration

In ABAP there are tables with header lines, and tables without header lines. Tables with header lines are an older concept and should not be used in new development. 

**Internal Table: Standard Table with / without header line**

This code declares the table `i_compc_all` with the existing structure of `compc_str`.

    DATA: i_compc_all TYPE STANDARD TABLE OF compc_str WITH HEADER LINE.
    DATA: i_compc_all TYPE STANDARD TABLE OF compc_str.

**Internal Table: Hashed Table with / without header line**

    DATA: i_map_rules_c TYPE HASHED TABLE OF /bic/ansdomm0100 WITH HEADER LINE
    DATA: i_map_rules_c TYPE HASHED TABLE OF /bic/ansdomm0100

**Declaration of a work area for tables without a header**

A work area (commonly abbreviated *wa*) has the exact same structure as the table, but can contain only one line (a WA is a structure of a table with only one dimension).

    DATA: i_compc_all_line LIKE LINE OF i_compc_all.

## Read, Write and Insert into Internal Tables
Read, write and insert into internal tables with a header line:

    " Read from table with header (using a loop):
    LOOP AT i_compc_all.              " Loop over table i_compc_all and assign header line
      CASE i_compc_all-ftype.         " Read cell ftype from header line from table i_compc_all 
        WHEN 'B'.                     " Bill-to customer number transformation
          i_compc_bil = i_compc_all.  " Assign header line of table i_compc_bil with content of header line i_compc_all
          APPEND i_compc_bil.         " Insert header line of table i_compc_bil into table i_compc_bil
        " ... more WHENs
      ENDCASE.
    ENDLOOP.

> Reminder: Internal tables with header lines are forbidden in object oriented contexts. Usage of internal tables *without* header lines is always recommended.

Read, write and insert into internal tables without a header line: 

    " Loop over table i_compc_all and assign current line to structure i_compc_all_line
    LOOP AT i_compc_all INTO i_compc_all_line.      
      CASE i_compc_all_line-ftype.                " Read column ftype from current line (which as assigned into i_compc_all_line)
        WHEN 'B'.                                 " Bill-to customer number transformation
          i_compc_bil_line = i_compc_all_line.    " Copy structure
          APPEND i_compc_bil_line TO i_compc_bil. " Append structure to table
        " more WHENs ...
      ENDCASE.
    ENDLOOP.


    " Insert into table with Header:
    INSERT TABLE i_sap_knb1.                      " insert into TABLE WITH HEADER: insert table header into it's content
    insert i_sap_knb1_line into table i_sap_knb1. " insert into HASHED TABLE: insert structure i_sap_knb1_line into hashed table i_sap_knb1
    APPEND p_t_errorlog_line to p_t_errorlog.     " insert into STANDARD TABLE: insert structure / wa p_t_errorlog_line into table p_t_errorlog_line

