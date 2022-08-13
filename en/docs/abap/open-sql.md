---
title: "Open SQL"
slug: "open-sql"
draft: false
images: []
weight: 9989
type: docs
toc: true
---

## SELECT statement
SELECT is an Open-SQL-statement for reading data from one or several database tables into [data objects][1].

 1. Selecting All Records

        * This returns all records into internal table lt_mara.
        SELECT * FROM mara
                 INTO lt_mara.

 2. Selecting Single Record

        * This returns single record if table consists multiple records with same key.
        SELECT SINGLE * INTO TABLE lt_mara
                        FROM mara
                        WHERE matnr EQ '400-500'.

 3. Selecting Distinct Records

        * This returns records with distinct values.
        SELECT DISTINCT * FROM mara
                          INTO TABLE lt_mara
                          ORDER BY matnr.

 4. Aggregate Functions

        * This puts the number of records present in table MARA into the variable lv_var 
        SELECT COUNT( * ) FROM mara
                          INTO lv_var.

  [1]: https://help.sap.com/abapdocu_731/en/abenbuilt_in.htm

