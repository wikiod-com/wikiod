---
title: "Handling NULL values"
slug: "handling-null-values"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

A column is NULL when it has no value, regardless of the data type of that column.

A column should never be compared to NULL using this syntax `a = NULL` as the result would be UNKNOWN. Instead use `a IS NULL` or `a IS NOT NULL` conditions. NULL is not equal to NULL. To compare two expressions where null can happen, use one of the functions described below.

All operators except concatenation return NULL if one of their operand is NULL. For instance the result of `3 * NULL + 5` is null.

NULL can't appear in columns restricted by a PRIMARY KEY or a NOT NULL constraint.
(Exception is a new constraint with NOVALIDATE clause) 

## Operations containing NULL are NULL, except concatenation
    SELECT 3 * NULL + 5, 'Hello ' || NULL || 'world'   from DUAL;
    

| 3*NULL+5 | 'HELLO'\|\|NULL\|\|'WORLD' |
| ------ | ------ |
| (null)   | Hello world   |

## NVL2 to get a different result if a value is null or not
If the first parameter is NOT NULL, NVL2 will return the second parameter. Otherwise it will return the third one. 

    SELECT NVL2(null, 'Foo', 'Bar'), NVL2(5, 'Foo', 'Bar') FROM DUAL;
| NVL2(NULL,'FOO','BAR')| NVL2(5,'FOO','BAR')|
| ------ | ------ |
| Bar    | Foo    |

## COALESCE to return the first non-NULL value
    SELECT COALESCE(a, b, c, d, 5) FROM 
        (SELECT NULL A, NULL b, NULL c, 4 d FROM DUAL);
| COALESCE(A,B,C,D,5) |
| ------ |
| 4   |

In some case, using COALESCE with two parameters can be faster than using NVL when the second parameter is not a constant. NVL will always evaluate both parameters. COALESCE will stop at the first non-NULL value it encounters. It means that if the first value is non-NULL, COALESCE will be faster.

## Columns of any data type can contain NULLs
    SELECT 1 NUM_COLUMN, 'foo' VARCHAR2_COLUMN from DUAL
    UNION ALL
    SELECT NULL, NULL from DUAL;

| NUM_COLUMN | VARCHAR2_COLUMN |
| ------ | ------ |
| 1   | foo   |
| (null)   | (null)  |


## Empty strings are NULL
    SELECT 1 a, '' b from DUAL;
| A | B |
| ------ | ------ |
| 1   | (null)   |

## NVL to replace null value
    SELECT a column_with_null, NVL(a, 'N/A') column_without_null FROM
      (SELECT NULL a FROM DUAL);

| COLUMN_WITH_NULL | COLUMN_WITHOUT_NULL |
| ------ | ------ |
| (null) | N/A |



NVL is useful to compare two values which can contain NULLs :

    SELECT
        CASE WHEN a = b THEN 1 WHEN a <> b THEN 0 else -1 END comparison_without_nvl,
        CASE WHEN NVL(a, -1) = NVL(b, -1) THEN 1 WHEN NVL(a, -1) <> NVL(b, -1) THEN 0 else -1 END comparison_with_nvl
      FROM
        (select null a, 3 b FROM DUAL
         UNION ALL
         SELECT NULL, NULL FROM DUAL);

| COMPARISON_WITHOUT_NVL| COMPARISON_WITH_NVL|
| ------ | ------ |
| -1   | 0 |
| -1   | 1 |

