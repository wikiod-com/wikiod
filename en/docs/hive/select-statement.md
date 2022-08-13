---
title: "SELECT Statement"
slug: "select-statement"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

## Syntax
 - SELECT [ALL | DISTINCT] select_expr, select_expr, select_expr, ….
 - FROM table_reference
 - [WHERE where_condition]
 - [GROUP BY col_list]
 - [HAVING having condition]
 - [ORDER BY col_list]
 - [LIMIT n]

## Select Specific Rows
This query will return all columns from the table `sales` where the values in the column `amount` is greater than 10 and the data in the `region` column in "US".

    SELECT * FROM sales WHERE amount > 10 AND region = "US"

You can use *regular expressions* to select the columns you want to obtain. The following statement will get the data from column `name` and all the columns starting with the prefix `address`. 

    SELECT name, address.* FROM Employees

You can also use the keyword `LIKE` (combined with the character '%') to match strings that begin with or end with a particular substring. The following query will return all the rows where the column `city` begins with "New"

    SELECT name, city FROM Employees WHERE city LIKE 'New%'

You can use the keyword `RLIKE` to use Java *[regular expressions][1]*. The following query will return rows which column `name` contains the words "smith" or "son".

    SELECT name, address FROM Employee WHERE name RLIKE '.*(smith|son).*'

You can apply functions to the returned data. The following sentence will return all name in upper case.

    SELECT upper(name) FROM Employees

You can use different [mathematical functions][2] , [collection functions][3], [type conversion functions][4], [date functions][5], [conditional functions][6] or [string functions][7].

In order to limit the number of rows given in result, you can use the `LIMIT` keyword. The following statement will return only ten rows.

    SELECT * FROM Employees LIMIT 10


  [1]: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
  [2]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-MathematicalFunctions
  [3]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-CollectionFunctions
  [4]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-TypeConversionFunctions
  [5]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-DateFunctions
  [6]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-ConditionalFunctions
  [7]: https://cwiki.apache.org/confluence/display/Hive/LanguageManual+UDF#LanguageManualUDF-StringFunctions

## Select All Rows
`SELECT` is used to retrieve rows of data from a table. You can specify which columns will be retrieved:

    SELECT Name, Position
    FROM Employees;

Or just use * to get all columns:

    SELECT *
    FROM Employees;

## Select: Project selected columns
**Sample table (say Employee) structure**

| Column Name | Datatype |
| ----------- | -------- |
| ID          | INT      |
| F_Name      | STRING   |
| L_Name      | STRING   |
| Phone       | STRING   |
| Address     | STRING   |

 **Project all the columns**

Use wild card `*` to project all the columns. e.g.

    Select * from Employee

**Project selected columns (say ID, Name)**

Use name of columns in the projection list. e.g.

    Select ID, Name from Employee

**Discard 1 column from Projection list**

Display all columns except 1 column. e.g.

    Select `(ID)?+.+` from Employee

**Discard columns matching pattern**

Reject all columns which matches the pattern. e.g. Reject all the columns ending with `NAME`

    Select `(.*NAME$)?+.+` from Employee

