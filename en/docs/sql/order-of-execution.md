---
title: "Order of Execution"
slug: "order-of-execution"
draft: false
images: []
weight: 9860
type: docs
toc: true
---

## Logical Order of  Query Processing in SQL
<pre><code>/*(8)*/  SELECT /*9*/ DISTINCT /*11*/ TOP <top_specification> <select_list>
/*(1)*/  FROM <left_table>
/*(3)*/       <join_type> JOIN <right_table>
/*(2)*/       ON <join_condition>
/*(4)*/  WHERE <where_condition>
/*(5)*/  GROUP BY <group_by_list>
/*(6)*/  WITH {CUBE | ROLLUP}
/*(7)*/  HAVING <having_condition>
/*(10)*/ ORDER BY <order_by_list>
/*(11)*/ LIMIT </code></pre>

The order in which a query is processed and description of each section.

VT stands for 'Virtual Table' and shows how various data is produced as the query is processed

1.    FROM: A Cartesian product (cross join) is performed between the first two tables in the FROM clause, and as a result, virtual table VT1 
    is generated.

2.    ON: The ON filter is applied to VT1. Only rows for which the is TRUE are inserted to VT2.

3.    OUTER (join): If an OUTER JOIN is specified (as opposed to a CROSS JOIN or an INNER JOIN), rows from the preserved table or tables for 
    which a match was not found are added to the rows from VT2 as outer rows, generating VT3. If more than two tables appear in the FROM clause,
    steps 1 through 3 are applied repeatedly between the result of the last join and the next table in the FROM clause until all tables are processed.

4.    WHERE: The WHERE filter is applied to VT3. Only rows for which the is TRUE are inserted to VT4.

5.    GROUP BY: The rows from VT4 are arranged in groups based on the column list specified in the GROUP BY clause. VT5 is generated.

6.    CUBE | ROLLUP: Supergroups (groups of groups) are added to the rows from VT5, generating VT6.

7.    HAVING: The HAVING filter is applied to VT6. Only groups for which the is TRUE are inserted to VT7.

8.    SELECT: The SELECT list is processed, generating VT8.

9.    DISTINCT: Duplicate rows are removed from VT8. VT9 is generated.

10.    ORDER BY: The rows from VT9 are sorted according to the column list specified in the ORDER BY clause. A cursor is generated (VC10).

11.    TOP: The specified number or percentage of rows is selected from the beginning of VC10. Table VT11 is generated and returned to the caller. LIMIT has the same functionality as TOP in some SQL dialects such as Postgres and Netezza.

