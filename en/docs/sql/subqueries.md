---
title: "Subqueries"
slug: "subqueries"
draft: false
images: []
weight: 9807
type: docs
toc: true
---

Subqueries can appear in different clauses of an outer query, or in the set operation.

They must be enclosed in parentheses `()`.
If the result of the subquery is compared to something else, the number of columns must match.
Table aliases are required for subqueries in the FROM clause to name the temporary table.

## Subquery in FROM clause


## Subquery in WHERE clause


## Subquery in SELECT clause


## Correlated Subqueries
Correlated (also known as Synchronized or Coordinated) Subqueries are nested queries that make references to the current row of their outer query:

    SELECT EmployeeId
        FROM Employee AS eOuter
        WHERE Salary > (
           SELECT AVG(Salary)
           FROM Employee eInner
           WHERE eInner.DepartmentId = eOuter.DepartmentId
        )

Subquery `SELECT AVG(Salary) ...` is *correlated* because it refers to `Employee` row `eOuter` from its outer query.

## Subqueries in FROM clause 
You can use subqueries to define a temporary table  and use it in the FROM clause of an "outer" query.
 

    SELECT * FROM (SELECT city, temp_hi - temp_lo AS temp_var FROM weather) AS w
    WHERE temp_var > 20;

The above finds cities from the [weather table][1] whose daily temperature variation is greater than 20. The result is:

|       city       | temp_var | 
|----------------- | ---------| 
| ST LOUIS         |       21 | 
| LOS ANGELES      |       31 | 
| LOS ANGELES      |       23 | 
| LOS ANGELES      |       31 | 
| LOS ANGELES      |       27 | 
| LOS ANGELES      |       28 | 
| LOS ANGELES      |       28 | 
| LOS ANGELES      |       32 |


.


  [1]: https://www.wikiod.com/sql/example-databases-and-tables "weather table"

## Subqueries in WHERE clause

The following example finds cities (from the [cities example][1]) whose population is below the average temperature (obtained via a sub-qquery):  

    SELECT name, pop2000 FROM cities 
    WHERE pop2000 < (SELECT avg(pop2000)  FROM cities);

Here: the subquery (SELECT avg(pop2000)  FROM cities) is used to specify conditions in the WHERE clause. The result is:

|       name       | pop2000 |
|----------------- | --------|
| San Francisco    |  776733 |
| ST LOUIS         |  348189 |
| Kansas City      |  146866 |

  [1]: https://www.wikiod.com/sql/example-databases-and-tables

## Filter query results using query on different table


## Subqueries in SELECT clause
Subqueries can also be used in the `SELECT` part of the outer query. The following query 
shows all [weather table][1] columns with the corresponding states from the [cities table][2].


    SELECT w.*,  (SELECT c.state FROM cities AS c WHERE c.name = w.city ) AS state 
    FROM weather AS w;


  [1]: https://www.wikiod.com/sql/example-databases-and-tables
  [2]: https://www.wikiod.com/sql/example-databases-and-tables

