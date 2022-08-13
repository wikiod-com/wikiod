---
title: "Filter results using WHERE and HAVING"
slug: "filter-results-using-where-and-having"
draft: false
images: []
weight: 9723
type: docs
toc: true
---

## Syntax
 - SELECT column_name  
FROM table_name  
WHERE column_name operator value
 - SELECT column_name, aggregate_function(column_name)  
FROM table_name  
GROUP BY column_name  
HAVING aggregate_function(column_name) operator value

## Use BETWEEN to Filter Results
The following examples use the [Item Sales][1] and [Customers][2] sample databases.

> Note: The BETWEEN operator *is* inclusive.

**Using the BETWEEN operator with Numbers:**

    SELECT * From ItemSales
    WHERE Quantity BETWEEN 10 AND 17

This query will return all `ItemSales` records that have a quantity that is greater or equal to 10 and less than or equal to 17. The results will look like: 

Id | SaleDate      | ItemId  | Quantity | Price  
---| --------------| --------| ---------| ------
 1 | 2013-07-01    | 100     | 10       | 34.5   
 4 | 2013-07-23    | 100     | 15       | 34.5 
 5 | 2013-07-24    | 145     | 10       | 34.5   

---
**Using the BETWEEN operator with Date Values:**  

    SELECT * From ItemSales
    WHERE SaleDate BETWEEN '2013-07-11' AND '2013-05-24'

This query will return all `ItemSales` records with a `SaleDate` that is greater than or equal to July 11, 2013 and less than or equal to May 24, 2013.

Id | SaleDate      | ItemId  | Quantity | Price  
---| --------------| --------| ---------| ------
 3 | 2013-07-11    | 100     | 20       | 34.5 
 4 | 2013-07-23    | 100     | 15       | 34.5 
 5 | 2013-07-24    | 145     | 10       | 34.5

> When comparing datetime values instead of dates, you may need to convert the datetime values into a date values, or add or subtract 24 hours to get the correct results.

---

**Using the BETWEEN operator with Text Values:**  

    SELECT Id, FName, LName FROM Customers
    WHERE LName BETWEEN 'D' AND 'L';

Live example: [SQL fiddle][3]

This query will return all customers whose name alphabetically falls between the letters 'D' and 'L'. In this case, Customer #1 and #3 will be returned. Customer #2, whose name begins with a 'M' will not be included.

Id | FName    | LName
---| ---------| -----
1  | William  | Jones
3  | Richard  | Davis

  [1]:https://www.wikiod.com/sql/example-databases-and-tables
  [2]:https://www.wikiod.com/sql/example-databases-and-tables
  [3]: http://sqlfiddle.com/#!9/76b9b/2


## Use HAVING with Aggregate Functions
Unlike the `WHERE` clause, `HAVING` can be used with aggregate functions.

>An aggregate function is a function where the values of multiple rows are grouped together as input on certain criteria to form a single value of more significant meaning or measurement ([Wikipedia][1]).

Common aggregate functions include `COUNT()`, `SUM()`, `MIN()`, and `MAX()`.

 ---
This example uses the [Car Table](https://www.wikiod.com/sql/example-databases-and-tables) from the Example Databases.

    SELECT CustomerId, COUNT(Id) AS [Number of Cars]
    FROM Cars
    GROUP BY CustomerId
    HAVING COUNT(Id) > 1

This query will return the `CustomerId` and `Number of Cars` count of any customer who has more than one car. In this case, the only customer who has more than one car is Customer #1.

The results will look like:

| CustomerId | Number of Cars |
| ---------- | -------------- |
| 1          | 2              |


  [1]: https://en.wikipedia.org/wiki/Aggregate_function

## WHERE clause with NULL/NOT NULL values
    SELECT *
    FROM Employees
    WHERE ManagerId IS NULL

This statement will return all [Employee][1] records where the value of the `ManagerId` column is `NULL`.

The result will be:

    Id    FName    LName    PhoneNumber    ManagerId    DepartmentId
    1     James    Smith    1234567890     NULL         1

----

    SELECT *
    FROM Employees
    WHERE ManagerId IS NOT NULL

This statement will return all [Employee][1] records where the value of the `ManagerId` is *not* `NULL`.

The result will be:

    Id    FName       LName     PhoneNumber    ManagerId    DepartmentId
    2     John        Johnson   2468101214     1            1
    3     Michael     Williams  1357911131     1            2
    4     Johnathon   Smith     1212121212     2            1

---

**Note:** The same query will not return results if you change the WHERE clause to `WHERE ManagerId = NULL` or `WHERE ManagerId <> NULL`.

  [1]: https://www.wikiod.com/sql/example-databases-and-tables#Auto Shop Database

## The WHERE clause only returns rows that match its criteria
Steam has a games under $10 section of their store page. Somewhere deep in the heart of their systems, there's probably a query that looks something like:

    SELECT * 
    FROM Items 
    WHERE Price < 10



## Equality


## AND and OR


## Use IN to return rows with a value contained in a list
This example uses the [Car Table](https://www.wikiod.com/sql/example-databases-and-tables) from the Example Databases.

    SELECT *
    FROM Cars
    WHERE TotalCost IN (100, 200, 300)

This query will return Car #2 which costs 200 and Car #3 which costs 100. Note that this is equivalent to using multiple clauses with `OR`, e.g.: 

    SELECT *
    FROM Cars
    WHERE TotalCost = 100 OR TotalCost = 200 OR TotalCost = 300

## Use LIKE to find matching strings and substrings
See [full documentation on LIKE operator](https://www.wikiod.com/sql/like-operator).

This example uses the [Employees Table](https://www.wikiod.com/sql/example-databases-and-tables#Auto Shop Database) from the Example Databases.

    SELECT *
    FROM Employees
    WHERE FName LIKE 'John'

This query will only return Employee #1 whose first name matches 'John' exactly.

    SELECT *
    FROM Employees
    WHERE FName like 'John%'

Adding `%` allows you to search for a substring:

 - `John%` - will return any Employee whose name begins with 'John', followed by any amount of characters
 - `%John` - will return any Employee whose name ends with 'John', proceeded by any amount of characters
 - `%John%` - will return any Employee whose name contains 'John' anywhere within the value

In this case, the query will return Employee #2 whose name is 'John' as well as Employee #4 whose name is 'Johnathon'.


## Use HAVING to check for multiple conditions in a group
 Orders Table
 
 | CustomerId | ProductId | Quantity | Price |
 | ------ | ------ |------ |-------|
 | 1 | 2 | 5 | 100 |
 | 1 | 3 | 2 | 200 |
 | 1 | 4 | 1 | 500 |
 | 2 | 1 | 4 | 50 |
 | 3 | 5 | 6 | 700 |
 
To check for customers who have ordered both - ProductID 2 and 3, HAVING can be used
 

     select customerId
     from orders
     where productID in (2,3)
     group by customerId
     having count(distinct productID) = 2

 
 Return value:
 
 | customerId |
 | ------ |
 | 1 |

The query selects only records with the productIDs in questions and with the HAVING clause checks for groups having 2 productIds and not just one.

Another possibility would be

     select customerId
     from orders
     group by customerId
     having sum(case when productID = 2 then 1 else 0 end) > 0
        and sum(case when productID = 3 then 1 else 0 end) > 0
  
 This query selects only groups having at least one record with productID 2 and at least one with productID 3.

## Where EXISTS
Will select records in `TableName` that have records matching in `TableName1`.

    SELECT * FROM TableName t WHERE EXISTS (
        SELECT 1 FROM TableName1 t1 where t.Id = t1.Id)

