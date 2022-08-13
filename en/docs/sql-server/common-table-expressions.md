---
title: "Common Table Expressions"
slug: "common-table-expressions"
draft: false
images: []
weight: 9861
type: docs
toc: true
---

## Syntax
- WITH _cte_name_ [(_column_name_1_, _column_name_2_, ...)] AS (_cte_expression_)

It is necessary to separate a CTE from the previous statement with a semi-colon (`;`) character.

i.e. `;WITH CommonTableName (...) SELECT ... FROM CommonTableName ...`

A CTE's scope is a single batch, and only downstream of its definition.  A batch may contain multiple CTEs, and a CTE may reference another CTE defined earlier in the batch, but a CTE may not reference another CTE that is defined later in the batch.

## Generate a table of dates using CTE
    DECLARE @startdate CHAR(8), @numberDays TINYINT

    SET @startdate = '20160101'
    SET @numberDays = 10;

    WITH CTE_DatesTable
    AS
    (
      SELECT CAST(@startdate as date) AS [date]
      UNION ALL
      SELECT DATEADD(dd, 1, [date])
      FROM CTE_DatesTable
      WHERE DATEADD(dd, 1, [date]) <= DateAdd(DAY, @numberDays-1, @startdate)
    )

    SELECT [date] FROM CTE_DatesTable

    OPTION (MAXRECURSION 0)

This example returns a single-column table of dates, starting with the date specified in the @startdate variable, and returning the next @numberDays worth of dates. 

## Employee Hierarchy
# Table Setup

<!-- language: lang-sql -->
```
CREATE TABLE dbo.Employees
(
    EmployeeID INT NOT NULL PRIMARY KEY,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    ManagerID INT NULL
)

GO

INSERT INTO Employees VALUES (101, 'Ken', 'Sánchez', NULL)
INSERT INTO Employees VALUES (102, 'Keith', 'Hall', 101)
INSERT INTO Employees VALUES (103, 'Fred', 'Bloggs', 101)
INSERT INTO Employees VALUES (104, 'Joseph', 'Walker', 102)
INSERT INTO Employees VALUES (105, 'Žydrė', 'Klybė', 101)
INSERT INTO Employees VALUES (106, 'Sam', 'Jackson', 105)
INSERT INTO Employees VALUES (107, 'Peter', 'Miller', 103)
INSERT INTO Employees VALUES (108, 'Chloe', 'Samuels', 105)
INSERT INTO Employees VALUES (109, 'George', 'Weasley', 105)
INSERT INTO Employees VALUES (110, 'Michael', 'Kensington', 106)
```

# Common Table Expression

```
;WITH cteReports (EmpID, FirstName, LastName, SupervisorID, EmpLevel) AS
(
    SELECT EmployeeID, FirstName, LastName, ManagerID, 1
    FROM Employees
    WHERE ManagerID IS NULL

    UNION ALL

    SELECT e.EmployeeID, e.FirstName, e.LastName, e.ManagerID, r.EmpLevel + 1
    FROM Employees        AS e
    INNER JOIN cteReports AS r ON e.ManagerID = r.EmpID
)

SELECT
    FirstName + ' ' + LastName AS FullName,
    EmpLevel,
    (SELECT FirstName + ' ' + LastName FROM Employees WHERE EmployeeID = cteReports.SupervisorID) AS ManagerName
FROM cteReports
ORDER BY EmpLevel, SupervisorID
```

# Output:

> <table>
<tr><th>FullName</th><th>EmpLevel</th><th>ManagerName</th></tr>
<tr><td>Ken Sánchez</td><td>1</td><td><i>null</i></td></tr>
<tr><td>Keith Hall</td><td>2</td><td>Ken Sánchez</td></tr>
<tr><td>Fred Bloggs</td><td>2</td><td>Ken Sánchez</td></tr>
<tr><td>Žydre Klybe</td><td>2</td><td>Ken Sánchez</td></tr>
<tr><td>Joseph Walker</td><td>3</td><td>Keith Hall</td></tr>
<tr><td>Peter Miller</td><td>3</td><td>Fred Bloggs</td></tr>
<tr><td>Sam Jackson</td><td>3</td><td>Žydre Klybe</td></tr>
<tr><td>Chloe Samuels</td><td>3</td><td>Žydre Klybe</td></tr>
<tr><td>George Weasley</td><td>3</td><td>Žydre Klybe</td></tr>
<tr><td>Michael Kensington</td><td>4</td><td>Sam Jackson</td></tr>
</table>

## Delete duplicate rows using CTE
Employees table :

    |  ID  | FirstName | LastName | Gender | Salary |
    +------+-----------+----------+--------+--------+
    |  1   | Mark      | Hastings | Male   | 60000  |
    |  1   | Mark      | Hastings | Male   | 60000  |
    |  2   | Mary      | Lambeth  | Female | 30000  |
    |  2   | Mary      | Lambeth  | Female | 30000  |
    |  3   | Ben       | Hoskins  | Male   | 70000  |
    |  3   | Ben       | Hoskins  | Male   | 70000  |
    |  3   | Ben       | Hoskins  | Male   | 70000  |
    +------+-----------+----------+--------+--------+

CTE (Common Table Expression) :

    WITH EmployeesCTE AS
    (
       SELECT *, ROW_NUMBER()OVER(PARTITION BY ID ORDER BY ID) AS RowNumber
       FROM Employees
    )
    DELETE FROM EmployeesCTE WHERE RowNumber > 1

Execution result :

    |  ID  | FirstName | LastName | Gender | Salary |
    +------+-----------+----------+--------+--------+
    |  1   | Mark      | Hastings | Male   | 60000  |
    |  2   | Mary      | Lambeth  | Female | 30000  |
    |  3   | Ben       | Hoskins  | Male   | 70000  |
    +------+-----------+----------+--------+--------+



## Recursive CTE
This example shows how to get every year from this year to 2011 (2012 - 1).

    WITH yearsAgo
    (
        myYear
    )
    AS
    (
         -- Base Case: This is where the recursion starts
         SELECT DATEPART(year, GETDATE()) AS myYear

         UNION ALL  -- This MUST be UNION ALL (cannot be UNION)

         -- Recursive Section: This is what we're doing with the recursive call
         SELECT yearsAgo.myYear - 1
         FROM yearsAgo
         WHERE yearsAgo.myYear >= 2012
    )
         SELECT myYear FROM yearsAgo;  -- A single SELECT, INSERT, UPDATE, or DELETE

| myYear| 
| ------ | 
| 2016 |
| 2015 |
| 2014 |
| 2013 |
| 2012 |
| 2011 |



You can control the recursion (think stack overflow in code) with MAXRECURSION as a query option that will limit the number of recursive calls.

    WITH yearsAgo
    (
        myYear
    )
    AS
    (
         -- Base Case
         SELECT DATEPART(year , GETDATE()) AS myYear
         UNION ALL
         -- Recursive Section
         SELECT yearsAgo.myYear - 1
         FROM yearsAgo
         WHERE yearsAgo.myYear >= 2002
    )
         SELECT * FROM yearsAgo
         OPTION (MAXRECURSION 10);

> Msg 530, Level 16, State 1, Line 2The statement terminated. The
> maximum recursion 10 has been exhausted before statement completion.

## Find nth highest salary using CTE
Employees table :

    |  ID  | FirstName | LastName | Gender | Salary |
    +------+-----------+----------+--------+--------+
    | 1    | Jahangir  | Alam     | Male   | 70000  |
    | 2    | Arifur    | Rahman   | Male   | 60000  |
    | 3    | Oli       | Ahammed  | Male   | 45000  |
    | 4    | Sima      | Sultana  | Female | 70000  |
    | 5    | Sudeepta  | Roy      | Male   | 80000  |
    +------+-----------+----------+--------+--------+
CTE (Common Table Expression) :

     WITH RESULT AS
    (
        SELECT SALARY,
               DENSE_RANK() OVER (ORDER BY SALARY DESC) AS DENSERANK
        FROM EMPLOYEES
    )
    SELECT TOP 1 SALARY
    FROM RESULT
    WHERE DENSERANK = 1

To find 2nd highest salary simply replace N with 2. Similarly, to find 3rd highest salary, simply replace N with 3.

## CTE with multiple AS statements
    ;WITH cte_query_1
    AS
    (
        SELECT *
        FROM database.table1
    ), 
    cte_query_2 
    AS
    (
        SELECT *
        FROM database.table2
    )
    SELECT *
    FROM cte_query_1
    WHERE cte_query_one.fk IN
    (
        SELECT PK
        FROM cte_query_2
    )

With common table expressions, it is possible to create multiple queries using comma-separated AS statements. A query can then reference any or all of those queries in many different ways, even joining them.

