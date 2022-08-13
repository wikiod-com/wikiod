---
title: "Using Joins in SAS"
slug: "using-joins-in-sas"
draft: false
images: []
weight: 9994
type: docs
toc: true
---

Each database is a collection of different tables and each table contains different data in an organized way. While working with data, most of the times information we need is scattered in more than one table. We need joins/merge to get the desired output.

In SAS we use joins while working with `Proc SQL` and use merge while working with `Data step`. We will now talk only about joins inside `Proc SQL`.

## Parameters
| Type of join | Output |
| ------ | ------ |
| Proc Sql | SQL procedure inside SAS   |
| Create Table | Creates a SAS dataset   |
| Select | Selects required variables from respective datasets  |
| Where | Specifies particular condition |
| Quit | End the procedure |

As mentioned in the introduction, we can also use `Merge`inside a `data step` which will be discussed under a separate topic. Joins play a very important role to blend and unify data according to the requirement.

## Vertical Joining
Vertical join appends dataset B to dataset A providing both of them have similar variables. For example, we have sales for the month of Jan'17 in dataset A and sales for Feb'17 in dataset B. To create a dataset C that has sales of both Jan and Feb we use Vertical Join.

    PROC SQL;
    CREATE TABLE C AS
    SELECT *
    FROM A
    UNION 
    SELECT *
    FROM B;
    QUIT;

Now dataset C has observations from both A and B and is appended vertically.

## Inner Join
Inner join creates a dataset that contains records that have matching values from both the tables. For example, we have a dataset A that contains customer information and a dataset B that contains credit card details. To get the credit card details of customers in dataset A, let us create dataset C

    PROC SQL;
    CREATE TABLE C AS
    SELECT A.*, B.CC_NUM
    FROM CUSTOMER A, CC_DETAILS B
    WHERE A.CUSTOMERID=B.CUSTOMERID
    QUIT;

Dataset C will have only matching observations from both the datasets.

## Left Join
Left join returns all the observations in the left data set regardless of their key values but only observations with matching key values from the right data set. Considering the same example as above,

    PROC SQL;
    CREATE TABLE C AS
    SELECT A.*, B.CC_NUMBER, B.START_DATE
    FROM CUSTOMER A LEFT JOIN CC_DETAILS B
    ON A.CUSTOMERID=B.CUSTOMERID
    QUIT;

Dataset C contains all the values from the left table, plus matched values from the right table or missing values in the case of no match.

## Right join
Like left join, right join selects all the observations from the right dataset and the matched records from the left table.

    PROC SQL;
    CREATE TABLE C AS
    SELECT A.*, B.CC_NUMBER, B.START_DATE
    FROM CUSTOMER A RIGHT JOIN CC_DETAILS B
    ON A.CUSTOMERID=B.CUSTOMERID
    QUIT;

Dataset C contains all the values from the right table, plus matched values from the left table or missing values in the case of no match.

## Full Join
Full join selects all the observations from both data sets but there are missing values where the key value in each observation is found in one table only.

    PROC SQL;
    CREATE TABLE C AS
    SELECT A.*, B.CC_NUMBER, B.START_DATE
    FROM CUSTOMER A FULL JOIN CC_DETAILS B 
    ON A.CUSTOMERID=B.CUSTOMERID
    QUIT;

Dataset C will contain all records from both the tables and fill in `.` for missing matches on either side.

