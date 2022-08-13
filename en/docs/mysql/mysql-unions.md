---
title: "MySQL Unions"
slug: "mysql-unions"
draft: false
images: []
weight: 9980
type: docs
toc: true
---

## Syntax
- SELECT column_name(s) FROM table1 UNION SELECT column_name(s) FROM table2;
- SELECT column_name(s) FROM table1
UNION ALL
SELECT column_name(s) FROM table2;
- SELECT column_name(s) FROM table1
WHERE col_name="XYZ"
UNION ALL
SELECT column_name(s) FROM table2
WHERE col_name="XYZ";

`UNION DISTINCT` is the same as `UNION`; it is slower than `UNION ALL` because of a de-duplicating pass.  A good practice is to always spell out `DISTINCT` or `ALL`, thereby signaling that you thought about which to do.

## Union operator
The UNION operator is used to combine the result-set (*only distinct values*) of two or more SELECT statements.

**Query:** (To selects all the different cities (*only distinct values*) from the "Customers" and the "Suppliers" tables)

    SELECT City FROM Customers
    UNION
    SELECT City FROM Suppliers
    ORDER BY City;

**Result:**

    Number of Records: 10

    City
    ------
    Aachen
    Albuquerque
    Anchorage
    Annecy
    Barcelona
    Barquisimeto
    Bend
    Bergamo
    Berlin
    Bern



## Union ALL
UNION ALL to select all (duplicate values also) cities from the "Customers" and "Suppliers" tables.

Query:

    SELECT City FROM Customers
    UNION ALL
    SELECT City FROM Suppliers
    ORDER BY City;

Result:

    Number of Records: 12

    City
    -------
    Aachen
    Albuquerque
    Anchorage
    Ann Arbor
    Annecy
    Barcelona
    Barquisimeto
    Bend
    Bergamo
    Berlin
    Berlin
    Bern



## UNION ALL With WHERE
UNION ALL to select all(duplicate values also) German cities from the "Customers" and "Suppliers" tables.
Here `Country="Germany"` is to be specified in the where clause.

**Query:**

    SELECT City, Country FROM Customers
    WHERE Country='Germany'
    UNION ALL
    SELECT City, Country FROM Suppliers
    WHERE Country='Germany'
    ORDER BY City;

Result:

    Number of Records: 14
    
<table><tbody><tr><th>City</th><th>Country</th></tr><tr><td>Aachen</td><td>Germany</td></tr><tr><td>Berlin</td><td>Germany</td></tr><tr><td>Berlin</td><td>Germany</td></tr><tr><td>Brandenburg</td><td>Germany</td></tr><tr><td>Cunewalde</td><td>Germany</td></tr><tr><td>Cuxhaven</td><td>Germany</td></tr><tr><td>Frankfurt</td><td>Germany</td></tr><tr><td>Frankfurt a.M. </td><td>Germany</td></tr><tr><td>Köln</td><td>Germany</td></tr><tr><td>Leipzig</td><td>Germany</td></tr><tr><td>Mannheim</td><td>Germany</td></tr><tr><td>München</td><td>Germany</td></tr><tr><td>Münster</td><td>Germany</td></tr><tr><td>Stuttgart</td><td>Germany</td></tr></tbody></table>



