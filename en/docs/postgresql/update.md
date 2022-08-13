---
title: "UPDATE"
slug: "update"
draft: false
images: []
weight: 9986
type: docs
toc: true
---

## Updating a table based on joining another table
You can also update data in a table based on data from another table:

    UPDATE person 
    SET state_code = cities.state_code 
    FROM cities
    WHERE cities.city = city;

Here we are joining the `person` `city` column to the `cities` `city` column in order to get the city's state code. This is then used to update the `state_code` column in the `person` table.

## Update all rows in a table
You update all rows in table by simply providing a `column_name = value`:

    UPDATE person SET planet = 'Earth';

## Update all rows meeting a condition
    UPDATE person SET state = 'NY' WHERE city = 'New York';

## Updating multiple columns in table
You can update multiple columns in a table in the same statement, separating `col=val` pairs with commas:

    UPDATE person 
       SET country = 'USA', 
           state = 'NY' 
    WHERE city = 'New York';

